%% Copyright (c) 2011-2012, Michael Santos <michael.santos@gmail.com>
%% All rights reserved.
%% 
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%% 
%% Redistributions of source code must retain the above copyright
%% notice, this list of conditions and the following disclaimer.
%% 
%% Redistributions in binary form must reproduce the above copyright
%% notice, this list of conditions and the following disclaimer in the
%% documentation and/or other materials provided with the distribution.
%% 
%% Neither the name of the author nor the names of its contributors
%% may be used to endorse or promote products derived from this software
%% without specific prior written permission.
%% 
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.
-module(verx_rpc).
-include_lib("procket/include/procket.hrl").
-include("verx.hrl").

-export([
    open/0, open/1,
    close/1,

    call/2, call/3,

    encode/1,
    decode/1,

    response/2, response/3
    ]).

-define(SOCK_PATH, "/var/run/libvirt/libvirt-sock").


%%-------------------------------------------------------------------------
%%% API
%%-------------------------------------------------------------------------
open() ->
    open(?SOCK_PATH).
open(Path) ->
    {ok, Socket} = sock_open(Path),
    sock_send(Socket, message(
            #remote_message_header{},
            arg()
        )),
    {ok, Response} = sock_recv(Socket),
    {Header, Buf} = decode({message, Response}),
    case Header#remote_message_header.status of
        ok ->
            {ok, Socket};
        _ ->
            close(Socket),
            {Header, Buf}
    end.

close(Socket) ->
    sock_send(Socket, message(
            #remote_message_header{proc = proc(close)},
            arg()
        )),
    Response = sock_recv(Socket),
    sock_close(Socket),
    Response.

call(Socket, Proc) ->
    call(Socket, Proc, arg()).

call(Socket, Proc, Arg) when is_atom(Proc), is_binary(Arg) ->
    sock_send(Socket, message(
        #remote_message_header{proc = proc(Proc)},
        Arg
    )),
    {ok, Response} = sock_recv(Socket),
    decode({message, Response}).


arg() ->
    list_to_binary([
        verx_xdr:encode({string, ""}),  % Name
        verx_xdr:encode({int, 0})       % Flags
    ]).

%%-------------------------------------------------------------------------
%%% Transport
%%-------------------------------------------------------------------------
sock_open(Path) when is_list(Path) ->
    sock_open(list_to_binary(Path));
sock_open(Path) when is_binary(Path) ->
    PathMax = procket:unix_path_max(),
    sock_open1(Path, PathMax).

sock_open1(Path, PathMax) when byte_size(Path) < PathMax ->
    {ok, Socket} = procket:socket(?PF_LOCAL, ?SOCK_STREAM, 0),
    Sun = <<?PF_LOCAL:16/native,    % sun_family
        Path/binary,                % socket path
        0:((PathMax-byte_size(Path))*8)
    >>,
    ok = procket:connect(Socket, Sun),
    {ok, #socket{
            s = Socket,
            sun = Sun
        }}.

sock_close(#socket{s = Socket}) ->
    procket:close(Socket).

sock_send(#socket{s = Socket}, Buf) when is_binary(Buf) ->
    procket:sendto(Socket, Buf).

sock_recv(#socket{s = Socket} = S) ->
    case procket:recvfrom(Socket, ?REMOTE_MESSAGE_MAX) of
        {error, eagain} ->
            timer:sleep(10),
            sock_recv(S);
        {ok, _Buf} = N ->
            N
    end.


%%-------------------------------------------------------------------------
%%% RPC protocol
%%-------------------------------------------------------------------------
encode({message, {Hdr, Payload}}) ->
    message(Hdr, Payload);
encode({header, Header}) ->
    header(Header);
encode({Type, _}) ->
    {error, {format, Type}}.

decode({message, <<Len:32, Header:24/bytes, Payload/binary>> = Msg}) when Len == byte_size(Msg) ->
    Hdr = header(Header),
    {Hdr, Payload};
decode({header, Header}) ->
    header(Header);
decode({Type, _}) ->
    {error, {format, Type}}.

response(Fun, Header, Buf) ->
    response(Fun, {Header, Buf}).

response(Fun, {#remote_message_header{status = ok}, Buf}) ->
    case verx_ret:decode(Fun, Buf) of
        {error, unsupported} -> {ok, void};
        N -> {ok, N}
    end;
response(_Fun, {#remote_message_header{status = error}, Buf}) ->
    {error, verx_xdr:decode({remote_error, Buf})}.


%%-------------------------------------------------------------------------
%%% Encode/decode RPC messages
%%-------------------------------------------------------------------------
%% A remote RPC message
message(#remote_message_header{} = Hdr, Payload) ->
    message(header(Hdr), Payload);
message(Hdr, Payload) when is_binary(Hdr), is_binary(Payload) ->
    Len = 4 + 24 + byte_size(Payload),
    <<Len:32, Hdr:24/bytes, Payload/bytes>>.

%% Message header
header(#remote_message_header{
        prog = Prog,
        vers = Vers,
        proc = Proc,
        type = Type,
        serial = Serial,
        status = Status
    }) ->
    <<Prog:32, Vers:32, (proc(Proc)):32, (type(Type)):32, Serial:32, (status(Status)):32>>;
header(<<
    Prog:32, Vers:32, Proc:32,
    Type:32, Serial:32, Status:32
    >>) ->
    #remote_message_header{
        prog = Prog,
        vers = Vers,
        proc = verx_constant:proc(Proc),
        type = verx_constant:type(Type),
        serial = Serial,
        status = verx_constant:status(Status)
    };
header(_) ->
    {error, header_format}.


%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------

% only use integers for protocol operations
proc(N) when is_atom(N) -> verx_constant:proc(N);
proc(N) when is_integer(N) -> N.

type(N) when is_atom(N) -> verx_constant:type(N);
type(N) when is_integer(N) -> N.

status(N) when is_atom(N) -> verx_constant:status(N);
status(N) when is_integer(N) -> N.

