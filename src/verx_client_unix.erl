%% Copyright (c) 2012, Michael Santos <michael.santos@gmail.com>
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
-module(verx_client_unix).
-behaviour(gen_server).

-include("verx.hrl").
-include_lib("procket/include/procket.hrl").

-export([
    call/2, call/3,
    cast/2, cast/3, cast/4,

    reply/2, reply/3,

    recv/1, recv/2, recv/3,
    recvall/1, recvall/2,

    send/2,
    finish/1,

    getfd/1
    ]).
-export([read_packet/1, read_packet/2]).
-export([start_link/0, start_link/1]).
-export([start/0, start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
    path,       % Unix socket path
    s,          % Unix socket fd
    proc,       % last called procedure
    serial = -1 % serial number
    }).


%%-------------------------------------------------------------------------
%%% API
%%-------------------------------------------------------------------------
call(Ref, Proc) ->
    call(Ref, Proc, []).
call(Ref, Proc, Arg) when is_pid(Ref), is_atom(Proc), is_list(Arg) ->
    case cast(Ref, Proc, Arg) of
        {ok, Serial} -> reply(Ref, Serial);
        Error -> Error
    end.

cast(Ref, Proc) ->
    cast(Ref, Proc, [], infinity).
cast(Ref, Proc, Arg) ->
    cast(Ref, Proc, Arg, infinity).
cast(Ref, Proc, Arg, Timeout) ->
    {ok, Serial} = gen_server:call(Ref, {call, Proc, Arg}, Timeout),
    #state{s = Socket} = getstate(Ref),
    {Header, Call} = verx_rpc:call(Proc, Arg),
    Message = verx_rpc:encode({Header#remote_message_header{serial = <<Serial:32>>}, Call}),
    Len = ?REMOTE_MESSAGE_HEADER_XDR_LEN + byte_size(Message),
    case procket:write(Socket, [<<?UINT32(Len)>>, Message]) of
        ok ->
            {ok, Serial};
        {error, _} = Error ->
            Error
    end.

reply(Ref, Serial) ->
    reply(Ref, Serial, infinity).
reply(Ref, Serial, infinity) ->
    reply(Ref, Serial, 60 * 60 * 1000); % XXX
reply(Ref, Serial, Timeout) ->
    receive
        {verx, Ref, {#remote_message_header{serial = <<Serial:32>>, type = <<?REMOTE_REPLY:32>>}, _} = Reply} ->
            verx_rpc:status(Reply)
    after
        0 ->
            case read(Ref, Timeout) of
                ok ->
                    reply(Ref, Serial, Timeout);
                {error, _} = Error ->
                    Error
            end
    end.

recv(Ref) ->
    recv(Ref, 5000).
recv(Ref, Timeout) ->
    #state{serial = Serial} = getstate(Ref),
    recv(Ref, Serial, Timeout).
recv(Ref, Serial, Timeout) ->
    receive
        {verx, Ref, {#remote_message_header{
                            serial = <<Serial:32>>,
                            type = <<?REMOTE_STREAM:32>>,
                            status = <<?REMOTE_OK:32>>}, []}} ->
            ok;
        {verx, Ref, {#remote_message_header{
                            serial = <<Serial:32>>,
                            type = <<?REMOTE_STREAM:32>>,
                            status = <<?REMOTE_CONTINUE:32>>}, Payload}} ->
            {ok, Payload}
    after
        0 ->
            case read(Ref, Timeout) of
                ok ->
                    recv(Ref, Timeout);
                {error, _} = Error ->
                    Error
            end
    end.

recvall(Ref) ->
    recvall(Ref, 5000).
recvall(Ref, Timeout) ->
    #state{serial = Serial} = getstate(Ref),
    recvall(Ref, Serial, Timeout, []).
recvall(Ref, Serial, Timeout, Acc) ->
    receive
        {verx, Ref, {#remote_message_header{
                            type = <<?REMOTE_STREAM:32>>,
                            status = <<?REMOTE_OK:32>>,
                            serial = <<Serial:32>>}, []}} ->
            {ok, lists:reverse(Acc)};
        % XXX A stream indicates finish by setting the status to
        % XXX REMOTE_OK. For screenshots, an empty body is returned with the
        % XXX status set to 'continue'.
        {verx, Ref, {#remote_message_header{
                            type = <<?REMOTE_STREAM:32>>,
                            status = <<?REMOTE_CONTINUE:32>>,
                            serial = <<Serial:32>>}, <<>>}} ->
            {ok, lists:reverse(Acc)};
        {verx, Ref, {#remote_message_header{
                            type = <<?REMOTE_STREAM:32>>,
                            status = <<?REMOTE_CONTINUE:32>>,
                            serial = <<Serial:32>>}, Payload}} ->
            recvall(Ref, Serial, Timeout, [Payload|Acc])
    after
        0 ->
            case read(Ref, Timeout) of
                ok ->
                    recvall(Ref, Serial, Timeout, Acc);
                {error, eagain} ->
                    {ok, lists:reverse(Acc)};
                {error, _} = Error ->
                    Error
            end
    end.

send(Ref, Buf) when is_list(Buf) ->
    #state{s = FD, proc = Proc, serial = Serial} = getstate(Ref),
    send(FD, Proc, Serial, Buf).

send(_FD, _Proc, _Serial, []) ->
    ok;
send(FD, Proc, Serial, [Buf|Rest]) when is_binary(Buf) ->
    Header = verx_rpc:header(#remote_message_header{
            proc = remote_protocol_xdr:enc_remote_procedure(Proc),
            type = <<?REMOTE_STREAM:32>>,
            serial = <<Serial:32>>,
            status = <<?REMOTE_CONTINUE:32>>
            }),
    Len = ?REMOTE_MESSAGE_HEADER_XDR_LEN + byte_size(Header) + byte_size(Buf),
    ok = procket:write(FD, [<<Len:32>>, Header, Buf]),
    send(FD, Proc, Serial, Rest).

finish(Ref) when is_pid(Ref) ->
    #state{s = FD, proc = Proc, serial = Serial} = getstate(Ref),
    Header = verx_rpc:header(#remote_message_header{
            proc = remote_protocol_xdr:enc_remote_procedure(Proc),
            type = <<?REMOTE_STREAM:32>>,
            serial = <<Serial:32>>,
            status = <<?REMOTE_OK:32>>
            }),
    Len = ?REMOTE_MESSAGE_HEADER_XDR_LEN + byte_size(Header),
    procket:write(FD, <<Len:32, Header/binary>>).

getfd(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, getfd).

start() ->
    start([]).
start(Path) ->
    gen_server:start(?MODULE, [Path], []).

start_link() ->
    start_link([]).
start_link(Opt) when is_list(Opt) ->
    gen_server:start_link(?MODULE, [Opt], []).

stop(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, stop).


%%-------------------------------------------------------------------------
%%% Callbacks
%%-------------------------------------------------------------------------
init([Opt]) ->

    Path = maybe_binary(proplists:get_value(path, Opt, ?LIBVIRT_SOCK_PATH)),

    % Connect to the libvirt Unix socket
    {ok, Socket} = procket:socket(?PF_LOCAL, ?SOCK_STREAM, 0),

    PathMax = procket:unix_path_max(),

    Sun = <<?PF_LOCAL:16/native,            % sun_family
            Path/binary,                    % socket path
            0:((PathMax-byte_size(Path))*8)>>,

    ok = procket:connect(Socket, Sun),

    {ok, #state{
            s = Socket,
            path = Sun
            }}.


handle_call({call, Proc, _Message}, _From, #state{serial = Serial} = State) ->
    Serial1 = Serial + 1,
    {reply, {ok, Serial1}, State#state{proc = Proc, serial = Serial1}};

handle_call(getfd, _From, #state{s = Socket} = State) ->
    {reply, Socket, State};

handle_call(getstate, _From, State) ->
    {reply, State, State};

handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.

terminate(_Reason, #state{s = S}) ->
    procket:close(S),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-------------------------------------------------------------------------
%%% Utility functions
%%-------------------------------------------------------------------------
read(Ref, Timeout) ->
    #state{s = Socket} = getstate(Ref),
    case read_packet(Socket, Timeout) of
        {ok, Buf} ->
            Reply = verx_rpc:decode(Buf),
            self() ! {verx, Ref, Reply},
            ok;
        {error, _} = Error ->
            Error
    end.

read_packet(Socket) ->
    read_packet(Socket, 5000).
read_packet(Socket, Timeout) when is_integer(Socket) ->
    case read_all(Socket, ?REMOTE_MESSAGE_HEADER_XDR_LEN, Timeout) of
        {ok, <<?UINT32(Len)>>} ->
            read_all(Socket, Len - ?REMOTE_MESSAGE_HEADER_XDR_LEN, Timeout);
        Error ->
            Error
    end.


%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------
maybe_binary(N) when is_binary(N) ->
    N;
maybe_binary(N) when is_list(N) ->
    list_to_binary(N).

read_all(Socket, Len, Timeout) ->
    read_all(Socket, Len, Timeout, []).
read_all(Socket, Len, Timeout, Acc) ->
    case procket:read(Socket, Len) of
        {ok, Buf} when Len - byte_size(Buf) =:= 0 ->
            {ok, list_to_binary(lists:reverse([Buf|Acc]))};
        {ok, Buf} ->
            read_all(Socket, Len - byte_size(Buf), Timeout, [Buf|Acc]);
        {error, eagain} when Timeout =< 0 ->
            {error, eagain};
        {error, eagain} ->
            timer:sleep(10),
            read_all(Socket, Len, Timeout - 10, Acc);
        Error ->
            Error
    end.

getstate(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, getstate).
