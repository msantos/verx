%% Copyright (c) 2011-2021, Michael Santos <michael.santos@gmail.com>
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
-module(verx_client).

-include_lib("kernel/include/inet.hrl").

-include("verx.hrl").

-export([start_link/0, start_link/1]).
-export([start/0, start/1, stop/1]).
-export([
    call/2, call/3,

    cast/2, cast/3, cast/4,
    reply/2, reply/3,

    send/2,
    recv/1, recv/2,
    recvall/1, recvall/2,
    finish/1,

    getserial/1
]).

-export([
    stream/1,
    resolv/1, resolv/2,
    reply_to_caller/2
]).

%%-------------------------------------------------------------------------
%%% API
%%-------------------------------------------------------------------------
start() ->
    start([]).

start(Arg) ->
    Transport = proplists:get_value(transport, Arg, verx_client_unix),
    Self = self(),
    gen_server:start(Transport, [Self, Arg], []).

start_link() ->
    start_link([]).

start_link(Arg) ->
    Transport = proplists:get_value(transport, Arg, verx_client_unix),
    Self = self(),
    gen_server:start_link(Transport, [Self, Arg], []).

stop(Ref) when is_pid(Ref) ->
    catch gen_server:call(Ref, stop),
    ok.

call(Ref, Proc) ->
    call(Ref, Proc, []).

call(Ref, Proc, Arg) ->
    case cast(Ref, Proc, Arg, infinity) of
        {ok, Serial} -> reply(Ref, Serial);
        Error -> Error
    end.

cast(Ref, Proc) ->
    cast(Ref, Proc, [], infinity).

cast(Ref, Proc, Arg) ->
    cast(Ref, Proc, Arg, infinity).

cast(Ref, Proc, Arg, Timeout) when is_pid(Ref), is_atom(Proc), is_list(Arg) ->
    gen_server:call(Ref, {call, Proc, Arg}, Timeout).

send(Ref, Buf) when is_binary(Buf) ->
    send(Ref, [Buf]);
send(_Ref, []) ->
    ok;
send(Ref, [Buf | Rest]) when is_binary(Buf) ->
    ok = gen_server:call(Ref, {send, Buf}, infinity),
    send(Ref, Rest).

reply(Ref, Serial) ->
    reply(Ref, Serial, infinity).

reply(Ref, Serial, Timeout) when is_pid(Ref), is_integer(Serial) ->
    receive
        {verx, Ref,
            {#remote_message_header{
                    serial = <<Serial:32>>,
                    type = <<?REMOTE_REPLY:32>>
                },
                _} = Reply} ->
            verx_rpc:status(Reply)
    after Timeout -> {error, eagain}
    end.

recv(Ref) ->
    recv(Ref, 5000).

recv(Ref, Timeout) ->
    Serial = getserial(Ref),
    recv(Ref, Serial, Timeout).

recv(Ref, Serial, Timeout) when is_pid(Ref), is_integer(Serial) ->
    receive
        {verx, Ref,
            {#remote_message_header{
                    serial = <<Serial:32>>,
                    type = <<?REMOTE_STREAM:32>>,
                    status = <<?REMOTE_OK:32>>
                },
                []}} ->
            ok;
        {verx, Ref,
            {#remote_message_header{
                    serial = <<Serial:32>>,
                    type = <<?REMOTE_STREAM:32>>,
                    status = <<?REMOTE_CONTINUE:32>>
                },
                Payload}} ->
            {ok, Payload}
    after Timeout -> {error, eagain}
    end.

recvall(Ref) ->
    recvall(Ref, 2000).

recvall(Ref, Timeout) ->
    recvall(Ref, Timeout, []).

recvall(Ref, Timeout, Acc) when is_pid(Ref) ->
    receive
        {verx, Ref,
            {#remote_message_header{
                    type = <<?REMOTE_STREAM:32>>,
                    status = <<?REMOTE_OK:32>>
                },
                []}} ->
            {ok, lists:reverse(Acc)};
        % XXX A stream indicates finish by setting the status to
        % XXX REMOTE_OK. For screenshots, an empty body is returned with the
        % XXX status set to 'continue'.
        {verx, Ref,
            {#remote_message_header{
                    type = <<?REMOTE_STREAM:32>>,
                    status = <<?REMOTE_CONTINUE:32>>
                },
                <<>>}} ->
            {ok, lists:reverse(Acc)};
        {verx, Ref,
            {#remote_message_header{
                    type = <<?REMOTE_STREAM:32>>,
                    status = <<?REMOTE_CONTINUE:32>>
                },
                Payload}} ->
            recvall(Ref, Timeout, [Payload | Acc])
    after Timeout -> {ok, lists:reverse(Acc)}
    end.

finish(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, finish, infinity).

getserial(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, getserial).

%%-------------------------------------------------------------------------
%%% Utility functions
%%-------------------------------------------------------------------------

% Parse a stream of bytes into remote protocol messages
stream(Data) ->
    stream(Data, []).

stream(Data, Acc) ->
    case message(Data) of
        {<<>>, Rest} ->
            {lists:reverse(Acc), Rest};
        {Bin, Rest} ->
            stream(Rest, [Bin | Acc])
    end.

% 4 byte length header, includes the size of the length header
message(<<?UINT32(Len), Data/binary>> = Bin) when Len =< byte_size(Bin) ->
    Size = Len - 4,
    <<Msg:Size/bytes, Rest/binary>> = Data,
    {Msg, Rest};
message(Data) ->
    {<<>>, Data}.

resolv(Host) ->
    resolv(Host, inet6).

resolv(Host, Family) ->
    case inet:gethostbyname(Host, Family) of
        {error, nxdomain} ->
            resolv(Host, inet);
        {ok, #hostent{h_addr_list = [IPaddr | _IPaddrs]}} ->
            {IPaddr, Family};
        Error ->
            Error
    end.

reply_to_caller(Pid, Data) ->
    Reply = verx_rpc:decode(Data),
    Pid ! {verx, self(), Reply},
    ok.
