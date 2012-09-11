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
-module(verx_client_tcp).
-behaviour(gen_server).

-include_lib("kernel/include/inet.hrl").
-include("verx.hrl").

-export([
    call/2, call/3,

    recv/1, recv/2,
    recvall/1, recvall/2,

    send/2,
    finish/1,

    getfd/1
    ]).
-export([start_link/0, start_link/1]).
-export([start/0, start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(verx_buf, {
        len = 0,
        buflen = 0,
        buf = []
        }).

-record(state, {
        pid,
        s,                  % socket
        proc,               % last called procedure
        serial = 0,         % serial number
        buf = #verx_buf{}
        }).


%%-------------------------------------------------------------------------
%%% API
%%-------------------------------------------------------------------------
call(Ref, Proc) ->
    call(Ref, Proc, []).
call(Ref, Proc, Arg) when is_pid(Ref), is_atom(Proc), is_list(Arg) ->
    ok = gen_server:call(Ref, {call, Proc, Arg}, infinity),

    receive
        {verx, Ref, Reply} ->
            verx_rpc:status(Reply)
    after
        5000 ->
            {error, eagain}
    end.

recv(Ref) ->
    recv(Ref, 5000).
recv(Ref, Timeout) ->
    recv(Ref, Timeout, []).
recv(Ref, Timeout, Acc) ->
    receive
        {verx, Ref, {#remote_message_header{
                            type = <<?REMOTE_STREAM:32>>,
                            status = <<?REMOTE_OK:32>>}, []}} ->
            {ok, lists:reverse(Acc)};
        {verx, Ref, {#remote_message_header{
                        type = <<?REMOTE_STREAM:32>>,
                        status = <<?REMOTE_CONTINUE:32>>}, Payload}} ->
            recv(Ref, Timeout, [Payload|Acc]);
        {verx, Ref, Buf} ->
            error_logger:info_report([{got, Buf}])
    after
        Timeout ->
            {error, eagain}
    end.

recvall(Ref) ->
    recvall(Ref, 2000).
recvall(Ref, Timeout) ->
    recvall(Ref, Timeout, []).
recvall(Ref, Timeout, Acc) ->
    receive
        {verx, Ref, {#remote_message_header{
                            type = <<?REMOTE_STREAM:32>>,
                            status = <<?REMOTE_OK:32>>}, []}} ->
            {ok, lists:reverse(Acc)};
        % XXX A stream indicates finish by setting the status to
        % XXX REMOTE_OK. For screenshots, an empty body is returned with the
        % XXX status set to 'continue'.
        {verx, Ref, {#remote_message_header{
                        type = <<?REMOTE_STREAM:32>>,
                        status = <<?REMOTE_CONTINUE:32>>}, <<>>}} ->
            {ok, lists:reverse(Acc)};
        {verx, Ref, {#remote_message_header{
                        type = <<?REMOTE_STREAM:32>>,
                        status = <<?REMOTE_CONTINUE:32>>}, Payload}} ->
            recvall(Ref, Timeout, [Payload|Acc]);
        {verx, Ref, Buf} ->
            error_logger:info_report([{got, Buf}])
    after
        Timeout ->
            case length(Acc) of
                0 -> {error, eagain};
                _ -> {ok, lists:reverse(Acc)}
            end
    end.

send(_Ref, []) ->
    ok;
send(Ref, [Buf|Rest]) when is_binary(Buf) ->
    ok = gen_server:call(Ref, {send, Buf}, infinity),
    send(Ref, Rest).

finish(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, finish, infinity).

getfd(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, getfd).

start() ->
    start([]).
start(Opt) when is_list(Opt) ->
    Self = self(),
    gen_server:start(?MODULE, [Self, Opt], []).

start_link() ->
    start_link([]).
start_link(Opt) when is_list(Opt) ->
    Self = self(),
    gen_server:start_link(?MODULE, [Self, Opt], []).

stop(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, stop).


%%-------------------------------------------------------------------------
%%% Callbacks
%%-------------------------------------------------------------------------
init([Pid, Opt]) ->

    Host = proplists:get_value(host, Opt, "127.0.0.1"),
    Port = proplists:get_value(port, Opt, ?LIBVIRT_TCP_PORT),

    {IP, Family} = resolv(Host),

    % Connect to the libvirt socket
    {ok, Socket} = gen_tcp:connect(IP, Port, [
                Family,
                binary,
                {active, false},
                {packet, 0}
                ]),

    {ok, #state{
            pid = Pid,
            s = Socket
            }}.


handle_call({call, Proc, Arg}, _From, #state{
                s = Socket,
                serial = Serial
                } = State) when is_list(Arg) ->
    {Header, Call} = verx_rpc:call(Proc, Arg),
    Message = verx_rpc:encode({Header#remote_message_header{
                    serial = <<Serial:32>>
                    }, Call}),
    Reply = send_rpc(Socket, Message),
    inet:setopts(Socket, [{active, once}]),
    {reply, Reply, State#state{proc = Proc, serial = Serial+1}};

handle_call({send, Buf}, _From, #state{
                s = Socket,
                proc = Proc,
                serial = Serial
                } = State) when is_binary(Buf) ->
    Message = verx_rpc:encode({#remote_message_header{
            proc = remote_protocol_xdr:enc_remote_procedure(Proc),
            type = <<?REMOTE_STREAM:32>>,
            serial = <<Serial:32>>,
            status = <<?REMOTE_CONTINUE:32>>
            }, Buf}),
    Reply = send_rpc(Socket, Message),
    inet:setopts(Socket, [{active, once}]),
    {reply, Reply, State};

handle_call(finish, _From, #state{
                proc = Proc,
                s = Socket,
                serial = Serial
                } = State) ->
    Header = verx_rpc:header(#remote_message_header{
            proc = remote_protocol_xdr:enc_remote_procedure(Proc),
            type = <<?REMOTE_STREAM:32>>,
            serial = <<Serial:32>>,
            status = <<?REMOTE_OK:32>>
            }),
    Reply = send_rpc(Socket, Header),
    inet:setopts(Socket, [{active, once}]),
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% Reply from libvirtd
%%

handle_info({tcp, Socket, Data},
            #state{s = Socket,
                   pid = Pid,
                   serial = Serial,
                   buf = Buf} = State) ->
    inet:setopts(Socket, [{active, once}]),
    {Msgs, Rest} = verx_client:stream(Data, Buf),
    [ reply_to_caller(Pid, Serial, Msg) || Msg <- Msgs ],
    {noreply, State#state{buf = Rest}};

handle_info({tcp_closed, Socket}, #state{s = Socket} = State) ->
    {stop, {shutdown, tcp_closed}, State};

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-------------------------------------------------------------------------
%%% Utility functions
%%-------------------------------------------------------------------------


%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------
resolv(Host) ->
    resolv(Host, inet6).
resolv(Host, Family) ->
    case inet:gethostbyname(Host, Family) of
        {error, nxdomain} ->
            resolv(Host, inet);
        {ok, #hostent{h_addr_list = [IPaddr|_IPaddrs]}} ->
            {IPaddr, Family};
        Error ->
            Error
    end.

send_rpc(Socket, Buf) ->
    Len = ?REMOTE_MESSAGE_HEADER_XDR_LEN + byte_size(Buf),
    gen_tcp:send(Socket, <<?UINT32(Len), Buf/binary>>).

reply_to_caller(Pid, Serial0, Data) ->
    Reply = verx_rpc:decode(Data),
    {#remote_message_header{type = <<?UINT32(Type)>>,
                            proc = Proc,
                            serial = <<?UINT32(RSerial)>>}, _} = Reply,

    % serial increments on every call
    Serial = Serial0 - 1,
    case {Type, RSerial} of
        {N, Serial} when N =:= ?REMOTE_REPLY; N =:= ?REMOTE_STREAM ->
            {_,D} = Reply,
            Pid ! {verx, self(), Reply};
        {?REMOTE_MESSAGE, 0} ->
            Pid ! {verx, self(), Reply};
        _ ->
            Pid ! {verx, self(),
                   {out_of_sync,
                    element(1, remote_protocol_xdr:dec_remote_procedure(Proc, 0)),
                    Type,
                    RSerial,
                    Serial}
                  }
    end,
    ok.
