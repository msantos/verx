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

-include_lib("procket/include/procket.hrl").
-include("verx.hrl").
-include("verx_client.hrl").

-export([
    call/2, call/3,

    cast/2, cast/3, cast/4,
    reply/2, reply/3,

    recv/1, recv/2, recv/3,
    recvall/1, recvall/2,

    send/2,
    finish/1,

    getserial/1
    ]).
-export([start_link/0, start_link/1]).
-export([start/0, start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
        pid,
        s,                  % socket
        port,               % Erlang port reference
        proc,               % last called procedure
        serial = -1,        % serial number
        buf = #verx_buf{}
        }).


%%-------------------------------------------------------------------------
%%% API
%%-------------------------------------------------------------------------
call(Ref, Proc) ->
    call(Ref, Proc, []).
call(Ref, Proc, Arg) when is_pid(Ref), is_atom(Proc), is_list(Arg) ->
    case cast(Ref, Proc, Arg, infinity) of
        {ok, Serial} -> reply(Ref, Serial);
        Error -> Error
    end.

cast(Ref, Proc) ->
    cast(Ref, Proc, [], infinity).
cast(Ref, Proc, Arg) ->
    cast(Ref, Proc, Arg, infinity).
cast(Ref, Proc, Arg, Timeout) ->
    gen_server:call(Ref, {call, Proc, Arg}, Timeout).

reply(Ref, Serial) ->
    reply(Ref, Serial, infinity).
reply(Ref, Serial, Timeout) ->
    receive
        {verx, Ref, {#remote_message_header{
                            serial = <<Serial:32>>,
                            type = <<?REMOTE_REPLY:32>>}, _} = Reply} ->
            verx_rpc:status(Reply)
    after
        Timeout ->
            {error, eagain}
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
            recvall(Ref, Timeout, [Payload|Acc])
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

getserial(Ref) when is_pid(Ref) ->
    #state{serial = Serial} = gen_server:call(Ref, getstate),
    Serial.

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
    process_flag(trap_exit, true),

    Path = maybe_binary(proplists:get_value(path, Opt, ?LIBVIRT_SOCK_PATH)),

    % Connect to the libvirt Unix socket
    {ok, Socket} = procket:socket(?PF_LOCAL, ?SOCK_STREAM, 0),

    PathMax = procket:unix_path_max(),

    Sun = <<?PF_LOCAL:16/native,            % sun_family
            Path/binary,                    % socket path
            0:((PathMax-byte_size(Path))*8)>>,

    ok = procket:connect(Socket, Sun),

    Port = erlang:open_port({fd, Socket, Socket}, [
                stream,
                binary
                ]),

    {ok, #state{
            pid = Pid,
            port = Port,
            s = Socket
            }}.


handle_call({call, Proc, Arg}, _From, #state{
                port = Port,
                serial = Serial0
                } = State) when is_list(Arg) ->
    {Header, Call} = verx_rpc:call(Proc, Arg),
    Serial = Serial0 + 1,
    Message = verx_rpc:encode({Header#remote_message_header{
                    serial = <<Serial:32>>
                    }, Call}),
    Reply = case send_rpc(Port, Message) of
        ok -> {ok, Serial};
        Error -> Error
    end,
    {reply, Reply, State#state{proc = Proc, serial = Serial}};

handle_call({send, Buf}, _From, #state{
                port = Port,
                proc = Proc,
                serial = Serial
                } = State) when is_binary(Buf) ->
    Message = verx_rpc:encode({#remote_message_header{
            proc = remote_protocol_xdr:enc_remote_procedure(Proc),
            type = <<?REMOTE_STREAM:32>>,
            serial = <<Serial:32>>,
            status = <<?REMOTE_CONTINUE:32>>
            }, Buf}),
    Reply = send_rpc(Port, Message),
    {reply, Reply, State};

handle_call(finish, _From, #state{
                proc = Proc,
                port = Port,
                serial = Serial
                } = State) ->
    Header = verx_rpc:header(#remote_message_header{
            proc = remote_protocol_xdr:enc_remote_procedure(Proc),
            type = <<?REMOTE_STREAM:32>>,
            serial = <<Serial:32>>,
            status = <<?REMOTE_OK:32>>
            }),
    Reply = send_rpc(Port, Header),
    {reply, Reply, State};

handle_call(getstate, _From, State) ->
    {reply, State, State};

handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%
%% Reply from libvirtd
%%

handle_info({Port, {data, Data}},
            #state{port = Port,
                   pid = Pid,
                   buf = Buf} = State) ->
    {Msgs, Rest} = verx_client:stream(Data, Buf),
    [ reply_to_caller(Pid, Msg) || Msg <- Msgs ],
    {noreply, State#state{buf = Rest}};

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {shutdown, Reason}, State};

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.

terminate(_Reason, #state{s = Socket, port = Port}) ->
    erlang:port_close(Port),
    procket:close(Socket),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-------------------------------------------------------------------------
%%% Utility functions
%%-------------------------------------------------------------------------


%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------
maybe_binary(N) when is_binary(N) ->
    N;
maybe_binary(N) when is_list(N) ->
    list_to_binary(N).

send_rpc(Port, Buf) ->
    Len = ?REMOTE_MESSAGE_HEADER_XDR_LEN + byte_size(Buf),
    try erlang:port_command(Port, <<?UINT32(Len), Buf/binary>>) of
        true ->
            ok
        catch
            error:Error ->
                {error, Error}
        end.

reply_to_caller(Pid, Data) ->
    Reply = verx_rpc:decode(Data),
    Pid ! {verx, self(), Reply},
    ok.

getstate(Ref) when is_pid(Ref) ->
    gen_server:call(Ref, getstate).
