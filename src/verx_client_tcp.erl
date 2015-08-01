%% Copyright (c) 2012-2014, Michael Santos <michael.santos@gmail.com>
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

-include("verx.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
        pid,
        s,                  % socket
        proc,               % last called procedure
        serial = -1,        % serial number
        buf = <<>>
        }).


%%-------------------------------------------------------------------------
%%% Callbacks
%%-------------------------------------------------------------------------
init([Pid, Opt]) ->

    Host = proplists:get_value(host, Opt, "127.0.0.1"),
    Port = proplists:get_value(port, Opt, ?LIBVIRT_TCP_PORT),

    {IP, Family} = verx_client:resolv(Host),

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
                serial = Serial0
                } = State) when is_list(Arg) ->
    {Header, Call} = verx_rpc:call(Proc, Arg),
    Serial = Serial0 + 1,
    Message = verx_rpc:encode({Header#remote_message_header{
                    serial = <<Serial:32>>
                    }, Call}),
    Reply = case send_rpc(Socket, Message) of
        ok -> {ok, Serial};
        Error -> Error
    end,
    inet:setopts(Socket, [{active, once}]),
    {reply, Reply, State#state{proc = Proc, serial = Serial}};

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

handle_call(getserial, _From, #state{serial = Serial} = State) ->
    {reply, Serial, State};

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
                   buf = Buf} = State) ->
    inet:setopts(Socket, [{active, once}]),
    {Msgs, Rest} = verx_client:stream(<<Buf/binary, Data/binary>>),
    [ verx_client:reply_to_caller(Pid, Msg) || Msg <- Msgs ],
    {noreply, State#state{buf = Rest}};

handle_info({tcp_closed, Socket}, #state{s = Socket} = State) ->
    {stop, {shutdown, tcp_closed}, State};

handle_info({tcp_error, Socket, Reason}, #state{s = Socket} = State) ->
    {stop, {shutdown, Reason}, State};

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------
send_rpc(Socket, Buf) ->
    Len = ?REMOTE_MESSAGE_HEADER_XDR_LEN + byte_size(Buf),
    gen_tcp:send(Socket, <<?UINT32(Len), Buf/binary>>).
