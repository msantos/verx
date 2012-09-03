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
-module(verx_client).
-behaviour(gen_server).

-include("verx.hrl").
-include_lib("procket/include/procket.hrl").

-export([
    call/2, call/3,
    getfd/1
    ]).
-export([read_packet/1]).
-export([start_link/0, start_link/1]).
-export([start/0, start/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

-record(state, {
    path,   % Unix socket path
    s       % Unix socket fd
    }).

-define(SOCK_PATH, <<"/var/run/libvirt/libvirt-sock">>).


%%-------------------------------------------------------------------------
%%% API
%%-------------------------------------------------------------------------
call(Ref, Proc) ->
    call(Ref, Proc, []).
call(Ref, Proc, Arg) when is_pid(Ref), is_atom(Proc), is_list(Arg) ->
    Call = verx_rpc:call(Proc, Arg),
    Message = verx_rpc:encode(Call),
    case gen_server:call(Ref, {call, Message}, infinity) of
        {ok, Buf} ->
            status(verx_rpc:decode(Buf));
        Error ->
            Error
    end.

status({#remote_message_header{
                    proc = <<?REMOTE_REPLY:32>>,
                    status = Status
                    }}) ->
    verx_rpc:field(status, Status);
status({#remote_message_header{
                    status = Status
                    }, Reply}) ->
    {verx_rpc:field(status, Status), Reply}.

getfd(Ref) ->
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

    Path = maybe_binary(proplists:get_value(path, Opt, ?SOCK_PATH)),

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


handle_call({call, Message}, _From, #state{s = Socket} = State) when is_binary(Message) ->
    Len = ?REMOTE_MESSAGE_HEADER_XDR_LEN + byte_size(Message),
    ok = procket:sendto(Socket, <<?UINT32(Len), Message/binary>>),
    Reply = read_packet(Socket),
    {reply, Reply, State};

handle_call(getfd, _From, #state{s = Socket} = State) ->
    {reply, Socket, State};

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
read_packet(Socket) ->
    {ok, <<?UINT32(Len)>>} = read_all(Socket, ?REMOTE_MESSAGE_HEADER_XDR_LEN),
    read_all(Socket, Len - ?REMOTE_MESSAGE_HEADER_XDR_LEN).


%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------
maybe_binary(N) when is_binary(N) ->
    N;
maybe_binary(N) when is_list(N) ->
    list_to_binary(N).

read_all(Socket, Len) ->
    read_all(Socket, Len, []).
read_all(Socket, Len, Acc) ->
    case procket:read(Socket, Len) of
        {ok, Buf} when Len - byte_size(Buf) =:= 0 ->
            {ok, list_to_binary(lists:reverse([Buf|Acc]))};
        {ok, Buf} ->
            read_all(Socket, Len - byte_size(Buf), [Buf|Acc]);
        {error, eagain} ->
            % XXX can spin forever here
            timer:sleep(10),
            read_all(Socket, Len, Acc);
        Error ->
            Error
    end.
