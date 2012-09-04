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

    download/1,
    upload/2,

    getfd/1
    ]).
-export([read_packet/1, read_packet/2]).
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
            verx_rpc:status(verx_rpc:decode(Buf));
        Error ->
            Error
    end.

% XXX handle in caller or in gen_server?
download(Ref) ->
    FD = getfd(Ref),
    download(FD, []).

download(FD, Acc) ->
    case verx_client:read_packet(FD) of
        {ok, Bin} ->
            case verx_rpc:decode(Bin) of
                {#remote_message_header{
                                type = <<?REMOTE_STREAM:32>>,
                                status = <<?REMOTE_CONTINUE:32>>}, []} ->
                    {ok, lists:reverse(Acc)};
                {#remote_message_header{
                                type = <<?REMOTE_STREAM:32>>,
                                status = <<?REMOTE_CONTINUE:32>>}, Payload} ->
                    download(FD, [Payload|Acc])
            end;
        Error ->
            Error
    end.

upload(Ref, Buf) when is_list(Buf) ->
    FD = getfd(Ref),
    upload_1(FD, Buf).

upload_1(_FD, []) ->
    ok;
upload_1(FD, [Buf|Rest]) when is_binary(Buf) ->
    ok = procket:write(FD, Buf),
    upload_1(FD, Rest).

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
    read_packet(Socket, 1000).
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
