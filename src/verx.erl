%% Copyright (c) 2011, Michael Santos <michael.santos@gmail.com>
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
-module(verx).
-behaviour(gen_server).
-include("verx.hrl").

-export([call/2, call/3]).
-export([start_link/0, start_link/1]).
-export([start/0, start/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).

% usage examples
% move somewhere else
-export([
        info/1, info/2,
        capabilities/1,

        create/1, create/2,
        list_domains/1, list_domains/2,

        destroy/2,
        id/2
    ]).

-define(XML_PATH, "/var/tmp/iso/qemu/demo1.xml").

-record(state, {
    path,   % Unix socket path
    s       % Unix socket fd
    }).



%%-------------------------------------------------------------------------
%%% API
%%-------------------------------------------------------------------------
call(Ref, Proc) ->
    call(Ref, Proc, []).
call(Ref, Proc, Arg) when is_atom(Proc), is_list(Arg) ->
    gen_server:call(Ref, {call, Proc, Arg}).

info(Ref) ->
    info(Ref, node).

info(Ref, node) ->
    call(Ref, node_get_info);
info(Ref, {domain, UUID}) when is_binary(UUID) ->
    call(Ref, domain_get_info, [
            verx_xdr:encode({string, ""}),  % name
            UUID,                           % UUID
            verx_xdr:encode({int, 0})       % id
        ]).

id(Ref, N) ->
    call(Ref, domain_lookup_by_id, [
            verx_xdr:encode({int, N})       % domain id
        ]).

capabilities(Ref) ->
    call(Ref, get_capabilities).

create(Ref) ->
    create(Ref, ?XML_PATH).
create(Ref, Path) ->
    {ok, Bin} = file:read_file(Path),
    call(Ref, domain_create_xml, [
            verx_xdr:encode({string, Bin}), % XML
            verx_xdr:encode({int, 0})       % flags
        ]).

list_domains(Ref) ->
    list_domains(Ref, 10).
list_domains(Ref, N) ->
    call(Ref, list_domains, [
            verx_xdr:encode({int, N})       % number of domains
        ]).

destroy(Ref, UUID) when is_binary(UUID) ->
    call(Ref, domain_destroy, [
            verx_xdr:encode({string, ""}),  % UUID, not checked?
            UUID,                           % UUID, binary
            verx_xdr:encode({int, 0})       % id, not checked
        ]).


start() ->
    start([]).
start(Path) ->
    gen_server:start(?MODULE, [Path], []).

start_link() ->
    start_link([]).
start_link(Path) when is_list(Path) ->
    gen_server:start_link(?MODULE, [Path], []).


%%-------------------------------------------------------------------------
%%% Callbacks
%%-------------------------------------------------------------------------
init([Path]) ->
    {ok, Socket} = case Path of
        [] -> verx_rpc:open();
        File -> verx_rpc:open(File)
    end,

    {ok, #state{
            s = Socket
        }}.


handle_call({call, Proc, []}, _From, #state{s = S} = State) ->
    Res = verx_rpc:call(S, Proc),
    Reply = verx_rpc:response(Proc, Res),
    {reply, Reply, State};
handle_call({call, Proc, Arg}, _From, #state{s = S} = State) ->
    Res = verx_rpc:call(S, Proc, list_to_binary(Arg)),
    Reply = verx_rpc:response(Proc, Res),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% WTF?
handle_info(Info, State) ->
    error_logger:error_report([{wtf, Info}]),
    {noreply, State}.

terminate(_Reason, #state{s = S}) ->
    verx_rpc:close(S),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
