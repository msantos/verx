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
-module(verx_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("verx.hrl").


create_test() ->
    {ok, Ref} = verx:start(),

    {ok, _Cap} = verx:capabilities(Ref),

    {ok,[{model,_},
         {memory,_},
         {cpus,_},
         {mhz,_},
         {nodes,_},
         {sockets,_},
         {cores,_},
         {threads,_}]} = verx:node_get_info(Ref),

    {ok, Domain} =  verx:create(Ref),

    {ok, [{ids, [_|_]}]} = verx:list_domains(Ref),

    Dom = proplists:get_value(dom, Domain),
    UUID = proplists:get_value(uuid, Dom),

    {ok, void} = verx:destroy(Ref, UUID),

    {error,{{remote_error,[{int,42},
                    {int,10},
                    {remote_string,<<"Domain not found: no domain with matching id 31337">>},
                    {int,2},
                    {remote_domain,<<>>},
                    {remote_string,<<"Domain not found: %s">>},
                    {remote_string,<<"no domain with matching id 31337">>},
                    {remote_string,<<>>},
                    {int,-1},
                    {int,-1},
                    {remote_network,<<>>}]},
            <<>>}} = verx:domain_lookup_by_id(Ref, 31337),

    ok = verx:stop(Ref).


node_info_test() ->
    {ok, Ref} = verx:start(),

    [ result(N, verx:call(Ref, N)) || N <- [
        node_get_info,
        node_get_cells_free_memory,
        get_version,
        get_lib_version,
        get_hostname,
        get_uri,
        node_get_free_memory,
        node_get_security_model,
        is_secure,
        get_capabilities
    ] ],

    ok = verx:stop(Ref).

result(Op, {ok, N}) ->
    error_logger:info_report([{op, Op}] ++ N);
result(Op, {error, _Error} = N) ->
    error_logger:error_report([{op, Op}] ++ [N]).
