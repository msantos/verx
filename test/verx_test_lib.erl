%% Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
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
-module(verx_test_lib).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("verx.hrl").

%%
%% Test Sets
%%

% Not all drivers support the same features
run(State) ->
    {inorder, [
            verx_test_lib:num_of_defined_domains(State),
            verx_test_lib:num_of_domains(State),
            verx_test_lib:list_defined_domains(State),
            verx_test_lib:list_domains(State),
            verx_test_lib:domain_suspend(State),
            verx_test_lib:domain_resume(State),
            verx_test_lib:domain_shutdown(State),
            verx_test_lib:lookup(State, "test"),
            verx_test_lib:node_get_info(State),
            verx_test_lib:node_get_cells_free_memory(State),
            verx_test_lib:get_version(State),
            verx_test_lib:get_lib_version(State),
            verx_test_lib:get_hostname(State),
            verx_test_lib:get_uri(State),
            verx_test_lib:is_secure(State),
            verx_test_lib:get_capabilities(State)
    ]}.

%%
%% Hypervisors
%%
start(Transport) ->
    {ok, Ref} = verx_client:start([{transport, Transport}]),
    ok = verx:open(Ref, [<<"test:///default">>, 0]),
    {ok, [Domain]} = verx:lookup(Ref, {domain, <<"test">>}),
    {Ref, Domain}.

destroy(Ref, Domain)  ->
    ok = verx:domain_destroy(Ref, [Domain]),
    ok = verx:domain_undefine(Ref, [Domain]),
    verx:close(Ref),
    catch verx_client:stop(Ref),
    ok.

%%
%% libvirt functions calls
%%

num_of_defined_domains({Ref, _Domain}) ->
    {ok, [NumDef]} = verx:num_of_defined_domains(Ref),
    ?_assert(is_integer(NumDef)).

num_of_domains({Ref, _Domain}) ->
    {ok, [NumRun]} = verx:num_of_domains(Ref),
    ?_assert(NumRun > 0).

list_defined_domains({Ref, _Domain}) ->
    {ok, [NumDef]} = verx:num_of_defined_domains(Ref),
    Shutoff = verx:list_defined_domains(Ref, [NumDef]),
    ?_assertMatch({ok, _}, Shutoff).

list_domains({Ref, _Domain}) ->
    {ok, [NumRun]} = verx:num_of_domains(Ref),
    Running = verx:list_domains(Ref, [NumRun]),
    ?_assertMatch({ok, _}, Running).

domain_suspend({Ref, Domain}) ->
    N = verx:domain_suspend(Ref, [Domain]),
    ?_assertEqual(ok, N).

domain_resume({Ref, Domain}) ->
    N = verx:domain_resume(Ref, [Domain]),
    ?_assertEqual(ok, N).

domain_shutdown({Ref, Domain}) ->
    N = verx:domain_shutdown(Ref, [Domain]),
    ?_assertEqual(ok, N).

lookup({Ref, _Domain}, Name) ->
    N = verx:lookup(Ref, {domain, Name}),
    ?_assertMatch({ok, [{_, _, _}]}, N).

screenshot({Ref, Domain}) ->
    {ok, [<<"image/x-portable-pixmap">>]} = verx:domain_screenshot(Ref, [Domain,0,0]),
    {ok, Buf} = verx_client:recvall(Ref),
    ?_assert(iolist_size(Buf) > 0).

node_get_info({Ref, _Domain}) ->
    N = verx:node_get_info(Ref),
    ?_assertMatch({ok, _}, N).

node_get_cells_free_memory({Ref, _Domain}) ->
    N = verx:node_get_cells_free_memory(Ref, [0,100]),
    ?_assertMatch({ok, _}, N).

get_version({Ref, _Domain}) ->
    N = verx:get_version(Ref),
    ?_assertMatch({ok, _}, N).

get_lib_version({Ref, _Domain}) ->
    N = verx:get_lib_version(Ref),
    ?_assertMatch({ok, _}, N).

get_hostname({Ref, _Domain}) ->
    N = verx:get_hostname(Ref),
    ?_assertMatch({ok, _}, N).

get_uri({Ref, _Domain}) ->
    N = verx:get_uri(Ref),
    ?_assertMatch({ok, _}, N).

node_get_free_memory({Ref, _Domain}) ->
    N = verx:node_get_free_memory(Ref),
    ?_assertMatch({ok, _}, N).

node_get_security_model({Ref, _Domain}) ->
    N = verx:node_get_security_model(Ref),
    ?_assertMatch({ok, _}, N).

is_secure({Ref, _Domain}) ->
    N = verx:is_secure(Ref),
    ?_assertMatch({ok, _}, N).

get_capabilities({Ref, _Domain}) ->
    N = verx:is_secure(Ref),
    ?_assertMatch({ok, _}, N).

%%
%% Utilities
%%
getenv(Var, Default) when is_list(Var), is_integer(Default) ->
    case os:getenv(Var) of
        false -> Default;
        N -> list_to_integer(N)
    end.

is_listening(Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {active,false}]) of
        {ok, S} ->
            ok = gen_tcp:close(S),
            true;
        _ ->
            false
    end.
