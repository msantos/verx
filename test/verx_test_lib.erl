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
run({Ref, _} = State, Transport) ->
    {ok, [Type0]} = verx:get_type(Ref),
    Type = case Type0 of
        <<"Test">> -> test;
        <<"QEMU">> -> kvm;
        <<"LXC">> -> lxc
    end,
    run(Type, State, Transport).

run(test, State, _Transport) ->
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
    ]};
run(kvm, State, _Transport) ->
    {inorder, [
            verx_test_lib:num_of_defined_domains(State),
            verx_test_lib:num_of_domains(State),
            verx_test_lib:list_defined_domains(State),
            verx_test_lib:list_domains(State),
            verx_test_lib:domain_suspend(State),
            verx_test_lib:domain_resume(State),
            verx_test_lib:domain_shutdown(State),
            verx_test_lib:screenshot(State),
            verx_test_lib:lookup(State, "testvm"),
            verx_test_lib:node_get_info(State),
            verx_test_lib:node_get_cells_free_memory(State),
            verx_test_lib:get_version(State),
            verx_test_lib:get_lib_version(State),
            verx_test_lib:get_hostname(State),
            verx_test_lib:get_uri(State),
            verx_test_lib:node_get_free_memory(State),
            verx_test_lib:node_get_security_model(State),
            verx_test_lib:is_secure(State),
            verx_test_lib:get_capabilities(State)
    ]};
run(lxc, State, Transport) ->
    {inorder, [
            verx_test_lib:console_create_file(State, Transport)
    ]}.

%%
%% Hypervisors
%%
start(Transport) ->
    start(Transport, test).
start(Transport, VM) ->
    {ok, Ref} = verx_client:start([{transport, Transport}]),
    ok = case VM of
        test -> verx:open(Ref, [<<"test:///default">>, 0]);
        kvm -> verx:open(Ref);
        lxc -> verx:open(Ref, [<<"lxc:///">>, 0])
    end,
    {ok, Domain} = create(Ref, VM),
    {Ref, Domain}.

%% TEST
create(Ref, test) ->
    {ok, [Domain]} = verx:lookup(Ref, {domain, <<"test">>}),
    {ok, Domain};

%% KVM
create(Ref, kvm) ->
    Path = filename:join([
            filename:dirname(code:which(vert)),
            "..",
            "priv",
            "example.xml"
            ]),

    {ok, XML} = file:read_file(Path),

    {ok, [Domain]} = verx:domain_define_xml(Ref, [XML]),
    ok = verx:domain_create(Ref, [Domain]),
    {ok, Domain};

%% LXC
create(Ref, lxc) ->
    Name = "test-" ++ integer_to_list(erlang:phash2(Ref)),

    <<Bytes:3/bytes, _/binary>> = erlang:md5(Name),
    Macaddr = "52:54:00:" ++ string:join([ httpd_util:integer_to_hexlist(N)
        || <<N:8>> <= Bytes ], ":"),

    XML = template(Name, Macaddr),

    {ok, [Domain]} = verx:domain_define_xml(Ref, [XML]),
    verx:domain_create(Ref, [Domain]),

    {ok, Domain}.

template(Name, Macaddr) ->
"<domain type='lxc'>
    <name>" ++ Name ++ "</name>
    <memory>102400</memory>
    <os>
        <type>exe</type>
        <init>/bin/sh</init>
    </os>
    <devices>
        <console type='pty'/>
            <interface type='bridge'>
                <mac address='" ++ Macaddr ++ "'/>
                <source bridge='br0'/>
            </interface>
    </devices>
</domain>".


%%
%% libvirt functions calls
%%

% KVM, test
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

% lxc
console_create_file({Ref, Domain}, verx_client_unix) ->
    File = "/tmp/console-test-" ++ os:getpid(),
    {ok, FH} = file:open(File, [raw, write, exclusive]),
    ok = file:close(FH),
    ok = verx:domain_open_console(Ref, [Domain, void, 0]),
    ok = verx_client:send(Ref, [list_to_binary(["/bin/rm ", File, "\n"])]),
    verx_client:finish(Ref),
    % XXX race condition: must wait for the file to disappear from /tmp
    timer:sleep(1000),
    ?_assertEqual({error, enoent}, file:read_file(File));
console_create_file({_Ref, _Domain}, _) ->
    ?_assert(true).

destroy(Ref, Domain)  ->
    ok = verx:domain_destroy(Ref, [Domain]),
    ok = verx:domain_undefine(Ref, [Domain]),
    verx:close(Ref),
    catch verx_client:stop(Ref),
    ok.

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
