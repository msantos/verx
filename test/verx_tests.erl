%% Copyright (c) 2012-2013, Michael Santos <michael.santos@gmail.com>
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


verx_test_() ->
    {timeout, 240, [
            {?LINE, fun() -> run(kvm, verx_client_unix) end},
%            {?LINE, fun() -> run(kvm, verx_client_tcp) end},
            {?LINE, fun() -> run(kvm, verx_client_tls) end},

            {?LINE, fun() -> run(lxc, verx_client_unix) end},
%            {?LINE, fun() -> run(lxc, verx_client_tcp) end},
            {?LINE, fun() -> run(lxc, verx_client_tls) end}
            ]}.

run(kvm, Transport) ->
    error_logger:info_report([{vm, kvm}, {transport, Transport}]),

    {ok, Ref} = verx_client:start([{transport, Transport}]),
    ok = verx:open(Ref),
    {ok, Domain} = create(kvm, Ref),

    [ begin ok = ?MODULE:Fun({Ref, Domain}) end ||
        Fun <- [
            domain_list_info,
            screenshot,
            node_info
            ] ],

    ok = destroy(Ref, Domain),

    ok;

run(lxc, Transport) ->
    error_logger:info_report([{vm, lxc}, {transport, Transport}]),

    {ok, Ref} = verx_client:start([{transport, Transport}]),
    ok = verx:open(Ref, ["lxc:///", 0]),
    {ok, Domain} = create(lxc, Ref),

    % XXX be careful here, the container is running as root
    ok = console_create_file(Ref, Transport, Domain),

    ok = destroy(Ref, Domain),

    ok.

domain_list_info({Ref, Domain}) ->
    {ok, [NumDef]} = verx:num_of_defined_domains(Ref),
    {ok, [NumRun]} = verx:num_of_domains(Ref),

    {ok, [Shutoff]} = verx:list_defined_domains(Ref, [NumDef]),
    {ok, [Running]} = verx:list_domains(Ref, [NumRun]),

    error_logger:info_report([{running, info(Ref, Running)},
                              {shutoff, info(Ref, Shutoff)}]),


    error_logger:info_report([{running, verx:domain_get_info(Ref, [Domain])}]),

    ok = verx:domain_suspend(Ref, [Domain]),
    error_logger:info_report([{suspended, verx:domain_get_info(Ref, [Domain])}]),

    ok = verx:domain_resume(Ref, [Domain]),
    error_logger:info_report([{resumed, verx:domain_get_info(Ref, [Domain])}]),

    ok = verx:domain_shutdown(Ref, [Domain]),
    error_logger:info_report([{shutdown, verx:domain_get_info(Ref, [Domain])}]),

    ok.

info(Ref, Domains) ->
    [ begin
        {ok, [{Name, UUID, Id}]} = verx:lookup(Ref, {domain, N}),
        {Name, [{uuid, UUID}, {id, Id}]}
      end || N <- Domains ].

screenshot({Ref, Domain}) ->
    {ok, [Mime]} = verx:domain_screenshot(Ref, [Domain,0,0]),

    Ext = case Mime of
        <<"image/x-portable-pixmap">> -> <<".ppm">>;
        _ -> <<".screen">>
    end,

    {ok, Buf} = verx_client:recvall(Ref),

    File = <<"localvm", Ext/binary>>,
    ok = file:write_file(File, Buf),

    error_logger:info_report([{screenshot, Mime, File}]),

    ok.

node_info({Ref, _Domain}) ->
    [ begin
            Reply = case Proc of
                {Call, Arg} -> verx:Call(Ref, Arg);
                Call -> verx:Call(Ref)
            end,
            result(Proc, Reply)
      end || Proc <- [
                node_get_info,
                {node_get_cells_free_memory, [0, 100]},
                get_version,
                get_lib_version,
                get_hostname,
                get_uri,
                node_get_free_memory,
                node_get_security_model,
                is_secure,
                get_capabilities
                ] ],
    ok.

result(Call, {ok, N}) ->
    error_logger:info_report([{call, Call}] ++ N);
result(Call, {error, _Error} = N) ->
    error_logger:error_report([{call, Call}] ++ N).


%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------

create(kvm, Ref) ->
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

create(lxc, Ref) ->
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

destroy(Ref, Domain)  ->
    ok = verx:domain_destroy(Ref, [Domain]),
    ok = verx:domain_undefine(Ref, [Domain]),
    verx:close(Ref),
    catch verx_client:stop(Ref),
    ok.

console_create_file(Ref, verx_client_unix, Domain) ->
    File = "/tmp/console-test-" ++ os:getpid(),
    {ok, FH} = file:open(File, [raw, write, exclusive]),
    ok = file:close(FH),
    ok = verx:domain_open_console(Ref, [Domain, void, 0]),
    ok = verx_client:send(Ref, [list_to_binary(["/bin/rm ", File, "\n"])]),
    verx_client:finish(Ref),
    % XXX race condition: must wait for the file to disappear from /tmp
    timer:sleep(1000),
    {error, enoent} = file:read_file(File),
    ok;
console_create_file(_Ref, _, _Domain) ->
    ok.
