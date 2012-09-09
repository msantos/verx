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
-module(verx_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("verx.hrl").

vert_unix_test_() ->
    {setup,
     fun unix/0,
     fun destroy/1,
     fun(N) -> [?_test(domain_list_info(N)),
                 ?_test(screenshot(N))] end
    }.

vert_tcp_test_() ->
    {setup,
     fun tcp/0,
     fun destroy/1,
     fun(N) -> [?_test(domain_list_info(N)),
                 ?_test(screenshot(N))] end
    }.

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

    {ok, Mime, File}.


%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------

unix() ->
    {ok, Ref} = verx_client:start(),
    create(Ref).

tcp() ->
    {ok, Ref} = verx_client:start([{transport, verx_client_tcp}]),
    create(Ref).

create(Ref) ->
    ok = verx:open(Ref),

    Path = filename:join([
            filename:dirname(code:which(vert)),
            "..",
            "priv",
            "example.xml"
            ]),

    {ok, XML} = file:read_file(Path),

    {ok, [Domain]} = verx:domain_define_xml(Ref, [XML]),
    ok = verx:domain_create(Ref, [Domain]),
    {Ref, Domain}.

destroy({Ref, Domain})  ->
    ok = verx:domain_destroy(Ref, [Domain]),
    ok = verx:domain_undefine(Ref, [Domain]),
    verx:close(Ref),
    verx_client:stop(Ref).
