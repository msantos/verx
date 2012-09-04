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

create_list_destroy_test() ->
    {ok, Ref} = verx_client:start(),
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

    {ok, [NumDef]} = verx:num_of_defined_domains(Ref),
    {ok, [NumRun]} = verx:num_of_domains(Ref),

    {ok, [Shutoff]} = verx:list_defined_domains(Ref, [NumDef]),
    {ok, [Running]} = verx:list_domains(Ref, [NumRun]),

    error_logger:info_report([{running, info(Ref, Running)},
                              {shutoff, info(Ref, Shutoff)}]),

    ok = verx:domain_destroy(Ref, [Domain]),

    verx:close(Ref),
    verx_client:stop(Ref).

info(Ref, Domains) ->
    [ begin
        {ok, [{Name, UUID, Id}]} = verx:domain_lookup_by_id(Ref, [N]),
        {Name, [{uuid, UUID}, {id, Id}]}
      end || N <- Domains ].
