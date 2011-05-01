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
-include("verx.hrl").

-export([start/0, stop/1]).
-export([call/2, call/3]).
-export([
        info/1, info/2,
        capabilities/1,

        create/1, create/2,
        list_domains/1, list_domains/2,

        destroy/2,
        id/2
    ]).

-define(XML_PATH, "priv/example.xml").


%%-------------------------------------------------------------------------
%%% API
%%-------------------------------------------------------------------------
start() ->
    verx_srv:start().
stop(Ref) ->
    verx_srv:stop(Ref).

call(Ref, Proc) ->
    verx_srv:call(Ref, Proc).
call(Ref, Proc, Arg) ->
    verx_srv:call(Ref, Proc, Arg).

info(Ref) ->
    info(Ref, node).

node_get_info(Ref) ->
    verx:call(Ref, node_get_info).

% Only the UUID is required
domain_get_info(Ref, UUID) ->
    domain_get_info(Ref, "", UUID, 0).

domain_get_info(Ref, Name, UUID, Id) when ( is_list(Name) orelse is_binary(Name) ),
    is_binary(UUID), is_integer(Id) ->
    verx:call(Ref, domain_get_info, [
            {string, Name},             % name
            {remote_uuid, UUID},        % UUID
            {int, Id}                   % id
        ]).

domain_lookup_by_id(Ref, N) when is_integer(N) ->
    verx:call(Ref, domain_lookup_by_id, [
            {int, N}                    % domain id
        ]).

get_capabilities(Ref) ->
    verx:call(Ref, get_capabilities).

create(Ref) ->
    domain_create_xml(Ref).
create(Ref, Path) ->
    domain_create_xml(Ref, Path).

domain_create_xml(Ref) ->
    domain_create_xml(Ref, ?XML_PATH).
domain_create_xml(Ref, Path) ->
    {ok, Bin} = file:read_file(Path),
    verx:call(Ref, domain_create_xml, [
            {remote_nonnull_string, Bin},   % XML
            {int, 0}                        % flags
        ]).

list_domains(Ref) ->
    list_domains(Ref, 10).
list_domains(Ref, N) when is_integer(N) ->
    verx:call(Ref, list_domains, [
            {int, N}                    % number of domains
        ]).

destroy(Ref, UUID) ->
    domain_destroy(Ref, UUID).

domain_destroy(Ref, UUID) ->
    domain_destroy(Ref, "", UUID, 0).

domain_destroy(Ref, Name, UUID, Id) when ( is_list(Name) orelse is_binary(Name) ),
    is_binary(UUID), is_integer(Id) ->
    verx:call(Ref, domain_destroy, [
            {remote_domain, [
                {remote_nonnull_string, ""},    % name
                {remote_uuid, UUID},            % UUID, binary
                {int, 0}                        % id
            ]}
        ]).
