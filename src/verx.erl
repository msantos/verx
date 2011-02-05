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

-export([
        info/0, info/1,
        capabilities/0,

        create/0, create/1,
        list_domains/0, list_domains/1,

        destroy/1,
        id/1,

        send/1, send/2
    ]).

-define(XML_PATH, "/var/tmp/iso/qemu/demo1.xml").


%%-------------------------------------------------------------------------
%%% API
%%-------------------------------------------------------------------------
info() ->
    info(node).

info(node) ->
    send(node_get_info);
info({domain, UUID}) when is_binary(UUID) ->
    send(domain_get_info, [
            verx_xdr:encode({string, ""}),  % name
            UUID,                           % UUID
            verx_xdr:encode({int, 0})       % id
        ]).

id(N) ->
    send(domain_lookup_by_id, [
            verx_xdr:encode({int, N})    % domain id
        ]).

capabilities() ->
    send(get_capabilities).

create() ->
    create(?XML_PATH).
create(Path) ->
    {ok, Bin} = file:read_file(Path),
    send(domain_create_xml, [
            verx_xdr:encode({string, Bin}), % XML
            verx_xdr:encode({int, 0})       % flags
        ]).

list_domains() ->
    list_domains(10).
list_domains(N) ->
    send(list_domains, [
            verx_xdr:encode({int, N})    % number of domains
        ]).

destroy(UUID) ->
    send(domain_destroy, [
            verx_xdr:encode({string, ""}),  % UUID, not checked?
            UUID,                           % UUID, binary
            verx_xdr:encode({int, 0})       % id, not checked
        ]).

send(Message) ->
    send(Message, []).
send(Message, Args) ->
    {ok,S} = verx_rpc:open(),
    Res = case Args of
        [] -> verx_rpc:call(S, Message);
        N -> verx_rpc:call(S, Message, list_to_binary(N))
    end,
    verx_rpc:close(S),
    verx_rpc:response(Message, Res).

