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
-module(verx_config).
-include_lib("xmerl/include/xmerl.hrl").

%%
%% Convert between libvirt XML configuration and Erlang terms
%%
-export([
        init/0, init/1,

        set/2, set/3,
        append/3,

        to_xml/1
    ]).

init() ->
    {domain, [], []}.

init(Attr) ->
    lists:foldl(fun(A,Cfg) -> set_attr(A, Cfg) end,
        init(),
        Attr).

to_xml(Cfg) when is_tuple(Cfg) ->
    lists:flatten(
        xmerl:export_simple_content([Cfg], xmerl_xml)
    ).

set(Key, Val) when is_atom(Key) ->
    {Key, maybe_list(Val)};
set({Key, Attr}, Val) ->
    {Key, Attr, maybe_list(Val)};
set(Keys, Val) when is_list(Keys) ->
    set_1(Keys, Val).

set_1(Keys, Val) ->
    {Node, Rest} = case lists:reverse(Keys) of
        [{K,A}|T] ->
            {{K,A,maybe_list(Val)}, T};
        [H|T] ->
            {{H,maybe_list(Val)}, T}
    end,
    set_2(Rest, Node).

set_2([], Node) ->
    Node;
set_2([{Key, Attr}|T], Node) ->
    set_2(T, {Key, Attr, [Node]});
set_2([Key|T], Node) ->
    set_2(T, {Key, [Node]}).

set(Key, Val, {domain, _, _} = Cfg) ->
    set_value(set(Key, Val), Cfg).

append(Key, Val, {domain, _, _} = Cfg) ->
    append_value(set(Key, Val), Cfg).

set_value({Key, Val}, {domain, Attr, Content}) ->
    {domain, Attr, set_opt({Key, Val}, Content)};
set_value({Key, Opt, Val}, {domain, Attr, Content}) ->
    {domain, Attr, set_opt({Key, Opt, Val}, Content)}.

append_value({Key, Val}, {domain, Attr, Content}) ->
    {domain, Attr, append_opt({Key, Val}, Content)}.

set_attr({Key, Val}, {domain, Attr, Content}) ->
    {domain, set_opt({Key, Val}, Attr), Content}.

set_opt({Key, Val}, Options) ->
    lists:keystore(Key, 1, Options, {Key, Val});
set_opt({Key, Opt, Val}, Options) ->
    lists:keystore(Key, 1, Options, {Key, Opt, Val}).

append_opt({Key, Val}, Options) ->
    case lists:keyfind(Key, 1, Options) of
        false ->
            set_opt({Key, Val}, Options);
        {Key, OVal} ->
            set_opt({Key, Val ++ OVal}, Options)
    end.

maybe_list([C|_] = N) when is_integer(C) -> [N];
maybe_list(N) when is_list(N) -> N;
maybe_list(N) -> [N].
