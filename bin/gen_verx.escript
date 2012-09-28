#!/usr/bin/env escript

%%%
%%% Generate the verx.erl file
%%%
main([]) ->
    File = "verx.erl",
    main([File]);

main([File]) ->
    % load remote_protocol_xdr
    true = code:add_pathz(filename:dirname(escript:script_name())
            ++ "/../ebin"),

    {{Year,_,_},{_,_,_}} = calendar:universal_time(),

    Date = integer_to_list(Year),

    Comment = [
" Copyright (c) 2011-" ++ Date ++ ", Michael Santos <michael.santos@gmail.com>",
" All rights reserved.",
"",
" Redistribution and use in source and binary forms, with or without",
" modification, are permitted provided that the following conditions",
" are met:",
"",
" Redistributions of source code must retain the above copyright",
" notice, this list of conditions and the following disclaimer.",
"",
" Redistributions in binary form must reproduce the above copyright",
" notice, this list of conditions and the following disclaimer in the",
" documentation and/or other materials provided with the distribution.",
"",
" Neither the name of the author nor the names of its contributors",
" may be used to endorse or promote products derived from this software",
" without specific prior written permission.",
"",
" THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS",
" \"AS IS\" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT",
" LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS",
" FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE",
" COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,",
" INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,",
" BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;",
" LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER",
" CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT",
" LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN",
" ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE",
" POSSIBILITY OF SUCH DAMAGE."],

    License = erl_syntax:comment(Comment),

    Module = erl_syntax:attribute(
            erl_syntax:atom(module),
            [erl_syntax:atom(filename:basename(File, ".erl"))]
            ),
    Includes = includes(["verx.hrl"]),

    % Any hardcoded functions will be included here
    Static = erl_syntax:comment(["%__STATIC__%%"]),

    Calls = calls(),

    % Generate the list of exports
    Comment_static = erl_syntax:comment([" Static functions"]),
    Exports_static = erl_syntax:attribute(erl_syntax:atom(export), [
                erl_syntax:list([
                    erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity))
                        || {Fun, Arity} <- static_exports() ])
                ]),

    Comment_gen = erl_syntax:comment([" Generated functions"]),
    Exports_gen = erl_syntax:attribute(erl_syntax:atom(export), [
                erl_syntax:list([
                    erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity+1))
                        || {Fun, _, Arity} <- Calls ])
                ]),

    % Generate the functions
    Functions = [ begin
                    {Pattern, Body} = case Arity of
                        0 ->
                            % name(Ref) -> verx_client:call(Ref, 'PROC_NAME').
                            {[erl_syntax:variable("Ref")],
                              erl_syntax:application(
                                erl_syntax:atom(verx_client),
                                erl_syntax:atom(call),
                                [erl_syntax:variable("Ref"), erl_syntax:atom(Proc)]
                              )};
                        _ ->
                            % name(Ref, Arg) -> verx_client:call(Ref, 'PROC_NAME', Arg).
                            {[erl_syntax:variable("Ref"), erl_syntax:variable("Arg")],
                              erl_syntax:application(
                                erl_syntax:atom(verx_client),
                                erl_syntax:atom(call),
                                [erl_syntax:variable("Ref"), erl_syntax:atom(Proc),
                                 erl_syntax:variable("Arg")]
                              )}
                    end,

                    Clause = erl_syntax:clause(Pattern, [], [Body]),
                    erl_syntax:function(erl_syntax:atom(Fun), [Clause])
                end || {Fun, Proc, Arity} <- Calls ],

    Code0 = erl_prettypr:format(erl_syntax:form_list(lists:flatten([
                License,
                Module,
                Includes,

                Comment_static,
                Exports_static,

                Comment_gen,
                Exports_gen,

                Static,
                Functions
            ]))),

    Code = re:replace(Code0, "%%__STATIC__%%", static()),

%    io:format("~s~n", [Code]).
    file:write_file(File, [Code]).

% List the supported libvirt remote protocol procedures
calls() ->
    % count from 1 until an exception is thrown
    calls(1, []).
calls(N, Acc) ->
    try remote_protocol_xdr:dec_remote_procedure(<<N:32>>, 0) of
        {Proc, _Off} -> calls(N+1, [Proc|Acc])
    catch
        error:{case_clause,_} ->
            call_to_fun(lists:reverse(Acc))
    end.

call_to_fun(Calls) ->
    Exports = proplists:get_value(exports, remote_protocol_xdr:module_info()),
    [ begin
        "remote_proc_" ++ Name = string:to_lower(atom_to_list(N)),
        {
            Name,                       % function name
            N,                          % remote protocol procedure
            call_arity(Name, Exports)   % number of arguments
        }
      end || N <- Calls ].

call_arity(Proc, Exports) ->
    Fun = list_to_atom("enc_remote_" ++ Proc ++ "_args"),
    proplists:get_value(Fun, Exports, 0).

static_exports() ->
    [{open, 1},
     {lookup, 2}].

static() ->
    [ static({Fun, Arity}) || {Fun, Arity} <- static_exports() ].

static({open, 1}) ->
"
% Send a remote protocol open message
%     <<>> : name
%     0 : flags
% See: remote_protocol_xdr:enc_remote_open_args/1

open(Ref) ->
    open(Ref, [<<>>, 0]).
";

static({lookup, 2}) ->
"
lookup(Ref, {domain, Name}) ->
    Fun = [ fun() -> verx:domain_lookup_by_id(Ref, [maybe_integer(Name)]) end,
            fun() -> verx:domain_lookup_by_name(Ref, [maybe_binary(Name)]) end,
            fun() -> verx:domain_lookup_by_uuid(Ref, [uuid:string_to_uuid(Name)]) end,
            fun() -> verx:domain_lookup_by_uuid(Ref, [maybe_binary(Name)]) end ],
    lookup_1(Fun).

lookup_1(Fun)  ->
    lookup_1(Fun, []).
lookup_1([], [{error, Error}|_]) ->
    {error, Error};
lookup_1([Fun|Tail], Acc) ->
    try Fun() of
        {ok, Res} ->
            {ok, Res};
        {error, Error} ->
            lookup_1(Tail, [{error, Error}|Acc])
        catch
            _:_ ->
                lookup_1(Tail, Acc)
    end.

maybe_integer(N) when is_integer(N) -> N;
maybe_integer(N) when is_list(N) -> list_to_integer(N).

maybe_binary(N) when is_binary(N) -> N;
maybe_binary(N) when is_list(N) -> list_to_binary(N).
".

includes(Header) ->
    [ erl_syntax:attribute(erl_syntax:atom(include), [erl_syntax:string(N)]) || N <- Header ].
