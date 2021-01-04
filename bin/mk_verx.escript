#!/usr/bin/env escript

%%%
%%% Generate the verx.erl file
%%%

main(_) ->
    % load remote_protocol_xdr
    Path = os:getenv(
        "REBAR_BUILD_DIR",
        filename:join(
            filename:dirname(escript:script_name()),
            "../_build/default"
        )
    ),

    true = code:add_pathz(filename:join(Path, "lib/verx/ebin")),

    {module, remote_protocol_xdr} = code:load_file(remote_protocol_xdr),

    {{Year, _, _}, {_, _, _}} = calendar:universal_time(),

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
        " POSSIBILITY OF SUCH DAMAGE."
    ],

    License = erl_syntax:comment(Comment),

    Module = erl_syntax:attribute(
        erl_syntax:atom(module),
        [erl_syntax:atom("verx")]
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
            || {Fun, Arity} <- static_exports()
        ])
    ]),

    Comment_gen = erl_syntax:comment([" Generated functions"]),
    Exports_gen = erl_syntax:attribute(erl_syntax:atom(export), [
        erl_syntax:list([
            erl_syntax:arity_qualifier(erl_syntax:atom(Fun), erl_syntax:integer(Arity + 1))
            || {Fun, _, Arity} <- Calls
        ])
    ]),

    % Generate the functions
    Functions = [
        begin
            {Pattern, Body} =
                case Arity of
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
                                [
                                    erl_syntax:variable("Ref"),
                                    erl_syntax:atom(Proc),
                                    erl_syntax:variable("Arg")
                                ]
                            )}
                end,

            Clause = erl_syntax:clause(Pattern, [], [Body]),
            erl_syntax:function(erl_syntax:atom(Fun), [Clause])
        end
        || {Fun, Proc, Arity} <- Calls
    ],

    Code0 = erl_prettypr:format(
        erl_syntax:form_list(
            lists:flatten([
                License,
                Module,
                Includes,

                Comment_static,
                Exports_static,

                Comment_gen,
                Exports_gen,

                Static,
                Functions
            ])
        )
    ),

    Code = re:replace(Code0, "%%__STATIC__%%", static()),

    io:format("~s~n", [Code]).

% List the supported libvirt remote protocol procedures
calls() ->
    % count from 1 until an exception is thrown
    calls(1, []).

calls(N, Acc) ->
    try remote_protocol_xdr:dec_remote_procedure(<<N:32>>, 0) of
        {Proc, _Off} -> calls(N + 1, [Proc | Acc])
    catch
        error:{case_clause, _} ->
            call_to_fun(lists:reverse(Acc))
    end.

call_to_fun(Calls) ->
    Exports = proplists:get_value(exports, remote_protocol_xdr:module_info()),
    [
        begin
            "remote_proc_" ++ Name = string:to_lower(atom_to_list(N)),
            {
                % function name
                Name,
                % remote protocol procedure
                N,
                % number of arguments
                call_arity(Name, Exports)
            }
        end
        || N <- Calls
    ].

call_arity(Proc, Exports) ->
    Fun = list_to_atom("enc_remote_" ++ Proc ++ "_args"),
    proplists:get_value(Fun, Exports, 0).

static_exports() ->
    [
        {connect_open, 1},
        {open, 1},
        {open, 2},
        {close, 1},
        {lookup, 2}
    ].

static() ->
    [static({Fun, Arity}) || {Fun, Arity} <- static_exports()].

static({connect_open, 1}) ->
    "\n"
    "% Send a remote protocol open message\n"
    "%     <<>> : name\n"
    "%     0 : flags\n"
    "% See: remote_protocol_xdr:enc_remote_connect_open_args/1\n"
    "connect_open(Ref) ->\n"
    "    connect_open(Ref, [<<>>, 0]).\n";
static({open, 1}) ->
    "\n"
    "% For backwards compatibility, use connect_open/1,2.\n"
    "open(Ref) ->\n"
    "    connect_open(Ref, [<<>>, 0]).\n";
static({open, 2}) ->
    "\n"
    "open(Ref, Arg) ->\n"
    "    connect_open(Ref, Arg).\n";
static({close, 1}) ->
    "\n"
    "close(Ref) ->\n"
    "    connect_close(Ref).\n";
static({lookup, 2}) ->
    "\n"
    "lookup(Ref, {domain, Name}) ->\n"
    "    Fun = [ fun() -> verx:domain_lookup_by_id(Ref, [maybe_integer(Name)]) end,\n"
    "            fun() -> verx:domain_lookup_by_name(Ref, [maybe_binary(Name)]) end,\n"
    "            fun() -> verx:domain_lookup_by_uuid(Ref, [uuid:string_to_uuid(Name)]) end,\n"
    "            fun() -> verx:domain_lookup_by_uuid(Ref, [maybe_binary(Name)]) end ],\n"
    "    lookup_1(Fun).\n"
    "\n"
    "lookup_1(Fun)  ->\n"
    "    lookup_1(Fun, []).\n"
    "lookup_1([], [{error, Error}|_]) ->\n"
    "    {error, Error};\n"
    "lookup_1([Fun|Tail], Acc) ->\n"
    "    try Fun() of\n"
    "        {ok, Res} ->\n"
    "            {ok, Res};\n"
    "        {error, Error} ->\n"
    "            lookup_1(Tail, [{error, Error}|Acc])\n"
    "        catch\n"
    "            _:_ ->\n"
    "                lookup_1(Tail, Acc)\n"
    "    end.\n"
    "\n"
    "maybe_integer(N) when is_integer(N) -> N;\n"
    "maybe_integer(N) when is_list(N) -> list_to_integer(N).\n"
    "\n"
    "maybe_binary(N) when is_binary(N) -> N;\n"
    "maybe_binary(N) when is_list(N) -> list_to_binary(N).\n".

includes(Header) ->
    [erl_syntax:attribute(erl_syntax:atom(include), [erl_syntax:string(N)]) || N <- Header].
