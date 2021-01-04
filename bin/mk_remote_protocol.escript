#!/usr/bin/env escript

%%%
%%% Generate the remote_protocol_xdr.erl module from
%%% the libvirt remote_protocol.x file
%%%

-include_lib("verx/include/libvirt_constants.hrl").

% Values from libvirt.h
defines() ->
    [
        {"VIR_SECURITY_MODEL_BUFLEN", ?VIR_SECURITY_MODEL_BUFLEN},
        {"VIR_SECURITY_LABEL_BUFLEN", ?VIR_SECURITY_LABEL_BUFLEN},
        {"VIR_SECURITY_DOI_BUFLEN", ?VIR_SECURITY_DOI_BUFLEN},
        {"VIR_UUID_BUFLEN", ?VIR_UUID_BUFLEN},
        {"VIR_TYPED_PARAM_INT", ?VIR_TYPED_PARAM_INT},
        {"VIR_TYPED_PARAM_UINT", ?VIR_TYPED_PARAM_UINT},
        {"VIR_TYPED_PARAM_LLONG", ?VIR_TYPED_PARAM_LLONG},
        {"VIR_TYPED_PARAM_ULLONG", ?VIR_TYPED_PARAM_ULLONG},
        {"VIR_TYPED_PARAM_DOUBLE", ?VIR_TYPED_PARAM_DOUBLE},
        {"VIR_TYPED_PARAM_BOOLEAN", ?VIR_TYPED_PARAM_BOOLEAN},
        {"VIR_TYPED_PARAM_STRING", ?VIR_TYPED_PARAM_STRING}
    ].

main([]) ->
    Src = "priv/remote_protocol.x",
    Dst = "src",
    main([Src, Dst]);
main([Src0, Dst0]) ->
    Dir = filename:dirname(filename:dirname(escript:script_name())) ++ "/",

    Src = filename:absname(Src0, Dir),
    Dst = filename:absname(Dst0, Dir),
    Include = filename:absname("include", Dir),
    Hrl = filename:basename(Src0, ".x") ++ ".hrl",

    case file_exists(Src) of
        true ->
            Path = os:getenv(
                "REBAR_BUILD_DIR",
                filename:join(
                    filename:dirname(escript:script_name()),
                    "../_build/default"
                )
            ),

            true = code:add_pathz(filename:join(Path, "lib/erpcgen/ebin")),

            % modules will not be loaded without rehashing here
            {module, erpcgen} = code:load_file(erpcgen),
            {module, xdr_auth} = code:load_file(xdr_auth),
            {module, xdrgen} = code:load_file(xdrgen),
            {module, xdr_parse} = code:load_file(xdr_parse),
            {module, xdr_scan} = code:load_file(xdr_scan),

            [ok, ok] = generate_xdr(Src, Dst),
            move_hrl(Dst ++ "/" ++ Hrl, Include ++ "/" ++ Hrl);
        false ->
            ok
    end.

file_exists(File) ->
    case file:read_file_info(File) of
        {ok, _} -> true;
        _ -> false
    end.

generate_xdr(Src, Dst) ->
    File = filename:basename(Src, ".x"),

    {ok, Bin} = file:read_file(Src),
    XDR = mangle_file(Bin),

    ok = file:write_file(Dst ++ "/" ++ File ++ ".x", XDR),

    % erpcgen has to be in the output directory
    {ok, Cur} = file:get_cwd(),
    ok = file:set_cwd(Dst),

    Res = erpcgen:file(list_to_atom(File), [xdrlib]),

    ok = file:set_cwd(Cur),
    Res.

% Hacks to get the remote protocol file to compile
mangle_file(Bin) ->
    lists:foldl(
        fun({RE, Swap}, XDR) ->
            Replace = maybe_list(Swap),
            re:replace(XDR, RE, Replace, [global, multiline])
        end,
        Bin,
        % header definitions
        [
            {"^(%.*)$", "/* \\1 */"},
            % chars are transferred as
            {"\\bchar\\b", "int"},
            %  integers (4 bytes)

            % shorts too
            {"\\bshort\\b", "int"}
            | defines()
        ]
    ).

move_hrl(Src, Dst) ->
    io:format("~s -> ~s~n", [Src, Dst]),
    {ok, _} = file:copy(Src, Dst),
    ok = file:delete(Src).

maybe_list(N) when is_list(N) -> N;
maybe_list(N) when is_integer(N) -> integer_to_list(N).
