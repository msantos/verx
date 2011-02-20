#!/usr/bin/env escript

%%%
%%% Download a test OS image and create the configuration
%%%
main(_) ->
    inets:start(),
    ssl:start(),

    URI = "https://github.com/downloads/msantos/evum/buildroot-x86-ext2.fs.bz2",
    File = "priv/" ++ filename:rootname(filename:basename(URI)), 

    Cfg = "priv/example.xml",

    download(URI, File),
    config(Cfg).

download(URI, File) ->
    io:format("Checking download test image ...~n"),
    case file:read_file_info(File) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            io:format("Downloading image: ~p -> ~p~n", [URI, File]),
            {ok, _} = httpc:request(get, {URI, []}, [], [{stream, File}]),
            os:cmd("bzip2 -d " ++ File ++ ".bz2")
    end.

config(File) ->
    io:format("Checking configuration...~n"),
    case file:read_file_info(File) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            Dist = File ++ ".dist",

            io:format("Creating config: ~p -> ~p~n", [Dist, File]),
            {ok, Dir} = file:get_cwd(),
            {ok, Bin} = file:read_file(Dist),
            ok = file:write_file(File, re:replace(Bin, "@PATH@", Dir, [{return, binary}]))
    end.


