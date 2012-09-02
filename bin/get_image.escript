#!/usr/bin/env escript

%%%
%%% Download a test OS image and create the configuration
%%%

%% Environment Variables
%%
%% VERT_QEMU_BIN : path to the qemu binary (default: /usr/bin/kvm)
%% VERT_BRIDGE_INTERFACE : bridge interface (default: user networking)

main(_) ->
    inets:start(),
    ssl:start(),

    URI = "http://downloads.openwrt.org/backfire/10.03.1/x86_generic/openwrt-x86-generic-combined-ext2.img.gz",
    File = "priv/" ++ filename:rootname(filename:basename(URI)),

    Cfg = "priv/example.xml",

    download(URI, File),
    config(Cfg, File).

download(URI, File) ->
    io:format("Checking download test image ...~n"),
    case file:read_file_info(File) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            io:format("Downloading image: ~p -> ~p~n", [URI, File]),
            {ok, _} = httpc:request(get, {URI, []}, [], [{stream, File}]),
            os:cmd("gunzip " ++ File ++ ".gz")
    end.

config(Cfg, File) ->
    io:format("Checking configuration...~n"),
    case file:read_file_info(Cfg) of
        {ok, _} ->
            ok;
        {error, enoent} ->
            Dist = Cfg ++ ".dist",

            io:format("Creating config: ~p -> ~p~n", [Dist, Cfg]),
            {ok, Dir} = file:get_cwd(),
            {ok, Bin} = file:read_file(Dist),

            Qemu =  get_qemu_path(),
            Interface =  get_interface(),

            Bin1 = lists:foldl(fun({Replace, With}, Buf) ->
                    io:format("~s: ~s = ~s~n", [Cfg, Replace, With]),
                    re:replace(Buf, Replace, With, [{return, binary}])
                end,
                Bin,
                [
                    {"@PATH@", Dir ++ "/" ++ File},
                    {"@QEMU@", Qemu},
                    {"@INTERFACE@", Interface}
                 ]),

            ok = file:write_file(Cfg, Bin1)
    end.

get_qemu_path() ->
    Qemu = os:getenv("VERT_QEMU_BIN"),

    case Qemu of
        false -> "/usr/bin/kvm";
        _ -> Qemu
    end.

get_interface() ->
    Bridge = os:getenv("VERT_BRIDGE_INTERFACE"),
    interface(Bridge).

interface(false) ->
    io_lib:format(
            "<interface type='user'>
                <mac address='~s'/>
            </interface>", [macaddr()]
            );
interface(Bridge) ->
    io_lib:format(
            "<interface type='bridge'>
                <mac address='~s'/>
                <source bridge='~s'/>
                <model type='e1000'/>
                <address type='pci' domain='0x0000' bus='0x00' slot='0x03' function='0x0'/>
            </interface>", [macaddr(), Bridge]
    ).

macaddr() ->
    "52:54:00:" ++ lists:flatten(string:join(
                [ io_lib:format("~.16B", [N]) || <<N>> <= crypto:rand_bytes(3) ],
                ":")).
