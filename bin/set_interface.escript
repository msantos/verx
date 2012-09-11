#!/usr/bin/env escript
%%! -pa ebin

%%%
%%% Bring up a single interface for the OpenWRT VM
%%%

main([]) ->
    main([["localvm"]]);
main(Hosts) ->
    true = code:add_pathz(filename:dirname(escript:script_name())
            ++ "/../deps/procket/ebin"),

    Commands = [
        "uci batch <<-EOF",
        "delete network.lan",
        "set network.wan=interface",
        "set network.wan.ifname=eth0",
        "set network.wan.proto=dhcp",
        "commit network",
        "EOF"
    ],
    [ send(Host, Commands) || Host <- Hosts ].

send(Host, Cmd) ->
    {ok, Ref} = console(Host),
    ok = verx_client:send(Ref, [ list_to_binary([C, "\n"]) || C <- Cmd ]),
    ok = verx:close(Ref),
    verx_client:stop(Ref).

console(Host) ->
    {ok, Ref} = verx_client:start(),
    ok = verx:open(Ref),
    {ok, [Domain]} = verx:lookup(Ref, {domain, Host}),
    ok = verx:domain_open_console(Ref, [Domain, void, 0]),
    {ok, Ref}.
