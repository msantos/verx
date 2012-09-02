#!/usr/bin/env escript
%%! -pa ebin

%%%
%%% Bring up a single interface for the OpenWRT VM
%%%

main([]) ->
    main([["testvm"]]);
main(Hosts) ->
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
    {ok, Ref} = vert_console:open(Host),
    lists:foreach(fun(C) ->
            error_logger:info_report([{cmd, Host, C}]),
            ok = vert_console:send(Ref, C)
            end,
            Cmd),
    vert_console:close(Ref).
