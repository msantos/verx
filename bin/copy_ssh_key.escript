#!/usr/bin/env escript
%%! -pa ebin

%%%
%%% Copy an SSH key onto the test VM
%%%

main([]) ->
    main([["localvm"]]);

main(Hosts) ->
    true = code:add_pathz(filename:dirname(escript:script_name())
            ++ "/../deps/procket/ebin"),

    Dir = os:getenv("HOME") ++ "/.ssh/",

    Path = hd([ Dir ++ N || N <- ["id_rsa.pub", "id_dsa.pub"],
        ok == element(1, file:read_file_info(Dir ++ N)) ]),

    {ok, Pubkey} = file:read_file(Path),
    [ upload(Host, Pubkey) || Host <- Hosts ],

    % open the firewall
    [ firewall(Host) || Host <- Hosts ].


upload(Host, Pubkey) ->
    % Chunk the key in 128 byte chunks, the console only accepts
    % 506 bytes on the command line

    Chunks = chunk(Pubkey),

    Copy = [ "printf '" ++ binary_to_list(N) ++ "' >> /etc/dropbear/authorized_keys"
                || N <- Chunks ],

    Cmd = [ "", "> /etc/dropbear/authorized_keys" | Copy],

    send(Host, Cmd).

firewall(Host) ->
    Cmd = [
        "uci batch<<-EOF",
        "set dropbear.@dropbear[-1].PasswordAuth=0",
        "uci set dropbear.@dropbear[-1].RootPasswordAuth=0",
        "commit dropbear",
        "add firewall rule",
        "set firewall.@rule[-1].src=wan",
        "set firewall.@rule[-1].dest_port=22",
        "set firewall.@rule[-1].target=ACCEPT",
        "set firewall.@rule[-1].proto=tcp",
        "commit network",
        "EOF",
        "/etc/init.d/dropbear restart",
        "/etc/init.d/firewall restart"
        ],

    send(Host, Cmd).

send(Host, Cmd) ->
    {ok, Ref} = console(Host),
    verx_client:send(Ref, [ list_to_binary([C, "\n"]) || C <- Cmd ]),
    verx:close(Ref),
    verx_client:stop(Ref).

chunk(Bin) ->
    chunk(Bin, []).
chunk(Bin, Chunks) when byte_size(Bin) < 128 ->
    lists:reverse([Bin|Chunks]);
chunk(<<Bin:128/bytes, Rest/binary>>, Chunks) ->
    chunk(Rest, [Bin|Chunks]).

console(Host) ->
    {ok, Ref} = verx_client:start(),
    ok = verx:open(Ref),
    {ok, [Domain]} = verx:lookup(Ref, {domain, Host}),
    ok = verx:domain_open_console(Ref, [Domain, void, 0]),
    {ok, Ref}.
