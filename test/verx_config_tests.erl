%% Copyright (c) 2013-2021, Michael Santos <michael.santos@gmail.com>
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
-module(verx_config_tests).

-compile(nowarn_export_all).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

to_xml_test() ->
    Cfg = verx_config:init([{type, ["lxc"]}]),
    verx_config:to_xml(Cfg),
    ok.

name_test() ->
    Cfg0 = verx_config:init([{type, ["qemu"]}]),
    Name = verx_config:set(name, "test"),
    {name, ["test"]} = Name,
    Cfg = verx_config:set(name, "test", Cfg0),
    XML = verx_config:to_xml(Cfg),
    ?assertEqual(
        "<domain type=\"qemu\"><name>test</name></domain>",
        XML
    ).

interface_entry_test() ->
    Cfg0 = verx_config:init([{type, ["qemu"]}]),
    Cfg = verx_config:append(
        [devices, {interface, [{type, "bridge"}]}],
        [
            {mac, [{address, ["00:11:22:33:44:55"]}], []},
            {source, [{bridge, "eth0"}], []}
        ],
        Cfg0
    ),
    XML = verx_config:to_xml(Cfg),
    ?assertEqual(
        "<domain type=\"qemu\"><devices>"
        "<interface type=\"bridge\"><mac address=\"00:11:22:33:44:55\"/>"
        "<source bridge=\"eth0\"/></interface>"
        "</devices></domain>",
        XML
    ).

interface_list_test() ->
    Cfg0 = verx_config:init(),
    Iface1 = [
        {mac, [{address, ["00:11:22:33:44:66"]}], []},
        {source, [{bridge, ["eth1"]}], []}
    ],
    Iface2 = [
        {mac, [{address, ["00:11:22:33:44:55"]}], []},
        {source, [{bridge, ["eth0"]}], []}
    ],
    Cfg1 = verx_config:append([devices, {interface, [{type, "bridge"}]}], Iface1, Cfg0),
    Cfg2 = verx_config:append([devices, {interface, [{type, "bridge"}]}], Iface2, Cfg1),
    XML = verx_config:to_xml(Cfg2),
    ?assertEqual(
        "<domain><devices>"
        "<interface type=\"bridge\"><mac address=\"00:11:22:33:44:55\"/>"
        "<source bridge=\"eth0\"/></interface><interface type=\"bridge\">"
        "<mac address=\"00:11:22:33:44:66\"/><source bridge=\"eth1\"/></interface>"
        "</devices></domain>",
        XML
    ).

filesystem_entry_test() ->
    Cfg0 = verx_config:init(),
    Cfg = verx_config:append(
        [devices, {filesystem, [{type, "mount"}]}],
        [
            {source, [{dir, ["/bin"]}], []},
            {target, [{dir, ["/bin"]}], []},
            readonly
        ],
        Cfg0
    ),
    XML = verx_config:to_xml(Cfg),
    ?assertEqual(
        "<domain><devices>"
        "<filesystem type=\"mount\"><source dir=\"/bin\"/>"
        "<target dir=\"/bin\"/><readonly/></filesystem>"
        "</devices></domain>",
        XML
    ).

filesystem_list_test() ->
    Cfg0 = verx_config:init(),
    FS1 = [
        {source, [{dir, ["/tmp/user"]}], []},
        {target, [{dir, ["/home/user"]}], []}
    ],
    FS2 = [
        {source, [{dir, ["/bin"]}], []},
        {target, [{dir, ["/bin"]}], []},
        readonly
    ],
    Cfg1 = verx_config:append([devices, {filesystem, [{type, "mount"}]}], FS1, Cfg0),
    Cfg2 = verx_config:append([devices, {filesystem, [{type, "mount"}]}], FS2, Cfg1),
    XML = verx_config:to_xml(Cfg2),
    ?assertEqual(
        "<domain><devices>"
        "<filesystem type=\"mount\"><source dir=\"/bin\"/>"
        "<target dir=\"/bin\"/><readonly/></filesystem>"
        "<filesystem type=\"mount\">"
        "<source dir=\"/tmp/user\"/><target dir=\"/home/user\"/></filesystem>"
        "</devices></domain>",
        XML
    ).
