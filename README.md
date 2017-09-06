Erlang implementation of the libvirtd remote protocol.

For an Erlang binding to the C libvirt interface, see:

<https://github.com/msantos/erlang-libvirt>


## WARNING

remote\_protocol.x contains this warning:

    (1) The protocol is internal and may change at any time, without
    notice.  Do not use it.  Instead link to libvirt and use the remote
    driver.

<http://libvirt.org/git/?p=libvirt.git;a=blob_plain;f=src/remote/remote_protocol.x;hb=HEAD>

However, see the section _GENERATING THE REMOTE PROTOCOL MODULE_ below for
instructions on recompiling the XDR protocol spec if any changes occur.

The RPC protocol is documented here:

<http://libvirt.org/internals/rpc.html>

For the remote support documentation:

<http://libvirt.org/remote.html>

The version of remote\_protocol.x used was taken from libvirt master
at 1.2.18, SHA commit bcfdd8e836edcf64d4dff4d01f4b7903bfe76775).

## HOW TO BUILD IT

    make

See _GENERATING THE REMOTE PROTOCOL MODULE_ to rebuild the XDR protocol
parser.

## CREATING A TEST VM

If you don't have a VM ready to test, you can download a test image
by running:

    bin/get_image.escript

The script will download an OpenWRT image and set up the configuration
in priv/example.xml. By default, it will set up the VM to run under
KVM using user mode networking.

You can manually modify the configuration afterwards or set these
environment variables before running the script:

    VERX_QEMU_BIN : path to the qemu binary (default: /usr/bin/kvm)
    VERX_BRIDGE_INTERFACE : bridge interface (default: user networking)

## TESTING EVERYTHING WORKS

To quickly test everything works, try running `bin/verx`, an escript
that provides a simple command line interface to the verx library.

You'll have to set up the ERL\_LIBS environment variable first, e.g.,
if verx is checked out in ~/src:

    export ERL_LIBS=$ERL_LIBS:~/src

Then run:

    bin/verx

To create the example VM:

    bin/verx create priv/example.xml

To see all the VMs (if you have TLS set up):

    bin/verx list --all --transport verx_client_tls

To connect to example VM's console using the Unix transport:

    bin/verx console localvm # control-C to exit

## HOW TO USE IT

## libvirt documentation

See <http://libvirt.org/html/libvirt-libvirt.html>

### DATA TYPES

    verx_transport()

        Reference to the underlying transport and transport handler.

    unix_socket() = string() | binary()

        Path to Unix socket.

### verx


    verx:Call(Ref) -> ok | {ok, Payload} | {error, Error}
    verx:Call(Ref, Arg) -> ok | {ok, Payload} | {error, Error}

        Types   Call = [connect_open, connect_close, connect_list_domain, ...]
                Ref = verx_transport()
                Arg = [remote_protocol_args()]
                Payload = [remote_protocol_ret()]
                Error = [ posix() | libvirt() ]

    verx has a large number of functions (283). See verx.erl or the
    exports in verx:module_info() for a list.

    Understanding the arguments for a remote protocol call takes some
    work.  For example, for verx:domain_define_xml/2, here are some
    places to look at:

        * check verx.erl for the arity

        * check remote_protocol_xdr.erl for the argument format. The
          parsing function is prefaced with "enc_remote_" and ends with
          "_args":

            enc_remote_domain_define_xml_args/1

        * check the XDR protocol file, remote_protocol.x:

            struct remote_domain_define_xml_args {
                remote_nonnull_string xml;
            };

        * look at the libvirt documentation. Generally the libvirt
          counterpart is camelcased and prefaced with "vir":

            virDomainDefineXML

    Similarly, for the call return values, search for the
    suffix "_ret", e.g., dec_remote_domain_define_xml_ret and
    remote_domain_define_xml_ret.

### verx\_client

    verx_client:start(Opt) -> {ok, Ref} | {error, posix()}

        Types   Opt = [ Options ]
                Options = {transport, Transport}

                      % Unix socket
                    | {path, unix_socket()}

                      % TCP and TLS
                    | {host, ip_address()}
                    | {port, uint16()}

                      % TLS
                    | {cacert, path()}
                    | {cert, path()}
                    | {key, path()}
                    | {depth, integer()}
                    | {password, string()}
                    | {ciphers, ciphers()}

                Transport = verx_client_unix
                    | verx_client_tcp
                    | verx_client_tls

        RPC transport layer, supports Unix sockets, TCP and TLS (IPv4
        and IPV6).

        Options depend on the underlying transport mechanism.

    verx_client:stop(Ref) -> ok

        Closes the transport socket.

    verx_client:recv(Ref) -> {ok, Buf} | {error, posix()}

        Types   Ref = verx_transport()
                Buf = [binary()]

        Returns streamed data. The stream must first be prepared
        by making the appropriate remote protocol call, e.g.,
        verx:domain_snapshot/2.

### verx\_client\_unix

### verx\_client\_tcp

### verx\_client\_tls

### verx\_rpc

## EXAMPLES

### OPEN A CONNECTION TO LIBVIRTD

    % Connect to the libvirtd socket
    {ok, Ref} = verx_client:start(),

    % libvirt remote procotol open message
    % by default to qemu:///system
    ok = verx:connect_open(Ref),

    % send a close message
    ok = verx:connect_close(Ref),

    % send a remote protocol open message
    %  connecting to lxc containers
    ok = verx:connect_open(Ref, ["lxc:///", 0]),

    % close and stop the transport
    ok = verx:connect_close(Ref),
    ok = verx_client:stop(Ref).

    % open a TLS connection on the default port
    CACert = "/tmp/cert/cacert.pem",
    Cert = "/tmp/cert/clientcert.pem",
    Key = "/tmp/cert/clientkey.pem",

    {ok, Ref} = verx_client:start([
            {transport, verx_client_tls},
            {cacert, CACert},
            {cert, Cert},
            {key, Key}
            ]).

### CREATING A DOMAIN

    -module(crvm).
    -export([file/0]).

    file() ->
        file("priv/example.xml").
    file(Path) ->
        % Connect to the libvirtd socket
        {ok, Ref} = verx_client:start(),

        % libvirt remote procotol open message
        ok = verx:connect_open(Ref),

        {ok, XML} = file:read_file(Path),

        % Domain is defined but not running
        {ok, [Domain]} = verx:domain_define_xml(Ref, [XML]),

        % Start the VM
        ok = verx:domain_create(Ref, [Domain]),

        {ok, [Active] = verx:connect_num_of_domains(Ref),
        io:format("Active Domains: ~p~n", [Active]),

        % Send a protocol close message
        ok = verx:connect_close(R),

        % Close the socket
        ok = verx_client:stop(R),

        {ok, Domain}.

To list the VMs:

    -module(lsvm).
    -export([ls/0]).

    ls() ->
        {ok, Ref} = verx_client:start(),
        ok = verx:connect_open(Ref),

        {ok, [NumDef]} = verx:connect_num_of_defined_domains(Ref),

        {ok, [NumRun]} = verx:connect_num_of_domains(Ref),

        {ok, [Shutoff]} = verx:connect_list_defined_domains(Ref, [NumDef]),
        {ok, [Running]} = verx:connect_list_domains(Ref, [NumRun]),

        {ok, [{running, info(Ref, Running)},
                 {shutoff, info(Ref, Shutoff)}]}.

    info(Ref, Domains) ->
        [ begin
            {ok, [{Name, UUID, Id}]} = verx:domain_lookup_by_id(Ref, [N]),
            {Name, [{uuid, UUID}, {id, Id}]}
          end || N <- Domains ].

To shutdown the VM:

    % Get the domain resource
    lookup(Ref, Id) when is_integer(Id) ->
        {ok, [Domain]} = verx:domain_lookup_by_id(Ref, [Id]),
        {ok, Domain};

    lookup(Ref, Name) when is_binary(Name) ->
        {ok, [Domain]} = verx:domain_lookup_by_name(Ref, [Name]),
        {ok, Domain}.

    halt(Ref, Domain) ->
        % shutdown only works if acpid is installed in the VM
        ok = verx:domain_shutdown(R, [Domain]),
        verx:domain_destroy(Ref, [Domain]).

To remove the VM, undefine it:

        verx:domain_undefine(Ref, [Domain])

### SUSPENDING AND RESUMING A DOMAIN

This example provides the Erlang equivalent of a Python script to
manipulate a running domain. The example was taken from:

<http://www.ibm.com/developerworks/linux/library/l-libvirt/>

    -module(ex6).

    -export([start/0, states/2]).

    start() ->
        {ok, Ref} = verx_client:start(),
        ok = verx:connect_open(Ref),

        {ok, [Num]} = verx:connect_num_of_domains(Ref),
        {ok, [Ids]} = verx:connect_list_domains(Ref, [Num]),

        [ states(Ref, Id) || Id <- Ids ],
        ok.

    states(Ref, Id) ->
        {ok, [Domain]} = verx:domain_lookup_by_id(Ref, [Id]),

        % return value of domain_get_info from remote_protocol.x:
        %
        % struct remote_domain_get_info_ret {
        %   unsigned char state;
        %   unsigned hyper maxMem;
        %   unsigned hyper memory;
        %   unsigned short nrVirtCpu;
        %   unsigned hyper cpuTime;
        % };

        io:format("running: ~p~n", [verx:domain_get_info(Ref, [Domain])]),

        ok = verx:domain_suspend(Ref, [Domain]),
        io:format("suspended: ~p~n", [verx:domain_get_info(Ref, [Domain])]),

        ok = verx:domain_resume(Ref, [Domain]),
        io:format("resumed: ~p~n", [verx:domain_get_info(Ref, [Domain])]),

        ok = verx:domain_shutdown(Ref, [Domain]),
        io:format("shutdown: ~p~n", [verx:domain_get_info(Ref, [Domain])]),

        ok = verx:domain_destroy(Ref, [Domain]),
        io:format("destroyed: ~p~n", [verx:domain_get_info(Ref, [Domain])]).


### RETRIEVING HYPERVISOR INFORMATION

Here is some code to retrieve information about the hypervisor,
similar to the example in the Ruby libvirt documentation
(<http://libvirt.org/ruby/examples/node_info.rb>):

    -module(node_info).
    -export([start/0]).

    start() ->
        {ok, Ref} = verx_client:start(),
        ok = verx:connect_open(Ref),

        [ begin
                Reply = case Proc of
                    {Call, Arg} -> verx:Call(Ref, Arg);
                    Call -> verx:Call(Ref)
                end,
                result(Proc, Reply)
          end || Proc <- [
                    node_get_info,
                    {node_get_cells_free_memory, [0, 100]},
                    connect_get_version,
                    connect_get_lib_version,
                    connect_get_hostname,
                    connect_get_uri,
                    node_get_free_memory,
                    node_get_security_model,
                    connect_is_secure,
                    connect_get_capabilities
                    ] ],

        ok = verx:connect_close(Ref),
        verx_client:stop(Ref).

    result(Call, {ok, N}) ->
        error_logger:info_report([{call, Call}] ++ N);
    result(Call, {error, _Error} = N) ->
        error_logger:error_report([{call, Call}] ++ N).

### SYSTEM CONSOLE

The VM system console can be accessed using any of the transports.

    % Connect to libvirtd using the Unix socket
    1> {ok, Ref} = verx_client:start().
    {ok,<0.43.0>}

    % Open a remote protocol session to the Linux containers hypervisor
    2> verx:connect_open(Ref, ["lxc:///", 0]).
    ok

    % Get a domain reference
    3> {ok, [Domain]} = verx:domain_lookup_by_name(Ref, [<<"lxc-1">>]).
    {ok,[{<<"lxc-3">>,
         <<150,162,91,134,54,66,203,130,29,224,244,242,121,45,5,118>>,
           19586}]}

    % Open the console. The arguments are:
    %   Domain
    %   Device name : string() or void (NULL)
    %   Flags : integer()
    4> verx:domain_open_console(Ref, [Domain, void, 0]).

    % Send a message to the console, check the results with
    % flush()

    % Start up Erlang ...
    5> verx_client:send(Ref, [<<"erl\n">>]).

    6> verx_client:send(Ref, [<<"spawn(fun() -> io:format(\"Erlang process in an Erlang VM in a Linux VM in an Erlang process!\") end).\n">>]).

    % Receive the message back from the console
    8> verx_client:recv(Ref).
    {ok,<<"Erlang process in an Erlang VM in a Linux VM in an Erlang process!">>}

### TAKING A SCREENSHOT

An example of using the libvirt stream interface to capture an image of
the VM console:

    -module(ss).
    -export([host/1]).

    host(Name) when is_list(Name) ->
        host(list_to_binary(Name));
    host(Name) when is_binary(Name) ->
        {ok, Ref} = verx_client:start(),
        ok = verx:connect_open(Ref),

        {ok, [Domain]} = verx:domain_lookup_by_name(Ref, [Name]),

        Screen = 0,
        Flags = 0,

        {ok, [Mime]} = verx:domain_screenshot(Ref, [Domain, Screen, Flags]),

        Ext = case Mime of
            <<"image/x-portable-pixmap">> -> <<".ppm">>;
            _ -> <<".screen">>
        end,

        {ok, Buf} = verx_client:recvall(Ref),

        File = <<Name/binary, Ext/binary>>,
        ok = file:write_file(File, Buf),

        {ok, Mime, File}.

## CREATING LINUX CONTAINERS

This example will generate many Linux containers (LXC) attached to a
bridge (br0).

    -module(clxc).
    -export([start/2, start/3, create/2, template/2]).

    start(Prefix, Num) ->
        {ok, Ref} = verx_client:start(),
        ok = verx:connect_open(Ref, ["lxc:///", 0]),
        start(Ref, Prefix, Num).

    start(_Ref, _Prefix, 0) ->
        ok;

    start(Ref, Prefix, Num) ->
        Name = Prefix ++ integer_to_list(Num),

        <<Bytes:3/bytes, _/binary>> = erlang:md5(Name),
        Macaddr = "52:54:00:" ++ string:join([ httpd_util:integer_to_hexlist(N)
            || <<N:8>> <= Bytes ], ":"),

        XML = template(Name, Macaddr),
        ok = create(Ref, XML),

        start(Ref, Prefix, Num-1).

    create(Ref, XML) ->
        {ok, [Domain]} = verx:domain_define_xml(Ref, [XML]),
        verx:domain_create(Ref, [Domain]).

    template(Name, Macaddr) ->
    "<domain type='lxc'>
        <name>" ++ Name ++ "</name>
        <memory>102400</memory>
        <os>
            <type>exe</type>
            <init>/bin/sh</init>
        </os>
        <devices>
            <console type='pty'/>
            <interface type='bridge'>
                <mac address='" ++ Macaddr ++ "'/>
                <source bridge='br0'/>
            </interface>
        </devices>
    </domain>".

## GENERATING THE REMOTE PROTOCOL MODULE

To create the remote\_protocol\_xdr.erl from a remote\_protocol.x file:

1. Copy remote\_protocol.x to priv

2. Run:

        make clean; make

If there are any errors, read through `bin/mk_remote_protocol.escript`.

## VERX CLIENT

`verx` is a simple command line client similar to `virsh`. To use
`verx`, the ERL\_LIBS environment variable must point to the directory
_containing_ the verx repository:

    export ERL_LIBS=$ERL_LIBS:~/src
    export PATH=$PATH:~/src/verx/bin

Running `verx` without any options will return the list of commands.

All `verx` commands can take some options:

    --uri : URI supported by libvirt (default: qemu:///system)
    --transport : (default: verx_client_unix)
        verx_client_unix
        verx_client_tcp
        verx_client_tls

For TCP and TLS transports:

    --host : hostname
    --port : port

For the TLS transport:

    --cacert : path to CA cert (default: /etc/pki/CA/cacert.pem)
    --cert : path to client cert (default: /etc/pki/libvirt/clientcert.pem)
    --depth : cert validation depth (default: 1)

Examples:

    # List all defined Qemu/KVM instances through the libvirtd Unix socket
    verx list --all

    # List running LXC instances
    verx list --uri=lxc:///

    # Dump the configuration of a KVM using the TLS transport over IPv6
    verx dumpxml myvm --transport verx_client_tls --host ::1

    # Access the console of a container over TLS/IPv6
    # Use ctl-C to exit
    verx console mylxc --uri lxc:/// --transport verx_client_tls --host ::1


## TODO

* verx\_client\_tls
    * single byte received before packet (works if thrown away)
