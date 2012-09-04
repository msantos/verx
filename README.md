(almost) pure Erlang implementation of the libvirtd remote protocol.

For an Erlang binding to the C libvirt interface, see:

<https://github.com/msantos/erlang-libvirt>


## WARNING

remote\_protocol.x contains this warning:

    (1) The protocol is internal and may change at any time, without
    notice.  Do not use it.  Instead link to libvirt and use the remote
    driver.

<http://libvirt.org/git/?p=libvirt.git;a=blob_plain;f=src/remote/remote_protocol.x;hb=HEAD>

However, see the section "GENERATING THE REMOTE PROTOCOL MODULE" below for
instructions on recompiling the XDR protocol spec if any changes occur.

The RPC protocol is documented here:

<http://libvirt.org/internals/rpc.html>

For the remote support documentation:

<http://libvirt.org/remote.html>

The version of remote\_protocol.x used was taken from libvirt master
(around v0.9.9-rc2, SHA commit ca5c99aecbecc832ed1a5bc630c7a3b8e13f4344).

## HOW TO BUILD IT

    make

See "GENERATING THE REMOTE PROTOCOL MODULE" to rebuild the XDR protocol
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

## HOW TO USE IT

## libvirt documentation

See <http://libvirt.org/html/libvirt-libvirt.html>

### verx

    verx:Call(Ref) -> ok | {ok, Payload} | {error, Error}
    verx:Call(Ref, Arg) -> ok | {ok, Payload} | {error, Error}

        Types   Call = [open, close, list_domain, ...]
                Ref = pid()
                Arg = [remote_protocol_args()]
                Payload = [remote_protocol_ret()]
                Error = [ posix() | libvirt() ]

    verx has a large number of functions (283). See verx.erl or the
    exports in verx:module_info() for a list.

    The Ref is pid of the verx_client.

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

    verx_client:start(Ref) -> {ok, Ref} | {error, posix()}
    verx_client:stop(Ref) -> ok

        RPC transport layer, currently only supports Unix sockets.

### verx\_rpc


## EXAMPLES

### OPEN A CONNECTION TO LIBVIRTD

    % Connect to the libvirtd socket
    {ok, Ref} = verx_client:start(),

    % libvirt remote procotol open message
    % by default to qemu:///system
    ok = verx:open(Ref),

    % send a close message
    ok = verx:close(Ref),

    % send a remote protocol open message
    %  connecting to lxc containers
    ok = verx:open(Ref, ["lxc:///", 0]),

    % close and stop the transport
    ok = verx:close(Ref),
    ok = verx_client:stop(Ref).

### CREATING A DOMAIN

    -module(crvm).
    -export([file/0]).

    file() ->
        file("priv/example.xml").
    file(Path) ->
        % Connect to the libvirtd socket
        {ok, Ref} = verx_client:start(),

        % libvirt remote procotol open message
        ok = verx:open(Ref),

        {ok, XML} = file:read_file(Path),

        % Domain is defined but not running
        {ok, [Domain]} = verx:domain_define_xml(Ref, [XML]),

        % Start the VM
        ok = verx:domain_create(Ref, [Domain]),

        {ok, [Active] = verx:num_of_domains(Ref),
        io:format("Active Domains: ~p~n", [Active]),

        % Send a protocol close message
        ok = verx:close(R),

        % Close the socket
        ok = verx_client:stop(R),

        {ok, Domain}.

To list the VMs:

    -module(lsvm).
    -export([ls/0]).

    ls() ->
        {ok, Ref} = verx_client:start(),
        ok = verx:open(Ref),

        {ok, [NumDef]} = verx:num_of_defined_domains(Ref),

        {ok, [NumRun]} = verx:num_of_domains(Ref),

        {ok, [Shutoff]} = verx:list_defined_domains(Ref, [NumDef]),
        {ok, [Running]} = verx:list_domains(Ref, [NumRun]),

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

This example is the Erlang equivalent of a Python script to manipulate a
running domain. The example is taken from:

<http://www.ibm.com/developerworks/linux/library/l-libvirt/>

    -module(ex6).

    -export([start/0, states/2]).

    start() ->
        {ok, Ref} = verx_client:start(),
        ok = verx:open(Ref),

        {ok, [Num]} = verx:num_of_domains(Ref),
        {ok, [Ids]} = verx:list_domains(Ref, [Num]),

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
        ok = verx:open(Ref),

        [ begin
                Reply = case Proc of
                    {Call, Arg} -> verx:Call(Ref, Arg);
                    Call -> verx:Call(Ref)
                end,
                result(Proc, Reply)
          end || Proc <- [
                    node_get_info,
                    {node_get_cells_free_memory, [0, 100]},
                    get_version,
                    get_lib_version,
                    get_hostname,
                    get_uri,
                    node_get_free_memory,
                    node_get_security_model,
                    is_secure,
                    get_capabilities
                    ] ],

        ok = verx:close(Ref),
        verx_client:stop(Ref).

    result(Call, {ok, N}) ->
        error_logger:info_report([{call, Call}] ++ N);
    result(Call, {error, _Error} = N) ->
        error_logger:error_report([{call, Call}] ++ N).


## GENERATING THE REMOTE PROTOCOL MODULE

To create the remote\_protocol\_xdr.erl from a remote\_protocol.x file:

1. Copy remote\_protocol.x to priv

2. Run: make clean; make

If there are any errors, read through `bin/gen_remote_protocol.escript`.

## TODO

* fix broken include paths for bin/verx, include/verx.hrl

* transport protocols
    * TCP
    * SSL
    * SSH

* increment the serial number in the message header

* continue status
    * upload stream
    * bidirectional stream
    * overlapping
    * passed fd
