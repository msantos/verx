(almost) pure Erlang implementation of the libvirtd remote protocol.

For an Erlang binding to the C libvirt interface, see:

<https://github.com/msantos/erlang-libvirt>

## WARNING

## HOW TO BUILD IT

make

## HOW TO USE IT

## EXAMPLES

### CREATING A DOMAIN

To download a test image, run:

    escript bin/get_image.escript

verx has some convenience functions that wrap call/2 and call/3. This
function will start a VM using defaults:

    start() ->
        {ok, Ref} = verx:start(),

        {ok, Domain} = verx:create(Ref),

        Dom = proplists:get_value(dom, Domain),
        UUID = proplists:get_value(uuid, Dom),

        Active = verx:list_domains(Ref),
        io:format("Active Domains: ~p~n", [Active]),

        {Ref, UUID}.

Or directly using call/3:

    start(Path) ->
        {ok, Ref} = verx:start(),

        {ok, Cfg} = file:read_file(Path),

        % for the arguments, see remote_protocol.x or the
        % output from: verx_args:param(domain_create_xml)

        {ok, Domain} = verx:call(Ref, domain_create_xml, [
                {remote_nonnull_string, Cfg},   % XML
                {int, 0}                        % Flags
            ]),

        Dom = proplists:get_value(dom, Domain),
        UUID = proplists:get_value(uuid, Dom),

        Active = verx:call(Ref, list_domains, [
                {int, 10}                       % Max domains to return
            ]),
        io:format("Active Domains: ~p~n", [Active]),

        {Ref, UUID}.

To shutdown the VM:

    halt(Ref, UUID) ->
        verx:destroy(Ref, UUID),
        verx:stop(Ref).


### SUSPENDING AND RESUMING A DOMAIN

### RETRIEVING HYPERVISOR INFORMATION

Here is some code to retrieve information about the hypervisor,
similar to the example in the Ruby libvirt documentation
(<http://libvirt.org/ruby/examples/node_info.rb>):

    -module(node_info).
    -compile(export_all).
    
    -include("verx.hrl").
    
    
    start() ->
        {ok, Ref} = verx:start(),
        
        [ result(N, verx:call(Ref, N)) || N <- [
            node_get_info,
            node_get_cells_free_memory,
            get_version,
            get_lib_version,
            get_hostname,
            get_uri,
            node_get_free_memory,
            node_get_security_model,
            is_secure,
            get_capabilities
        ] ],
    
        verx:stop(Ref).
    
    result(Op, {ok, N}) ->
        error_logger:info_report([{op, Op}] ++ N);
    result(Op, {error, Error}) ->
        N = case is_list(Error) of
            true -> Error;
            false -> [{error, Error}]
        end,
        error_logger:info_report([{op, Op}] ++ N).

## TODO

