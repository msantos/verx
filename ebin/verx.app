{application, verx,
    [
    {description, "Erlang libvirtd remote protocol"},
    {vsn, "0.01"},
    {modules, [
        verx,
        verx_srv,
        verx_rpc,
        verx_xdr,
        verx_args,
        verx_ret,
        verx_constant,
        verx_util,
        verx_err
            ]},
    {registered, []},
    {applications, [
        kernel,
        stdlib
            ]},
    {env, []}
    ]}.

