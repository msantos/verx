%% Copyright (c) 2011, Michael Santos <michael.santos@gmail.com>
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
-module(verx_ret).
-export([decode/2]).
-include("verx.hrl").

decode(supports_feature, Buf) ->
    struct(Buf, [
         {supported, int}
        ]);
decode(get_type, Buf) ->
    struct(Buf, [
         {type, remote_nonnull_string}
        ]);
decode(get_version, Buf) ->
    struct(Buf, [
         {hv_ver, hyper}
        ]);
decode(get_lib_version, Buf) ->
    struct(Buf, [
         {lib_ver, hyper}
        ]);
decode(get_hostname, Buf) ->
    struct(Buf, [
         {hostname, remote_nonnull_string}
        ]);
decode(get_uri, Buf) ->
    struct(Buf, [
         {uri, remote_nonnull_string}
        ]);
decode(get_max_vcpus, Buf) ->
    struct(Buf, [
         {max_vcpus, int}
        ]);
decode(node_get_info, Buf) ->
    struct(Buf, [
         {model, {char, 32}},
         {memory, hyper},
         {cpus, int},
         {mhz, int},
         {nodes, int},
         {sockets, int},
         {cores, int},
         {threads, int}
        ]);
decode(get_capabilities, Buf) ->
    struct(Buf, [
         {capabilities, remote_nonnull_string}
        ]);
decode(node_get_cells_free_memory, Buf) ->
    struct(Buf, [
         {freeMems, hyper}
        ]);
decode(node_get_free_memory, Buf) ->
    struct(Buf, [
         {freeMem, hyper}
        ]);
decode(domain_get_scheduler_type, Buf) ->
    struct(Buf, [
         {type, remote_nonnull_string},
         {nparams, int}
        ]);
decode(domain_get_scheduler_parameters, Buf) ->
    struct(Buf, [
         {params, remote_sched_param}
        ]);
decode(domain_block_stats, Buf) ->
    struct(Buf, [
         {rd_req, hyper},
         {rd_bytes, hyper},
         {wr_req, hyper},
         {wr_bytes, hyper},
         {errs, hyper}
        ]);
decode(domain_interface_stats, Buf) ->
    struct(Buf, [
         {rx_bytes, hyper},
         {rx_packets, hyper},
         {rx_errs, hyper},
         {rx_drop, hyper},
         {tx_bytes, hyper},
         {tx_packets, hyper},
         {tx_errs, hyper},
         {tx_drop, hyper}
        ]);
decode(domain_memory_stats, Buf) ->
    struct(Buf, [
         {stats, remote_domain_memory_stat}
        ]);
decode(domain_block_peek, Buf) ->
    struct(Buf, [
         {buffer, opaque}
        ]);
decode(domain_memory_peek, Buf) ->
    struct(Buf, [
         {buffer, opaque}
        ]);
decode(list_domains, Buf) ->
    struct(Buf, [
         {ids, int}
        ]);
decode(num_of_domains, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(domain_create_xml, Buf) ->
    struct(Buf, [
         {dom, remote_nonnull_domain}
        ]);
decode(domain_lookup_by_id, Buf) ->
    struct(Buf, [
         {dom, remote_nonnull_domain}
        ]);
decode(domain_lookup_by_uuid, Buf) ->
    struct(Buf, [
         {dom, remote_nonnull_domain}
        ]);
decode(domain_lookup_by_name, Buf) ->
    struct(Buf, [
         {dom, remote_nonnull_domain}
        ]);
decode(domain_get_os_type, Buf) ->
    struct(Buf, [
         {type, remote_nonnull_string}
        ]);
decode(domain_get_max_memory, Buf) ->
    struct(Buf, [
         {memory, uhyper}
        ]);
decode(domain_get_info, Buf) ->
    struct(Buf, [
         {state, {uchar, 1}},
         {max_mem, uhyper},
         {memory, uhyper},
         {nr_virt_cpu, ushort},
         {cpu_time, uhyper}
        ]);
decode(domain_dump_xml, Buf) ->
    struct(Buf, [
         {xml, remote_nonnull_string}
        ]);
decode(domain_migrate_prepare, Buf) ->
    struct(Buf, [
         {cookie, opaque},
         {uri_out, remote_string}
        ]);
decode(domain_migrate_finish, Buf) ->
    struct(Buf, [
         {ddom, remote_nonnull_domain}
        ]);
decode(list_defined_domains, Buf) ->
    struct(Buf, [
         {names, remote_nonnull_string}
        ]);
decode(num_of_defined_domains, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(domain_define_xml, Buf) ->
    struct(Buf, [
         {dom, remote_nonnull_domain}
        ]);
decode(domain_get_vcpus, Buf) ->
    struct(Buf, [
         {info, remote_vcpu_info},
         {cpumaps, opaque}
        ]);
decode(domain_get_max_vcpus, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(domain_get_security_label, Buf) ->
    struct(Buf, [
         {label, char},
         {enforcing, int}
        ]);
decode(node_get_security_model, Buf) ->
    struct(Buf, [
         {model, char},
         {doi, char}
        ]);
decode(domain_get_autostart, Buf) ->
    struct(Buf, [
         {autostart, int}
        ]);
decode(num_of_networks, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(list_networks, Buf) ->
    struct(Buf, [
         {names, remote_nonnull_string}
        ]);
decode(num_of_defined_networks, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(list_defined_networks, Buf) ->
    struct(Buf, [
         {names, remote_nonnull_string}
        ]);
decode(network_lookup_by_uuid, Buf) ->
    struct(Buf, [
         {net, remote_nonnull_network}
        ]);
decode(network_lookup_by_name, Buf) ->
    struct(Buf, [
         {net, remote_nonnull_network}
        ]);
decode(network_create_xml, Buf) ->
    struct(Buf, [
         {net, remote_nonnull_network}
        ]);
decode(network_define_xml, Buf) ->
    struct(Buf, [
         {net, remote_nonnull_network}
        ]);
decode(network_dump_xml, Buf) ->
    struct(Buf, [
         {xml, remote_nonnull_string}
        ]);
decode(network_get_bridge_name, Buf) ->
    struct(Buf, [
         {name, remote_nonnull_string}
        ]);
decode(network_get_autostart, Buf) ->
    struct(Buf, [
         {autostart, int}
        ]);
decode(num_of_interfaces, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(list_interfaces, Buf) ->
    struct(Buf, [
         {names, remote_nonnull_string}
        ]);
decode(num_of_defined_interfaces, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(list_defined_interfaces, Buf) ->
    struct(Buf, [
         {names, remote_nonnull_string}
        ]);
decode(interface_lookup_by_name, Buf) ->
    struct(Buf, [
         {iface, remote_nonnull_interface}
        ]);
decode(interface_lookup_by_mac_string, Buf) ->
    struct(Buf, [
         {iface, remote_nonnull_interface}
        ]);
decode(interface_get_xml_desc, Buf) ->
    struct(Buf, [
         {xml, remote_nonnull_string}
        ]);
decode(interface_define_xml, Buf) ->
    struct(Buf, [
         {iface, remote_nonnull_interface}
        ]);
decode(auth_list, Buf) ->
    struct(Buf, [
         {types, remote_auth_type}
        ]);
decode(auth_sasl_init, Buf) ->
    struct(Buf, [
         {mechlist, remote_nonnull_string}
        ]);
decode(auth_sasl_start, Buf) ->
    struct(Buf, [
         {complete, int},
         {nil, int},
         {data, char}
        ]);
decode(auth_sasl_step, Buf) ->
    struct(Buf, [
         {complete, int},
         {nil, int},
         {data, char}
        ]);
decode(auth_polkit, Buf) ->
    struct(Buf, [
         {complete, int}
        ]);
decode(num_of_storage_pools, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(list_storage_pools, Buf) ->
    struct(Buf, [
         {names, remote_nonnull_string}
        ]);
decode(num_of_defined_storage_pools, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(list_defined_storage_pools, Buf) ->
    struct(Buf, [
         {names, remote_nonnull_string}
        ]);
decode(find_storage_pool_sources, Buf) ->
    struct(Buf, [
         {xml, remote_nonnull_string}
        ]);
decode(storage_pool_lookup_by_uuid, Buf) ->
    struct(Buf, [
         {pool, remote_nonnull_storage_pool}
        ]);
decode(storage_pool_lookup_by_name, Buf) ->
    struct(Buf, [
         {pool, remote_nonnull_storage_pool}
        ]);
decode(storage_pool_lookup_by_volume, Buf) ->
    struct(Buf, [
         {pool, remote_nonnull_storage_pool}
        ]);
decode(storage_pool_create_xml, Buf) ->
    struct(Buf, [
         {pool, remote_nonnull_storage_pool}
        ]);
decode(storage_pool_define_xml, Buf) ->
    struct(Buf, [
         {pool, remote_nonnull_storage_pool}
        ]);
decode(storage_pool_dump_xml, Buf) ->
    struct(Buf, [
         {xml, remote_nonnull_string}
        ]);
decode(storage_pool_get_info, Buf) ->
    struct(Buf, [
         {state, {uchar, 1}},
         {capacity, uhyper},
         {allocation, uhyper},
         {available, uhyper}
        ]);
decode(storage_pool_get_autostart, Buf) ->
    struct(Buf, [
         {autostart, int}
        ]);
decode(storage_pool_num_of_volumes, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(storage_pool_list_volumes, Buf) ->
    struct(Buf, [
         {names, remote_nonnull_string}
        ]);
decode(storage_vol_lookup_by_name, Buf) ->
    struct(Buf, [
         {vol, remote_nonnull_storage_vol}
        ]);
decode(storage_vol_lookup_by_key, Buf) ->
    struct(Buf, [
         {vol, remote_nonnull_storage_vol}
        ]);
decode(storage_vol_lookup_by_path, Buf) ->
    struct(Buf, [
         {vol, remote_nonnull_storage_vol}
        ]);
decode(storage_vol_create_xml, Buf) ->
    struct(Buf, [
         {vol, remote_nonnull_storage_vol}
        ]);
decode(storage_vol_create_xml_from, Buf) ->
    struct(Buf, [
         {vol, remote_nonnull_storage_vol}
        ]);
decode(storage_vol_dump_xml, Buf) ->
    struct(Buf, [
         {xml, remote_nonnull_string}
        ]);
decode(storage_vol_get_info, Buf) ->
    struct(Buf, [
         {type, {char, 1}},
         {capacity, uhyper},
         {allocation, uhyper}
        ]);
decode(storage_vol_get_path, Buf) ->
    struct(Buf, [
         {name, remote_nonnull_string}
        ]);
decode(node_num_of_devices, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(node_list_devices, Buf) ->
    struct(Buf, [
         {names, remote_nonnull_string}
        ]);
decode(node_device_lookup_by_name, Buf) ->
    struct(Buf, [
         {dev, remote_nonnull_node_device}
        ]);
decode(node_device_dump_xml, Buf) ->
    struct(Buf, [
         {xml, remote_nonnull_string}
        ]);
decode(node_device_get_parent, Buf) ->
    struct(Buf, [
         {parent, remote_string}
        ]);
decode(node_device_num_of_caps, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(node_device_list_caps, Buf) ->
    struct(Buf, [
         {names, remote_nonnull_string}
        ]);
decode(node_device_create_xml, Buf) ->
    struct(Buf, [
         {dev, remote_nonnull_node_device}
        ]);
decode(domain_events_register, Buf) ->
    struct(Buf, [
         {cb_registered, int}
        ]);
decode(domain_events_deregister, Buf) ->
    struct(Buf, [
         {cb_registered, int}
        ]);
decode(domain_xml_from_native, Buf) ->
    struct(Buf, [
         {domainXml, remote_nonnull_string}
        ]);
decode(domain_xml_to_native, Buf) ->
    struct(Buf, [
         {nativeConfig, remote_nonnull_string}
        ]);
decode(num_of_secrets, Buf) ->
    struct(Buf, [
         {num, int}
        ]);
decode(list_secrets, Buf) ->
    struct(Buf, [
         {uuids, remote_nonnull_string}
        ]);
decode(secret_lookup_by_uuid, Buf) ->
    struct(Buf, [
         {secret, remote_nonnull_secret}
        ]);
decode(secret_define_xml, Buf) ->
    struct(Buf, [
         {secret, remote_nonnull_secret}
        ]);
decode(secret_get_xml_desc, Buf) ->
    struct(Buf, [
         {xml, remote_nonnull_string}
        ]);
decode(secret_get_value, Buf) ->
    struct(Buf, [
         {value, opaque}
        ]);
decode(secret_lookup_by_usage, Buf) ->
    struct(Buf, [
         {secret, remote_nonnull_secret}
        ]);
decode(is_secure, Buf) ->
    struct(Buf, [
         {secure, int}
        ]);
decode(domain_is_active, Buf) ->
    struct(Buf, [
         {active, int}
        ]);
decode(domain_is_persistent, Buf) ->
    struct(Buf, [
         {persistent, int}
        ]);
decode(network_is_active, Buf) ->
    struct(Buf, [
         {active, int}
        ]);
decode(network_is_persistent, Buf) ->
    struct(Buf, [
         {persistent, int}
        ]);
decode(storage_pool_is_active, Buf) ->
    struct(Buf, [
         {active, int}
        ]);
decode(storage_pool_is_persistent, Buf) ->
    struct(Buf, [
         {persistent, int}
        ]);
decode(interface_is_active, Buf) ->
    struct(Buf, [
         {active, int}
        ]);
decode(cpu_compare, Buf) ->
    struct(Buf, [
         {result, int}
        ]);

decode(_, _) ->
    {error, unsupported}.


struct(Buf, Struct) ->
    {Values, _Remainder} = verx_xdr:struct(Buf, Struct),
    Values.

