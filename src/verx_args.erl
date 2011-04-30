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
-module(verx_args).
-export([encode/2, param/1, api/0]).
-include("verx.hrl").

encode(Proc, Struct) ->
    case param(Proc) of
        {error, _} = Err ->
            Err;
        Args ->
            verx_xdr:arg(Struct, Args)
    end.

param(open) ->
    [
       {name, remote_string},
       {flags, int}
    ];

param(supports_feature) ->
    [
       {feature, int}
    ];

param(get_max_vcpus) ->
    [
       {type, remote_string}
    ];

param(node_get_cells_free_memory) ->
    [
       {startCell, int},
       {maxCells, int}
    ];

param(domain_get_scheduler_type) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_get_scheduler_parameters) ->
    [
       {dom, remote_nonnull_domain},
       {nparams, int}
    ];

param(domain_set_scheduler_parameters) ->
    [
       {dom, remote_nonnull_domain},
       {params, remote_sched_param}
    ];

param(domain_block_stats) ->
    [
       {dom, remote_nonnull_domain},
       {path, remote_nonnull_string}
    ];

param(domain_interface_stats) ->
    [
       {dom, remote_nonnull_domain},
       {path, remote_nonnull_string}
    ];

param(domain_memory_stats) ->
    [
       {dom, remote_nonnull_domain},
       {maxStats, u_int},
       {flags, u_int}
    ];

param(domain_block_peek) ->
    [
       {dom, remote_nonnull_domain},
       {path, remote_nonnull_string},
       {offset, uhyper},
       {size, uint},
       {flags, uint}
    ];

param(domain_memory_peek) ->
    [
       {dom, remote_nonnull_domain},
       {offset, uhyper},
       {size, uint},
       {flags, uint}
    ];

param(list_domains) ->
    [
       {maxids, int}
    ];

param(domain_create_xml) ->
    [
       {xml_desc, remote_nonnull_string},
       {flags, int}
    ];

param(domain_lookup_by_id) ->
    [
       {id, int}
    ];

param(domain_lookup_by_uuid) ->
    [
       {uuid, remote_uuid}
    ];

param(domain_lookup_by_name) ->
    [
       {name, remote_nonnull_string}
    ];

param(domain_suspend) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_resume) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_shutdown) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_reboot) ->
    [
       {dom, remote_nonnull_domain},
       {flags, int}
    ];

param(domain_destroy) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_get_os_type) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_get_max_memory) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_set_max_memory) ->
    [
       {dom, remote_nonnull_domain},
       {memory, uhyper}
    ];

param(domain_set_memory) ->
    [
       {dom, remote_nonnull_domain},
       {memory, uhyper}
    ];

param(domain_get_info) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_save) ->
    [
       {dom, remote_nonnull_domain},
       {to, remote_nonnull_string}
    ];

param(domain_restore) ->
    [
       {from, remote_nonnull_string}
    ];

param(domain_core_dump) ->
    [
       {dom, remote_nonnull_domain},
       {to, remote_nonnull_string},
       {flags, int}
    ];

param(domain_dump_xml) ->
    [
       {dom, remote_nonnull_domain},
       {flags, int}
    ];

param(domain_migrate_prepare) ->
    [
       {uri_in, remote_string},
       {flags, uhyper},
       {dname, remote_string},
       {resource, uhyper}
    ];

param(domain_migrate_perform) ->
    [
       {dom, remote_nonnull_domain},
       {cookie, opaque},
       {uri, remote_nonnull_string},
       {flags, uhyper},
       {dname, remote_string},
       {resource, uhyper}
    ];

param(domain_migrate_finish) ->
    [
       {dname, remote_nonnull_string},
       {cookie, opaque},
       {uri, remote_nonnull_string},
       {flags, uhyper}
    ];

param(list_defined_domains) ->
    [
       {maxnames, int}
    ];

param(domain_create) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_define_xml) ->
    [
       {xml, remote_nonnull_string}
    ];

param(domain_undefine) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_set_vcpus) ->
    [
       {dom, remote_nonnull_domain},
       {nvcpus, int}
    ];

param(domain_pin_vcpu) ->
    [
       {dom, remote_nonnull_domain},
       {vcpu, int},
       {cpumap, opaque}
    ];

param(domain_get_vcpus) ->
    [
       {dom, remote_nonnull_domain},
       {maxinfo, int},
       {maplen, int}
    ];

param(domain_get_max_vcpus) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_get_security_label) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_attach_device) ->
    [
       {dom, remote_nonnull_domain},
       {xml, remote_nonnull_string}
    ];

param(domain_detach_device) ->
    [
       {dom, remote_nonnull_domain},
       {xml, remote_nonnull_string}
    ];

param(domain_get_autostart) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_set_autostart) ->
    [
       {dom, remote_nonnull_domain},
       {autostart, int}
    ];

param(list_networks) ->
    [
       {maxnames, int}
    ];

param(list_defined_networks) ->
    [
       {maxnames, int}
    ];

param(network_lookup_by_uuid) ->
    [
       {uuid, remote_uuid}
    ];

param(network_lookup_by_name) ->
    [
       {name, remote_nonnull_string}
    ];

param(network_create_xml) ->
    [
       {xml, remote_nonnull_string}
    ];

param(network_define_xml) ->
    [
       {xml, remote_nonnull_string}
    ];

param(network_undefine) ->
    [
       {net, remote_nonnull_network}
    ];

param(network_create) ->
    [
       {net, remote_nonnull_network}
    ];

param(network_destroy) ->
    [
       {net, remote_nonnull_network}
    ];

param(network_dump_xml) ->
    [
       {net, remote_nonnull_network},
       {flags, int}
    ];

param(network_get_bridge_name) ->
    [
       {net, remote_nonnull_network}
    ];

param(network_get_autostart) ->
    [
       {net, remote_nonnull_network}
    ];

param(network_set_autostart) ->
    [
       {net, remote_nonnull_network},
       {autostart, int}
    ];

param(list_interfaces) ->
    [
       {maxnames, int}
    ];

param(list_defined_interfaces) ->
    [
       {maxnames, int}
    ];

param(interface_lookup_by_name) ->
    [
       {name, remote_nonnull_string}
    ];

param(interface_lookup_by_mac_string) ->
    [
       {mac, remote_nonnull_string}
    ];

param(interface_get_xml_desc) ->
    [
       {iface, remote_nonnull_interface},
       {flags, uint}
    ];

param(interface_define_xml) ->
    [
       {xml, remote_nonnull_string},
       {flags, uint}
    ];

param(interface_undefine) ->
    [
       {iface, remote_nonnull_interface}
    ];

param(interface_create) ->
    [
       {iface, remote_nonnull_interface},
       {flags, uint}
    ];

param(interface_destroy) ->
    [
       {iface, remote_nonnull_interface},
       {flags, uint}
    ];

param(auth_sasl_start) ->
    [
       {mech, remote_nonnull_string},
       {nil, int},
       {data, char}
    ];

param(auth_sasl_step) ->
    [
       {nil, int},
       {data, char}
    ];

param(list_storage_pools) ->
    [
       {maxnames, int}
    ];

param(list_defined_storage_pools) ->
    [
       {maxnames, int}
    ];

param(find_storage_pool_sources) ->
    [
       {type, remote_nonnull_string},
       {srcSpec, remote_string},
       {flags, uint}
    ];

param(storage_pool_lookup_by_uuid) ->
    [
       {uuid, remote_uuid}
    ];

param(storage_pool_lookup_by_name) ->
    [
       {name, remote_nonnull_string}
    ];

param(storage_pool_lookup_by_volume) ->
    [
       {vol, remote_nonnull_storage_vol}
    ];

param(storage_pool_create_xml) ->
    [
       {xml, remote_nonnull_string},
       {flags, uint}
    ];

param(storage_pool_define_xml) ->
    [
       {xml, remote_nonnull_string},
       {flags, uint}
    ];

param(storage_pool_build) ->
    [
       {pool, remote_nonnull_storage_pool},
       {flags, uint}
    ];

param(storage_pool_undefine) ->
    [
       {pool, remote_nonnull_storage_pool}
    ];

param(storage_pool_create) ->
    [
       {pool, remote_nonnull_storage_pool},
       {flags, uint}
    ];

param(storage_pool_destroy) ->
    [
       {pool, remote_nonnull_storage_pool}
    ];

param(storage_pool_delete) ->
    [
       {pool, remote_nonnull_storage_pool},
       {flags, uint}
    ];

param(storage_pool_refresh) ->
    [
       {pool, remote_nonnull_storage_pool},
       {flags, uint}
    ];

param(storage_pool_dump_xml) ->
    [
       {pool, remote_nonnull_storage_pool},
       {flags, uint}
    ];

param(storage_pool_get_info) ->
    [
       {pool, remote_nonnull_storage_pool}
    ];

param(storage_pool_get_autostart) ->
    [
       {pool, remote_nonnull_storage_pool}
    ];

param(storage_pool_set_autostart) ->
    [
       {pool, remote_nonnull_storage_pool},
       {autostart, int}
    ];

param(storage_pool_num_of_volumes) ->
    [
       {pool, remote_nonnull_storage_pool}
    ];

param(storage_pool_list_volumes) ->
    [
       {pool, remote_nonnull_storage_pool},
       {maxnames, int}
    ];

param(storage_vol_lookup_by_name) ->
    [
       {pool, remote_nonnull_storage_pool},
       {name, remote_nonnull_string}
    ];

param(storage_vol_lookup_by_key) ->
    [
       {key, remote_nonnull_string}
    ];

param(storage_vol_lookup_by_path) ->
    [
       {path, remote_nonnull_string}
    ];

param(storage_vol_create_xml) ->
    [
       {pool, remote_nonnull_storage_pool},
       {xml, remote_nonnull_string},
       {flags, uint}
    ];

param(storage_vol_create_xml_from) ->
    [
       {pool, remote_nonnull_storage_pool},
       {xml, remote_nonnull_string},
       {clonevol, remote_nonnull_storage_vol},
       {flags, uint}
    ];

param(storage_vol_delete) ->
    [
       {vol, remote_nonnull_storage_vol},
       {flags, uint}
    ];

param(storage_vol_dump_xml) ->
    [
       {vol, remote_nonnull_storage_vol},
       {flags, uint}
    ];

param(storage_vol_get_info) ->
    [
       {vol, remote_nonnull_storage_vol}
    ];

param(storage_vol_get_path) ->
    [
       {vol, remote_nonnull_storage_vol}
    ];

param(node_num_of_devices) ->
    [
       {cap, remote_string},
       {flags, uint}
    ];

param(node_list_devices) ->
    [
       {cap, remote_string},
       {maxnames, int},
       {flags, uint}
    ];

param(node_device_lookup_by_name) ->
    [
       {name, remote_nonnull_string}
    ];

param(node_device_dump_xml) ->
    [
       {name, remote_nonnull_string},
       {flags, uint}
    ];

param(node_device_get_parent) ->
    [
       {name, remote_nonnull_string}
    ];

param(node_device_num_of_caps) ->
    [
       {name, remote_nonnull_string}
    ];

param(node_device_list_caps) ->
    [
       {name, remote_nonnull_string},
       {maxnames, int}
    ];

param(node_device_dettach) ->
    [
       {name, remote_nonnull_string}
    ];

param(node_device_re_attach) ->
    [
       {name, remote_nonnull_string}
    ];

param(node_device_reset) ->
    [
       {name, remote_nonnull_string}
    ];

param(node_device_create_xml) ->
    [
       {xml_desc, remote_nonnull_string},
       {flags, int}
    ];

param(node_device_destroy) ->
    [
       {name, remote_nonnull_string}
    ];

param(domain_xml_from_native) ->
    [
       {nativeFormat, remote_nonnull_string},
       {nativeConfig, remote_nonnull_string},
       {flags, uint}
    ];

param(domain_xml_to_native) ->
    [
       {nativeFormat, remote_nonnull_string},
       {domainXml, remote_nonnull_string},
       {flags, uint}
    ];

param(list_secrets) ->
    [
       {maxuuids, int}
    ];

param(secret_lookup_by_uuid) ->
    [
       {uuid, remote_uuid}
    ];

param(secret_define_xml) ->
    [
       {xml, remote_nonnull_string},
       {flags, uint}
    ];

param(secret_get_xml_desc) ->
    [
       {secret, remote_nonnull_secret},
       {flags, uint}
    ];

param(secret_set_value) ->
    [
       {secret, remote_nonnull_secret},
       {value, opaque},
       {flags, uint}
    ];

param(secret_get_value) ->
    [
       {secret, remote_nonnull_secret},
       {flags, uint}
    ];

param(secret_undefine) ->
    [
       {secret, remote_nonnull_secret}
    ];

param(secret_lookup_by_usage) ->
    [
       {usageType, int},
       {usageID, remote_nonnull_string}
    ];

param(domain_migrate_prepare_tunnel) ->
    [
       {flags, uhyper},
       {dname, remote_string},
       {resource, uhyper},
       {dom_xml, remote_nonnull_string}
    ];

param(domain_is_active) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(domain_is_persistent) ->
    [
       {dom, remote_nonnull_domain}
    ];

param(network_is_active) ->
    [
       {net, remote_nonnull_network}
    ];

param(network_is_persistent) ->
    [
       {net, remote_nonnull_network}
    ];

param(storage_pool_is_active) ->
    [
       {pool, remote_nonnull_storage_pool}
    ];

param(storage_pool_is_persistent) ->
    [
       {pool, remote_nonnull_storage_pool}
    ];

param(interface_is_active) ->
    [
       {iface, remote_nonnull_interface}
    ];

param(cpu_compare) ->
    [
       {xml, remote_nonnull_string},
       {flags, uint}
    ];


param(domain_events_deregister) -> [];
param(auth_sasl_init) -> [];
param(get_hostname) -> [];
param(num_of_defined_interfaces) -> [];
param(domain_events_register) -> [];
param(num_of_interfaces) -> [];
param(auth_polkit) -> [];
param(num_of_defined_domains) -> [];
param(num_of_storage_pools) -> [];
param(get_version) -> [];
param(auth_list) -> [];
param(num_of_domains) -> [];
param(get_uri) -> [];
param(close) -> [];
param(num_of_defined_networks) -> [];
param(get_capabilities) -> [];
param(node_get_security_model) -> [];
param(get_type) -> [];
param(is_secure) -> [];
param(num_of_secrets) -> [];
param(node_get_info) -> [];
param(num_of_defined_storage_pools) -> [];
param(get_lib_version) -> [];
param(domain_event) -> [];
param(domain_migrate_finish2) -> [];
param(node_get_free_memory) -> [];
param(domain_migrate_prepare2) -> [];
param(num_of_networks) -> [];

param(_) ->
    {error, unsupported}.

api() -> [
	open,
	supports_feature,
	get_max_vcpus,
	node_get_cells_free_memory,
	domain_get_scheduler_type,
	domain_get_scheduler_parameters,
	domain_set_scheduler_parameters,
	domain_block_stats,
	domain_interface_stats,
	domain_memory_stats,
	domain_block_peek,
	domain_memory_peek,
	list_domains,
	domain_create_xml,
	domain_lookup_by_id,
	domain_lookup_by_uuid,
	domain_lookup_by_name,
	domain_suspend,
	domain_resume,
	domain_shutdown,
	domain_reboot,
	domain_destroy,
	domain_get_os_type,
	domain_get_max_memory,
	domain_set_max_memory,
	domain_set_memory,
	domain_get_info,
	domain_save,
	domain_restore,
	domain_core_dump,
	domain_dump_xml,
	domain_migrate_prepare,
	domain_migrate_perform,
	domain_migrate_finish,
	list_defined_domains,
	domain_create,
	domain_define_xml,
	domain_undefine,
	domain_set_vcpus,
	domain_pin_vcpu,
	domain_get_vcpus,
	domain_get_max_vcpus,
	domain_get_security_label,
	domain_attach_device,
	domain_detach_device,
	domain_get_autostart,
	domain_set_autostart,
	list_networks,
	list_defined_networks,
	network_lookup_by_uuid,
	network_lookup_by_name,
	network_create_xml,
	network_define_xml,
	network_undefine,
	network_create,
	network_destroy,
	network_dump_xml,
	network_get_bridge_name,
	network_get_autostart,
	network_set_autostart,
	list_interfaces,
	list_defined_interfaces,
	interface_lookup_by_name,
	interface_lookup_by_mac_string,
	interface_get_xml_desc,
	interface_define_xml,
	interface_undefine,
	interface_create,
	interface_destroy,
	auth_sasl_start,
	auth_sasl_step,
	list_storage_pools,
	list_defined_storage_pools,
	find_storage_pool_sources,
	storage_pool_lookup_by_uuid,
	storage_pool_lookup_by_name,
	storage_pool_lookup_by_volume,
	storage_pool_create_xml,
	storage_pool_define_xml,
	storage_pool_build,
	storage_pool_undefine,
	storage_pool_create,
	storage_pool_destroy,
	storage_pool_delete,
	storage_pool_refresh,
	storage_pool_dump_xml,
	storage_pool_get_info,
	storage_pool_get_autostart,
	storage_pool_set_autostart,
	storage_pool_num_of_volumes,
	storage_pool_list_volumes,
	storage_vol_lookup_by_name,
	storage_vol_lookup_by_key,
	storage_vol_lookup_by_path,
	storage_vol_create_xml,
	storage_vol_create_xml_from,
	storage_vol_delete,
	storage_vol_dump_xml,
	storage_vol_get_info,
	storage_vol_get_path,
	node_num_of_devices,
	node_list_devices,
	node_device_lookup_by_name,
	node_device_dump_xml,
	node_device_get_parent,
	node_device_num_of_caps,
	node_device_list_caps,
	node_device_dettach,
	node_device_re_attach,
	node_device_reset,
	node_device_create_xml,
	node_device_destroy,
	domain_xml_from_native,
	domain_xml_to_native,
	list_secrets,
	secret_lookup_by_uuid,
	secret_define_xml,
	secret_get_xml_desc,
	secret_set_value,
	secret_get_value,
	secret_undefine,
	secret_lookup_by_usage,
	domain_migrate_prepare_tunnel,
	domain_is_active,
	domain_is_persistent,
	network_is_active,
	network_is_persistent,
	storage_pool_is_active,
	storage_pool_is_persistent,
	interface_is_active,
	cpu_compare,
	domain_events_deregister,
	auth_sasl_init,
	get_hostname,
	num_of_defined_interfaces,
	domain_events_register,
	num_of_interfaces,
	auth_polkit,
	num_of_defined_domains,
	num_of_storage_pools,
	get_version,
	auth_list,
	num_of_domains,
	get_uri,
	close,
	num_of_defined_networks,
	get_capabilities,
	node_get_security_model,
	get_type,
	is_secure,
	num_of_secrets,
	node_get_info,
	num_of_defined_storage_pools,
	get_lib_version,
	domain_event,
	domain_migrate_finish2,
	node_get_free_memory,
	domain_migrate_prepare2,
	num_of_networks
].
