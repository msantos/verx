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
-module(verx_arg).
-export([
     domain_lookup_by_uuid/1,
     domain_lookup_by_uuid/0,
     storage_pool_list_volumes/1,
     storage_pool_list_volumes/0,
     secret_lookup_by_usage/1,
     secret_lookup_by_usage/0,
     domain_events_deregister/0,
     domain_shutdown/1,
     domain_shutdown/0,
     storage_vol_create_xml/1,
     storage_vol_create_xml/0,
     interface_define_xml/1,
     interface_define_xml/0,
     auth_sasl_init/0,
     get_hostname/0,
     num_of_defined_interfaces/0,
     domain_events_register/0,
     network_undefine/1,
     network_undefine/0,
     domain_create/1,
     domain_create/0,
     num_of_interfaces/0,
     secret_lookup_by_uuid/1,
     secret_lookup_by_uuid/0,
     domain_pin_vcpu/1,
     domain_pin_vcpu/0,
     network_create_xml/1,
     network_create_xml/0,
     open/1,
     open/0,
     storage_vol_create_xml_from/1,
     storage_vol_create_xml_from/0,
     list_domains/1,
     list_domains/0,
     network_define_xml/1,
     network_define_xml/0,
     storage_vol_delete/1,
     storage_vol_delete/0,
     network_dump_xml/1,
     network_dump_xml/0,
     domain_reboot/1,
     domain_reboot/0,
     domain_set_memory/1,
     domain_set_memory/0,
     storage_pool_destroy/1,
     storage_pool_destroy/0,
     secret_define_xml/1,
     secret_define_xml/0,
     auth_polkit/0,
     interface_lookup_by_mac_string/1,
     interface_lookup_by_mac_string/0,
     auth_sasl_start/1,
     auth_sasl_start/0,
     domain_interface_stats/1,
     domain_interface_stats/0,
     interface_destroy/1,
     interface_destroy/0,
     storage_pool_dump_xml/1,
     storage_pool_dump_xml/0,
     interface_get_xml_desc/1,
     interface_get_xml_desc/0,
     node_device_re_attach/1,
     node_device_re_attach/0,
     storage_pool_delete/1,
     storage_pool_delete/0,
     domain_get_max_vcpus/1,
     domain_get_max_vcpus/0,
     secret_get_value/1,
     secret_get_value/0,
     supports_feature/1,
     supports_feature/0,
     list_defined_interfaces/1,
     list_defined_interfaces/0,
     domain_lookup_by_name/1,
     domain_lookup_by_name/0,
     storage_pool_set_autostart/1,
     storage_pool_set_autostart/0,
     list_interfaces/1,
     list_interfaces/0,
     secret_set_value/1,
     secret_set_value/0,
     auth_sasl_step/1,
     auth_sasl_step/0,
     domain_migrate_finish/1,
     domain_migrate_finish/0,
     interface_undefine/1,
     interface_undefine/0,
     node_device_dump_xml/1,
     node_device_dump_xml/0,
     node_device_lookup_by_name/1,
     node_device_lookup_by_name/0,
     list_secrets/1,
     list_secrets/0,
     domain_memory_peek/1,
     domain_memory_peek/0,
     num_of_defined_domains/0,
     node_list_devices/1,
     node_list_devices/0,
     domain_block_stats/1,
     domain_block_stats/0,
     domain_memory_stats/1,
     domain_memory_stats/0,
     num_of_storage_pools/0,
     domain_save/1,
     domain_save/0,
     domain_migrate_prepare_tunnel/1,
     domain_migrate_prepare_tunnel/0,
     get_version/0,
     domain_suspend/1,
     domain_suspend/0,
     network_set_autostart/1,
     network_set_autostart/0,
     domain_is_active/1,
     domain_is_active/0,
     network_get_autostart/1,
     network_get_autostart/0,
     node_device_create_xml/1,
     node_device_create_xml/0,
     auth_list/0,
     list_defined_storage_pools/1,
     list_defined_storage_pools/0,
     domain_get_max_memory/1,
     domain_get_max_memory/0,
     num_of_domains/0,
     list_networks/1,
     list_networks/0,
     storage_pool_undefine/1,
     storage_pool_undefine/0,
     domain_set_autostart/1,
     domain_set_autostart/0,
     storage_pool_get_autostart/1,
     storage_pool_get_autostart/0,
     domain_get_security_label/1,
     domain_get_security_label/0,
     storage_vol_get_path/1,
     storage_vol_get_path/0,
     get_uri/0,
     close/0,
     interface_is_active/1,
     interface_is_active/0,
     storage_pool_get_info/1,
     storage_pool_get_info/0,
     domain_restore/1,
     domain_restore/0,
     network_create/1,
     network_create/0,
     num_of_defined_networks/0,
     storage_pool_define_xml/1,
     storage_pool_define_xml/0,
     network_lookup_by_uuid/1,
     network_lookup_by_uuid/0,
     storage_vol_get_info/1,
     storage_vol_get_info/0,
     domain_define_xml/1,
     domain_define_xml/0,
     storage_vol_dump_xml/1,
     storage_vol_dump_xml/0,
     interface_lookup_by_name/1,
     interface_lookup_by_name/0,
     node_device_destroy/1,
     node_device_destroy/0,
     get_max_vcpus/1,
     get_max_vcpus/0,
     domain_migrate_perform/1,
     domain_migrate_perform/0,
     node_device_get_parent/1,
     node_device_get_parent/0,
     list_defined_domains/1,
     list_defined_domains/0,
     get_capabilities/0,
     secret_undefine/1,
     secret_undefine/0,
     domain_set_max_memory/1,
     domain_set_max_memory/0,
     interface_create/1,
     interface_create/0,
     domain_get_os_type/1,
     domain_get_os_type/0,
     domain_get_autostart/1,
     domain_get_autostart/0,
     domain_set_vcpus/1,
     domain_set_vcpus/0,
     node_get_security_model/0,
     network_destroy/1,
     network_destroy/0,
     storage_pool_lookup_by_uuid/1,
     storage_pool_lookup_by_uuid/0,
     storage_vol_lookup_by_key/1,
     storage_vol_lookup_by_key/0,
     list_defined_networks/1,
     list_defined_networks/0,
     node_device_list_caps/1,
     node_device_list_caps/0,
     storage_pool_refresh/1,
     storage_pool_refresh/0,
     storage_vol_lookup_by_path/1,
     storage_vol_lookup_by_path/0,
     network_is_active/1,
     network_is_active/0,
     get_type/0,
     domain_block_peek/1,
     domain_block_peek/0,
     is_secure/0,
     domain_set_scheduler_parameters/1,
     domain_set_scheduler_parameters/0,
     storage_pool_is_persistent/1,
     storage_pool_is_persistent/0,
     storage_pool_create_xml/1,
     storage_pool_create_xml/0,
     storage_pool_build/1,
     storage_pool_build/0,
     storage_pool_lookup_by_volume/1,
     storage_pool_lookup_by_volume/0,
     domain_get_info/1,
     domain_get_info/0,
     node_device_num_of_caps/1,
     node_device_num_of_caps/0,
     storage_pool_num_of_volumes/1,
     storage_pool_num_of_volumes/0,
     domain_resume/1,
     domain_resume/0,
     domain_destroy/1,
     domain_destroy/0,
     network_get_bridge_name/1,
     network_get_bridge_name/0,
     find_storage_pool_sources/1,
     find_storage_pool_sources/0,
     node_num_of_devices/1,
     node_num_of_devices/0,
     domain_get_vcpus/1,
     domain_get_vcpus/0,
     domain_get_scheduler_parameters/1,
     domain_get_scheduler_parameters/0,
     num_of_secrets/0,
     node_get_info/0,
     node_device_reset/1,
     node_device_reset/0,
     network_lookup_by_name/1,
     network_lookup_by_name/0,
     node_device_dettach/1,
     node_device_dettach/0,
     domain_xml_from_native/1,
     domain_xml_from_native/0,
     domain_detach_device/1,
     domain_detach_device/0,
     domain_migrate_prepare/1,
     domain_migrate_prepare/0,
     domain_create_xml/1,
     domain_create_xml/0,
     domain_undefine/1,
     domain_undefine/0,
     domain_get_scheduler_type/1,
     domain_get_scheduler_type/0,
     node_get_cells_free_memory/1,
     node_get_cells_free_memory/0,
     network_is_persistent/1,
     network_is_persistent/0,
     storage_pool_lookup_by_name/1,
     storage_pool_lookup_by_name/0,
     secret_get_xml_desc/1,
     secret_get_xml_desc/0,
     storage_pool_create/1,
     storage_pool_create/0,
     num_of_defined_storage_pools/0,
     domain_is_persistent/1,
     domain_is_persistent/0,
     domain_core_dump/1,
     domain_core_dump/0,
     get_lib_version/0,
     domain_event/0,
     domain_migrate_finish2/0,
     node_get_free_memory/0,
     domain_migrate_prepare2/0,
     domain_attach_device/1,
     domain_attach_device/0,
     domain_lookup_by_id/1,
     domain_lookup_by_id/0,
     num_of_networks/0,
     cpu_compare/1,
     cpu_compare/0,
     domain_xml_to_native/1,
     domain_xml_to_native/0,
     list_storage_pools/1,
     list_storage_pools/0,
     storage_vol_lookup_by_name/1,
     storage_vol_lookup_by_name/0,
     storage_pool_is_active/1,
     storage_pool_is_active/0,
     domain_dump_xml/1,
     domain_dump_xml/0
    ]).
-include("verx.hrl").

open(Struct) ->
    verx_xdr:arg(Struct, open()).
open() ->
    [
       {name, remote_string},
       {flags, int}
    ].

supports_feature(Struct) ->
    verx_xdr:arg(Struct, supports_feature()).
supports_feature() ->
    [
       {feature, int}
    ].

get_max_vcpus(Struct) ->
    verx_xdr:arg(Struct, get_max_vcpus()).
get_max_vcpus() ->
    [
       {type, remote_string}
    ].

node_get_cells_free_memory(Struct) ->
    verx_xdr:arg(Struct, node_get_cells_free_memory()).
node_get_cells_free_memory() ->
    [
       {startCell, int},
       {maxCells, int}
    ].

domain_get_scheduler_type(Struct) ->
    verx_xdr:arg(Struct, domain_get_scheduler_type()).
domain_get_scheduler_type() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_get_scheduler_parameters(Struct) ->
    verx_xdr:arg(Struct, domain_get_scheduler_parameters()).
domain_get_scheduler_parameters() ->
    [
       {dom, remote_nonnull_domain},
       {nparams, int}
    ].

domain_set_scheduler_parameters(Struct) ->
    verx_xdr:arg(Struct, domain_set_scheduler_parameters()).
domain_set_scheduler_parameters() ->
    [
       {dom, remote_nonnull_domain},
       {params, remote_sched_param}
    ].

domain_block_stats(Struct) ->
    verx_xdr:arg(Struct, domain_block_stats()).
domain_block_stats() ->
    [
       {dom, remote_nonnull_domain},
       {path, remote_nonnull_string}
    ].

domain_interface_stats(Struct) ->
    verx_xdr:arg(Struct, domain_interface_stats()).
domain_interface_stats() ->
    [
       {dom, remote_nonnull_domain},
       {path, remote_nonnull_string}
    ].

domain_memory_stats(Struct) ->
    verx_xdr:arg(Struct, domain_memory_stats()).
domain_memory_stats() ->
    [
       {dom, remote_nonnull_domain},
       {maxStats, u_int},
       {flags, u_int}
    ].

domain_block_peek(Struct) ->
    verx_xdr:arg(Struct, domain_block_peek()).
domain_block_peek() ->
    [
       {dom, remote_nonnull_domain},
       {path, remote_nonnull_string},
       {offset, uhyper},
       {size, uint},
       {flags, uint}
    ].

domain_memory_peek(Struct) ->
    verx_xdr:arg(Struct, domain_memory_peek()).
domain_memory_peek() ->
    [
       {dom, remote_nonnull_domain},
       {offset, uhyper},
       {size, uint},
       {flags, uint}
    ].

list_domains(Struct) ->
    verx_xdr:arg(Struct, list_domains()).
list_domains() ->
    [
       {maxids, int}
    ].

domain_create_xml(Struct) ->
    verx_xdr:arg(Struct, domain_create_xml()).
domain_create_xml() ->
    [
       {xml_desc, remote_nonnull_string},
       {flags, int}
    ].

domain_lookup_by_id(Struct) ->
    verx_xdr:arg(Struct, domain_lookup_by_id()).
domain_lookup_by_id() ->
    [
       {id, int}
    ].

domain_lookup_by_uuid(Struct) ->
    verx_xdr:arg(Struct, domain_lookup_by_uuid()).
domain_lookup_by_uuid() ->
    [
       {uuid, remote_uuid}
    ].

domain_lookup_by_name(Struct) ->
    verx_xdr:arg(Struct, domain_lookup_by_name()).
domain_lookup_by_name() ->
    [
       {name, remote_nonnull_string}
    ].

domain_suspend(Struct) ->
    verx_xdr:arg(Struct, domain_suspend()).
domain_suspend() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_resume(Struct) ->
    verx_xdr:arg(Struct, domain_resume()).
domain_resume() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_shutdown(Struct) ->
    verx_xdr:arg(Struct, domain_shutdown()).
domain_shutdown() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_reboot(Struct) ->
    verx_xdr:arg(Struct, domain_reboot()).
domain_reboot() ->
    [
       {dom, remote_nonnull_domain},
       {flags, int}
    ].

domain_destroy(Struct) ->
    verx_xdr:arg(Struct, domain_destroy()).
domain_destroy() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_get_os_type(Struct) ->
    verx_xdr:arg(Struct, domain_get_os_type()).
domain_get_os_type() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_get_max_memory(Struct) ->
    verx_xdr:arg(Struct, domain_get_max_memory()).
domain_get_max_memory() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_set_max_memory(Struct) ->
    verx_xdr:arg(Struct, domain_set_max_memory()).
domain_set_max_memory() ->
    [
       {dom, remote_nonnull_domain},
       {memory, uhyper}
    ].

domain_set_memory(Struct) ->
    verx_xdr:arg(Struct, domain_set_memory()).
domain_set_memory() ->
    [
       {dom, remote_nonnull_domain},
       {memory, uhyper}
    ].

domain_get_info(Struct) ->
    verx_xdr:arg(Struct, domain_get_info()).
domain_get_info() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_save(Struct) ->
    verx_xdr:arg(Struct, domain_save()).
domain_save() ->
    [
       {dom, remote_nonnull_domain},
       {to, remote_nonnull_string}
    ].

domain_restore(Struct) ->
    verx_xdr:arg(Struct, domain_restore()).
domain_restore() ->
    [
       {from, remote_nonnull_string}
    ].

domain_core_dump(Struct) ->
    verx_xdr:arg(Struct, domain_core_dump()).
domain_core_dump() ->
    [
       {dom, remote_nonnull_domain},
       {to, remote_nonnull_string},
       {flags, int}
    ].

domain_dump_xml(Struct) ->
    verx_xdr:arg(Struct, domain_dump_xml()).
domain_dump_xml() ->
    [
       {dom, remote_nonnull_domain},
       {flags, int}
    ].

domain_migrate_prepare(Struct) ->
    verx_xdr:arg(Struct, domain_migrate_prepare()).
domain_migrate_prepare() ->
    [
       {uri_in, remote_string},
       {flags, uhyper},
       {dname, remote_string},
       {resource, uhyper}
    ].

domain_migrate_perform(Struct) ->
    verx_xdr:arg(Struct, domain_migrate_perform()).
domain_migrate_perform() ->
    [
       {dom, remote_nonnull_domain},
       {cookie, opaque},
       {uri, remote_nonnull_string},
       {flags, uhyper},
       {dname, remote_string},
       {resource, uhyper}
    ].

domain_migrate_finish(Struct) ->
    verx_xdr:arg(Struct, domain_migrate_finish()).
domain_migrate_finish() ->
    [
       {dname, remote_nonnull_string},
       {cookie, opaque},
       {uri, remote_nonnull_string},
       {flags, uhyper}
    ].

list_defined_domains(Struct) ->
    verx_xdr:arg(Struct, list_defined_domains()).
list_defined_domains() ->
    [
       {maxnames, int}
    ].

domain_create(Struct) ->
    verx_xdr:arg(Struct, domain_create()).
domain_create() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_define_xml(Struct) ->
    verx_xdr:arg(Struct, domain_define_xml()).
domain_define_xml() ->
    [
       {xml, remote_nonnull_string}
    ].

domain_undefine(Struct) ->
    verx_xdr:arg(Struct, domain_undefine()).
domain_undefine() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_set_vcpus(Struct) ->
    verx_xdr:arg(Struct, domain_set_vcpus()).
domain_set_vcpus() ->
    [
       {dom, remote_nonnull_domain},
       {nvcpus, int}
    ].

domain_pin_vcpu(Struct) ->
    verx_xdr:arg(Struct, domain_pin_vcpu()).
domain_pin_vcpu() ->
    [
       {dom, remote_nonnull_domain},
       {vcpu, int},
       {cpumap, opaque}
    ].

domain_get_vcpus(Struct) ->
    verx_xdr:arg(Struct, domain_get_vcpus()).
domain_get_vcpus() ->
    [
       {dom, remote_nonnull_domain},
       {maxinfo, int},
       {maplen, int}
    ].

domain_get_max_vcpus(Struct) ->
    verx_xdr:arg(Struct, domain_get_max_vcpus()).
domain_get_max_vcpus() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_get_security_label(Struct) ->
    verx_xdr:arg(Struct, domain_get_security_label()).
domain_get_security_label() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_attach_device(Struct) ->
    verx_xdr:arg(Struct, domain_attach_device()).
domain_attach_device() ->
    [
       {dom, remote_nonnull_domain},
       {xml, remote_nonnull_string}
    ].

domain_detach_device(Struct) ->
    verx_xdr:arg(Struct, domain_detach_device()).
domain_detach_device() ->
    [
       {dom, remote_nonnull_domain},
       {xml, remote_nonnull_string}
    ].

domain_get_autostart(Struct) ->
    verx_xdr:arg(Struct, domain_get_autostart()).
domain_get_autostart() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_set_autostart(Struct) ->
    verx_xdr:arg(Struct, domain_set_autostart()).
domain_set_autostart() ->
    [
       {dom, remote_nonnull_domain},
       {autostart, int}
    ].

list_networks(Struct) ->
    verx_xdr:arg(Struct, list_networks()).
list_networks() ->
    [
       {maxnames, int}
    ].

list_defined_networks(Struct) ->
    verx_xdr:arg(Struct, list_defined_networks()).
list_defined_networks() ->
    [
       {maxnames, int}
    ].

network_lookup_by_uuid(Struct) ->
    verx_xdr:arg(Struct, network_lookup_by_uuid()).
network_lookup_by_uuid() ->
    [
       {uuid, remote_uuid}
    ].

network_lookup_by_name(Struct) ->
    verx_xdr:arg(Struct, network_lookup_by_name()).
network_lookup_by_name() ->
    [
       {name, remote_nonnull_string}
    ].

network_create_xml(Struct) ->
    verx_xdr:arg(Struct, network_create_xml()).
network_create_xml() ->
    [
       {xml, remote_nonnull_string}
    ].

network_define_xml(Struct) ->
    verx_xdr:arg(Struct, network_define_xml()).
network_define_xml() ->
    [
       {xml, remote_nonnull_string}
    ].

network_undefine(Struct) ->
    verx_xdr:arg(Struct, network_undefine()).
network_undefine() ->
    [
       {net, remote_nonnull_network}
    ].

network_create(Struct) ->
    verx_xdr:arg(Struct, network_create()).
network_create() ->
    [
       {net, remote_nonnull_network}
    ].

network_destroy(Struct) ->
    verx_xdr:arg(Struct, network_destroy()).
network_destroy() ->
    [
       {net, remote_nonnull_network}
    ].

network_dump_xml(Struct) ->
    verx_xdr:arg(Struct, network_dump_xml()).
network_dump_xml() ->
    [
       {net, remote_nonnull_network},
       {flags, int}
    ].

network_get_bridge_name(Struct) ->
    verx_xdr:arg(Struct, network_get_bridge_name()).
network_get_bridge_name() ->
    [
       {net, remote_nonnull_network}
    ].

network_get_autostart(Struct) ->
    verx_xdr:arg(Struct, network_get_autostart()).
network_get_autostart() ->
    [
       {net, remote_nonnull_network}
    ].

network_set_autostart(Struct) ->
    verx_xdr:arg(Struct, network_set_autostart()).
network_set_autostart() ->
    [
       {net, remote_nonnull_network},
       {autostart, int}
    ].

list_interfaces(Struct) ->
    verx_xdr:arg(Struct, list_interfaces()).
list_interfaces() ->
    [
       {maxnames, int}
    ].

list_defined_interfaces(Struct) ->
    verx_xdr:arg(Struct, list_defined_interfaces()).
list_defined_interfaces() ->
    [
       {maxnames, int}
    ].

interface_lookup_by_name(Struct) ->
    verx_xdr:arg(Struct, interface_lookup_by_name()).
interface_lookup_by_name() ->
    [
       {name, remote_nonnull_string}
    ].

interface_lookup_by_mac_string(Struct) ->
    verx_xdr:arg(Struct, interface_lookup_by_mac_string()).
interface_lookup_by_mac_string() ->
    [
       {mac, remote_nonnull_string}
    ].

interface_get_xml_desc(Struct) ->
    verx_xdr:arg(Struct, interface_get_xml_desc()).
interface_get_xml_desc() ->
    [
       {iface, remote_nonnull_interface},
       {flags, uint}
    ].

interface_define_xml(Struct) ->
    verx_xdr:arg(Struct, interface_define_xml()).
interface_define_xml() ->
    [
       {xml, remote_nonnull_string},
       {flags, uint}
    ].

interface_undefine(Struct) ->
    verx_xdr:arg(Struct, interface_undefine()).
interface_undefine() ->
    [
       {iface, remote_nonnull_interface}
    ].

interface_create(Struct) ->
    verx_xdr:arg(Struct, interface_create()).
interface_create() ->
    [
       {iface, remote_nonnull_interface},
       {flags, uint}
    ].

interface_destroy(Struct) ->
    verx_xdr:arg(Struct, interface_destroy()).
interface_destroy() ->
    [
       {iface, remote_nonnull_interface},
       {flags, uint}
    ].

auth_sasl_start(Struct) ->
    verx_xdr:arg(Struct, auth_sasl_start()).
auth_sasl_start() ->
    [
       {mech, remote_nonnull_string},
       {nil, int},
       {data, char}
    ].

auth_sasl_step(Struct) ->
    verx_xdr:arg(Struct, auth_sasl_step()).
auth_sasl_step() ->
    [
       {nil, int},
       {data, char}
    ].

list_storage_pools(Struct) ->
    verx_xdr:arg(Struct, list_storage_pools()).
list_storage_pools() ->
    [
       {maxnames, int}
    ].

list_defined_storage_pools(Struct) ->
    verx_xdr:arg(Struct, list_defined_storage_pools()).
list_defined_storage_pools() ->
    [
       {maxnames, int}
    ].

find_storage_pool_sources(Struct) ->
    verx_xdr:arg(Struct, find_storage_pool_sources()).
find_storage_pool_sources() ->
    [
       {type, remote_nonnull_string},
       {srcSpec, remote_string},
       {flags, uint}
    ].

storage_pool_lookup_by_uuid(Struct) ->
    verx_xdr:arg(Struct, storage_pool_lookup_by_uuid()).
storage_pool_lookup_by_uuid() ->
    [
       {uuid, remote_uuid}
    ].

storage_pool_lookup_by_name(Struct) ->
    verx_xdr:arg(Struct, storage_pool_lookup_by_name()).
storage_pool_lookup_by_name() ->
    [
       {name, remote_nonnull_string}
    ].

storage_pool_lookup_by_volume(Struct) ->
    verx_xdr:arg(Struct, storage_pool_lookup_by_volume()).
storage_pool_lookup_by_volume() ->
    [
       {vol, remote_nonnull_storage_vol}
    ].

storage_pool_create_xml(Struct) ->
    verx_xdr:arg(Struct, storage_pool_create_xml()).
storage_pool_create_xml() ->
    [
       {xml, remote_nonnull_string},
       {flags, uint}
    ].

storage_pool_define_xml(Struct) ->
    verx_xdr:arg(Struct, storage_pool_define_xml()).
storage_pool_define_xml() ->
    [
       {xml, remote_nonnull_string},
       {flags, uint}
    ].

storage_pool_build(Struct) ->
    verx_xdr:arg(Struct, storage_pool_build()).
storage_pool_build() ->
    [
       {pool, remote_nonnull_storage_pool},
       {flags, uint}
    ].

storage_pool_undefine(Struct) ->
    verx_xdr:arg(Struct, storage_pool_undefine()).
storage_pool_undefine() ->
    [
       {pool, remote_nonnull_storage_pool}
    ].

storage_pool_create(Struct) ->
    verx_xdr:arg(Struct, storage_pool_create()).
storage_pool_create() ->
    [
       {pool, remote_nonnull_storage_pool},
       {flags, uint}
    ].

storage_pool_destroy(Struct) ->
    verx_xdr:arg(Struct, storage_pool_destroy()).
storage_pool_destroy() ->
    [
       {pool, remote_nonnull_storage_pool}
    ].

storage_pool_delete(Struct) ->
    verx_xdr:arg(Struct, storage_pool_delete()).
storage_pool_delete() ->
    [
       {pool, remote_nonnull_storage_pool},
       {flags, uint}
    ].

storage_pool_refresh(Struct) ->
    verx_xdr:arg(Struct, storage_pool_refresh()).
storage_pool_refresh() ->
    [
       {pool, remote_nonnull_storage_pool},
       {flags, uint}
    ].

storage_pool_dump_xml(Struct) ->
    verx_xdr:arg(Struct, storage_pool_dump_xml()).
storage_pool_dump_xml() ->
    [
       {pool, remote_nonnull_storage_pool},
       {flags, uint}
    ].

storage_pool_get_info(Struct) ->
    verx_xdr:arg(Struct, storage_pool_get_info()).
storage_pool_get_info() ->
    [
       {pool, remote_nonnull_storage_pool}
    ].

storage_pool_get_autostart(Struct) ->
    verx_xdr:arg(Struct, storage_pool_get_autostart()).
storage_pool_get_autostart() ->
    [
       {pool, remote_nonnull_storage_pool}
    ].

storage_pool_set_autostart(Struct) ->
    verx_xdr:arg(Struct, storage_pool_set_autostart()).
storage_pool_set_autostart() ->
    [
       {pool, remote_nonnull_storage_pool},
       {autostart, int}
    ].

storage_pool_num_of_volumes(Struct) ->
    verx_xdr:arg(Struct, storage_pool_num_of_volumes()).
storage_pool_num_of_volumes() ->
    [
       {pool, remote_nonnull_storage_pool}
    ].

storage_pool_list_volumes(Struct) ->
    verx_xdr:arg(Struct, storage_pool_list_volumes()).
storage_pool_list_volumes() ->
    [
       {pool, remote_nonnull_storage_pool},
       {maxnames, int}
    ].

storage_vol_lookup_by_name(Struct) ->
    verx_xdr:arg(Struct, storage_vol_lookup_by_name()).
storage_vol_lookup_by_name() ->
    [
       {pool, remote_nonnull_storage_pool},
       {name, remote_nonnull_string}
    ].

storage_vol_lookup_by_key(Struct) ->
    verx_xdr:arg(Struct, storage_vol_lookup_by_key()).
storage_vol_lookup_by_key() ->
    [
       {key, remote_nonnull_string}
    ].

storage_vol_lookup_by_path(Struct) ->
    verx_xdr:arg(Struct, storage_vol_lookup_by_path()).
storage_vol_lookup_by_path() ->
    [
       {path, remote_nonnull_string}
    ].

storage_vol_create_xml(Struct) ->
    verx_xdr:arg(Struct, storage_vol_create_xml()).
storage_vol_create_xml() ->
    [
       {pool, remote_nonnull_storage_pool},
       {xml, remote_nonnull_string},
       {flags, uint}
    ].

storage_vol_create_xml_from(Struct) ->
    verx_xdr:arg(Struct, storage_vol_create_xml_from()).
storage_vol_create_xml_from() ->
    [
       {pool, remote_nonnull_storage_pool},
       {xml, remote_nonnull_string},
       {clonevol, remote_nonnull_storage_vol},
       {flags, uint}
    ].

storage_vol_delete(Struct) ->
    verx_xdr:arg(Struct, storage_vol_delete()).
storage_vol_delete() ->
    [
       {vol, remote_nonnull_storage_vol},
       {flags, uint}
    ].

storage_vol_dump_xml(Struct) ->
    verx_xdr:arg(Struct, storage_vol_dump_xml()).
storage_vol_dump_xml() ->
    [
       {vol, remote_nonnull_storage_vol},
       {flags, uint}
    ].

storage_vol_get_info(Struct) ->
    verx_xdr:arg(Struct, storage_vol_get_info()).
storage_vol_get_info() ->
    [
       {vol, remote_nonnull_storage_vol}
    ].

storage_vol_get_path(Struct) ->
    verx_xdr:arg(Struct, storage_vol_get_path()).
storage_vol_get_path() ->
    [
       {vol, remote_nonnull_storage_vol}
    ].

node_num_of_devices(Struct) ->
    verx_xdr:arg(Struct, node_num_of_devices()).
node_num_of_devices() ->
    [
       {cap, remote_string},
       {flags, uint}
    ].

node_list_devices(Struct) ->
    verx_xdr:arg(Struct, node_list_devices()).
node_list_devices() ->
    [
       {cap, remote_string},
       {maxnames, int},
       {flags, uint}
    ].

node_device_lookup_by_name(Struct) ->
    verx_xdr:arg(Struct, node_device_lookup_by_name()).
node_device_lookup_by_name() ->
    [
       {name, remote_nonnull_string}
    ].

node_device_dump_xml(Struct) ->
    verx_xdr:arg(Struct, node_device_dump_xml()).
node_device_dump_xml() ->
    [
       {name, remote_nonnull_string},
       {flags, uint}
    ].

node_device_get_parent(Struct) ->
    verx_xdr:arg(Struct, node_device_get_parent()).
node_device_get_parent() ->
    [
       {name, remote_nonnull_string}
    ].

node_device_num_of_caps(Struct) ->
    verx_xdr:arg(Struct, node_device_num_of_caps()).
node_device_num_of_caps() ->
    [
       {name, remote_nonnull_string}
    ].

node_device_list_caps(Struct) ->
    verx_xdr:arg(Struct, node_device_list_caps()).
node_device_list_caps() ->
    [
       {name, remote_nonnull_string},
       {maxnames, int}
    ].

node_device_dettach(Struct) ->
    verx_xdr:arg(Struct, node_device_dettach()).
node_device_dettach() ->
    [
       {name, remote_nonnull_string}
    ].

node_device_re_attach(Struct) ->
    verx_xdr:arg(Struct, node_device_re_attach()).
node_device_re_attach() ->
    [
       {name, remote_nonnull_string}
    ].

node_device_reset(Struct) ->
    verx_xdr:arg(Struct, node_device_reset()).
node_device_reset() ->
    [
       {name, remote_nonnull_string}
    ].

node_device_create_xml(Struct) ->
    verx_xdr:arg(Struct, node_device_create_xml()).
node_device_create_xml() ->
    [
       {xml_desc, remote_nonnull_string},
       {flags, int}
    ].

node_device_destroy(Struct) ->
    verx_xdr:arg(Struct, node_device_destroy()).
node_device_destroy() ->
    [
       {name, remote_nonnull_string}
    ].

domain_xml_from_native(Struct) ->
    verx_xdr:arg(Struct, domain_xml_from_native()).
domain_xml_from_native() ->
    [
       {nativeFormat, remote_nonnull_string},
       {nativeConfig, remote_nonnull_string},
       {flags, uint}
    ].

domain_xml_to_native(Struct) ->
    verx_xdr:arg(Struct, domain_xml_to_native()).
domain_xml_to_native() ->
    [
       {nativeFormat, remote_nonnull_string},
       {domainXml, remote_nonnull_string},
       {flags, uint}
    ].

list_secrets(Struct) ->
    verx_xdr:arg(Struct, list_secrets()).
list_secrets() ->
    [
       {maxuuids, int}
    ].

secret_lookup_by_uuid(Struct) ->
    verx_xdr:arg(Struct, secret_lookup_by_uuid()).
secret_lookup_by_uuid() ->
    [
       {uuid, remote_uuid}
    ].

secret_define_xml(Struct) ->
    verx_xdr:arg(Struct, secret_define_xml()).
secret_define_xml() ->
    [
       {xml, remote_nonnull_string},
       {flags, uint}
    ].

secret_get_xml_desc(Struct) ->
    verx_xdr:arg(Struct, secret_get_xml_desc()).
secret_get_xml_desc() ->
    [
       {secret, remote_nonnull_secret},
       {flags, uint}
    ].

secret_set_value(Struct) ->
    verx_xdr:arg(Struct, secret_set_value()).
secret_set_value() ->
    [
       {secret, remote_nonnull_secret},
       {value, opaque},
       {flags, uint}
    ].

secret_get_value(Struct) ->
    verx_xdr:arg(Struct, secret_get_value()).
secret_get_value() ->
    [
       {secret, remote_nonnull_secret},
       {flags, uint}
    ].

secret_undefine(Struct) ->
    verx_xdr:arg(Struct, secret_undefine()).
secret_undefine() ->
    [
       {secret, remote_nonnull_secret}
    ].

secret_lookup_by_usage(Struct) ->
    verx_xdr:arg(Struct, secret_lookup_by_usage()).
secret_lookup_by_usage() ->
    [
       {usageType, int},
       {usageID, remote_nonnull_string}
    ].

domain_migrate_prepare_tunnel(Struct) ->
    verx_xdr:arg(Struct, domain_migrate_prepare_tunnel()).
domain_migrate_prepare_tunnel() ->
    [
       {flags, uhyper},
       {dname, remote_string},
       {resource, uhyper},
       {dom_xml, remote_nonnull_string}
    ].

domain_is_active(Struct) ->
    verx_xdr:arg(Struct, domain_is_active()).
domain_is_active() ->
    [
       {dom, remote_nonnull_domain}
    ].

domain_is_persistent(Struct) ->
    verx_xdr:arg(Struct, domain_is_persistent()).
domain_is_persistent() ->
    [
       {dom, remote_nonnull_domain}
    ].

network_is_active(Struct) ->
    verx_xdr:arg(Struct, network_is_active()).
network_is_active() ->
    [
       {net, remote_nonnull_network}
    ].

network_is_persistent(Struct) ->
    verx_xdr:arg(Struct, network_is_persistent()).
network_is_persistent() ->
    [
       {net, remote_nonnull_network}
    ].

storage_pool_is_active(Struct) ->
    verx_xdr:arg(Struct, storage_pool_is_active()).
storage_pool_is_active() ->
    [
       {pool, remote_nonnull_storage_pool}
    ].

storage_pool_is_persistent(Struct) ->
    verx_xdr:arg(Struct, storage_pool_is_persistent()).
storage_pool_is_persistent() ->
    [
       {pool, remote_nonnull_storage_pool}
    ].

interface_is_active(Struct) ->
    verx_xdr:arg(Struct, interface_is_active()).
interface_is_active() ->
    [
       {iface, remote_nonnull_interface}
    ].

cpu_compare(Struct) ->
    verx_xdr:arg(Struct, cpu_compare()).
cpu_compare() ->
    [
       {xml, remote_nonnull_string},
       {flags, uint}
    ].


domain_events_deregister() -> [].
auth_sasl_init() -> [].
get_hostname() -> [].
num_of_defined_interfaces() -> [].
domain_events_register() -> [].
num_of_interfaces() -> [].
auth_polkit() -> [].
num_of_defined_domains() -> [].
num_of_storage_pools() -> [].
get_version() -> [].
auth_list() -> [].
num_of_domains() -> [].
get_uri() -> [].
close() -> [].
num_of_defined_networks() -> [].
get_capabilities() -> [].
node_get_security_model() -> [].
get_type() -> [].
is_secure() -> [].
num_of_secrets() -> [].
node_get_info() -> [].
num_of_defined_storage_pools() -> [].
get_lib_version() -> [].
domain_event() -> [].
domain_migrate_finish2() -> [].
node_get_free_memory() -> [].
domain_migrate_prepare2() -> [].
num_of_networks() -> [].
