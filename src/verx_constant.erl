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
-module(verx_constant).
-export([proc/1,type/1,status/1]).
-include("verx.hrl").

proc(?REMOTE_PROC_OPEN) -> open;
proc(open) -> ?REMOTE_PROC_OPEN;
proc(?REMOTE_PROC_CLOSE) -> close;
proc(close) -> ?REMOTE_PROC_CLOSE;
proc(?REMOTE_PROC_GET_TYPE) -> get_type;
proc(get_type) -> ?REMOTE_PROC_GET_TYPE;
proc(?REMOTE_PROC_GET_VERSION) -> get_version;
proc(get_version) -> ?REMOTE_PROC_GET_VERSION;
proc(?REMOTE_PROC_GET_MAX_VCPUS) -> get_max_vcpus;
proc(get_max_vcpus) -> ?REMOTE_PROC_GET_MAX_VCPUS;
proc(?REMOTE_PROC_NODE_GET_INFO) -> node_get_info;
proc(node_get_info) -> ?REMOTE_PROC_NODE_GET_INFO;
proc(?REMOTE_PROC_GET_CAPABILITIES) -> get_capabilities;
proc(get_capabilities) -> ?REMOTE_PROC_GET_CAPABILITIES;
proc(?REMOTE_PROC_DOMAIN_ATTACH_DEVICE) -> domain_attach_device;
proc(domain_attach_device) -> ?REMOTE_PROC_DOMAIN_ATTACH_DEVICE;
proc(?REMOTE_PROC_DOMAIN_CREATE) -> domain_create;
proc(domain_create) -> ?REMOTE_PROC_DOMAIN_CREATE;
proc(?REMOTE_PROC_DOMAIN_CREATE_XML) -> domain_create_xml;
proc(domain_create_xml) -> ?REMOTE_PROC_DOMAIN_CREATE_XML;
proc(?REMOTE_PROC_DOMAIN_DEFINE_XML) -> domain_define_xml;
proc(domain_define_xml) -> ?REMOTE_PROC_DOMAIN_DEFINE_XML;
proc(?REMOTE_PROC_DOMAIN_DESTROY) -> domain_destroy;
proc(domain_destroy) -> ?REMOTE_PROC_DOMAIN_DESTROY;
proc(?REMOTE_PROC_DOMAIN_DETACH_DEVICE) -> domain_detach_device;
proc(domain_detach_device) -> ?REMOTE_PROC_DOMAIN_DETACH_DEVICE;
proc(?REMOTE_PROC_DOMAIN_DUMP_XML) -> domain_dump_xml;
proc(domain_dump_xml) -> ?REMOTE_PROC_DOMAIN_DUMP_XML;
proc(?REMOTE_PROC_DOMAIN_GET_AUTOSTART) -> domain_get_autostart;
proc(domain_get_autostart) -> ?REMOTE_PROC_DOMAIN_GET_AUTOSTART;
proc(?REMOTE_PROC_DOMAIN_GET_INFO) -> domain_get_info;
proc(domain_get_info) -> ?REMOTE_PROC_DOMAIN_GET_INFO;
proc(?REMOTE_PROC_DOMAIN_GET_MAX_MEMORY) -> domain_get_max_memory;
proc(domain_get_max_memory) -> ?REMOTE_PROC_DOMAIN_GET_MAX_MEMORY;
proc(?REMOTE_PROC_DOMAIN_GET_MAX_VCPUS) -> domain_get_max_vcpus;
proc(domain_get_max_vcpus) -> ?REMOTE_PROC_DOMAIN_GET_MAX_VCPUS;
proc(?REMOTE_PROC_DOMAIN_GET_OS_TYPE) -> domain_get_os_type;
proc(domain_get_os_type) -> ?REMOTE_PROC_DOMAIN_GET_OS_TYPE;
proc(?REMOTE_PROC_DOMAIN_GET_VCPUS) -> domain_get_vcpus;
proc(domain_get_vcpus) -> ?REMOTE_PROC_DOMAIN_GET_VCPUS;
proc(?REMOTE_PROC_LIST_DEFINED_DOMAINS) -> list_defined_domains;
proc(list_defined_domains) -> ?REMOTE_PROC_LIST_DEFINED_DOMAINS;
proc(?REMOTE_PROC_DOMAIN_LOOKUP_BY_ID) -> domain_lookup_by_id;
proc(domain_lookup_by_id) -> ?REMOTE_PROC_DOMAIN_LOOKUP_BY_ID;
proc(?REMOTE_PROC_DOMAIN_LOOKUP_BY_NAME) -> domain_lookup_by_name;
proc(domain_lookup_by_name) -> ?REMOTE_PROC_DOMAIN_LOOKUP_BY_NAME;
proc(?REMOTE_PROC_DOMAIN_LOOKUP_BY_UUID) -> domain_lookup_by_uuid;
proc(domain_lookup_by_uuid) -> ?REMOTE_PROC_DOMAIN_LOOKUP_BY_UUID;
proc(?REMOTE_PROC_NUM_OF_DEFINED_DOMAINS) -> num_of_defined_domains;
proc(num_of_defined_domains) -> ?REMOTE_PROC_NUM_OF_DEFINED_DOMAINS;
proc(?REMOTE_PROC_DOMAIN_PIN_VCPU) -> domain_pin_vcpu;
proc(domain_pin_vcpu) -> ?REMOTE_PROC_DOMAIN_PIN_VCPU;
proc(?REMOTE_PROC_DOMAIN_REBOOT) -> domain_reboot;
proc(domain_reboot) -> ?REMOTE_PROC_DOMAIN_REBOOT;
proc(?REMOTE_PROC_DOMAIN_RESUME) -> domain_resume;
proc(domain_resume) -> ?REMOTE_PROC_DOMAIN_RESUME;
proc(?REMOTE_PROC_DOMAIN_SET_AUTOSTART) -> domain_set_autostart;
proc(domain_set_autostart) -> ?REMOTE_PROC_DOMAIN_SET_AUTOSTART;
proc(?REMOTE_PROC_DOMAIN_SET_MAX_MEMORY) -> domain_set_max_memory;
proc(domain_set_max_memory) -> ?REMOTE_PROC_DOMAIN_SET_MAX_MEMORY;
proc(?REMOTE_PROC_DOMAIN_SET_MEMORY) -> domain_set_memory;
proc(domain_set_memory) -> ?REMOTE_PROC_DOMAIN_SET_MEMORY;
proc(?REMOTE_PROC_DOMAIN_SET_VCPUS) -> domain_set_vcpus;
proc(domain_set_vcpus) -> ?REMOTE_PROC_DOMAIN_SET_VCPUS;
proc(?REMOTE_PROC_DOMAIN_SHUTDOWN) -> domain_shutdown;
proc(domain_shutdown) -> ?REMOTE_PROC_DOMAIN_SHUTDOWN;
proc(?REMOTE_PROC_DOMAIN_SUSPEND) -> domain_suspend;
proc(domain_suspend) -> ?REMOTE_PROC_DOMAIN_SUSPEND;
proc(?REMOTE_PROC_DOMAIN_UNDEFINE) -> domain_undefine;
proc(domain_undefine) -> ?REMOTE_PROC_DOMAIN_UNDEFINE;
proc(?REMOTE_PROC_LIST_DEFINED_NETWORKS) -> list_defined_networks;
proc(list_defined_networks) -> ?REMOTE_PROC_LIST_DEFINED_NETWORKS;
proc(?REMOTE_PROC_LIST_DOMAINS) -> list_domains;
proc(list_domains) -> ?REMOTE_PROC_LIST_DOMAINS;
proc(?REMOTE_PROC_LIST_NETWORKS) -> list_networks;
proc(list_networks) -> ?REMOTE_PROC_LIST_NETWORKS;
proc(?REMOTE_PROC_NETWORK_CREATE) -> network_create;
proc(network_create) -> ?REMOTE_PROC_NETWORK_CREATE;
proc(?REMOTE_PROC_NETWORK_CREATE_XML) -> network_create_xml;
proc(network_create_xml) -> ?REMOTE_PROC_NETWORK_CREATE_XML;
proc(?REMOTE_PROC_NETWORK_DEFINE_XML) -> network_define_xml;
proc(network_define_xml) -> ?REMOTE_PROC_NETWORK_DEFINE_XML;
proc(?REMOTE_PROC_NETWORK_DESTROY) -> network_destroy;
proc(network_destroy) -> ?REMOTE_PROC_NETWORK_DESTROY;
proc(?REMOTE_PROC_NETWORK_DUMP_XML) -> network_dump_xml;
proc(network_dump_xml) -> ?REMOTE_PROC_NETWORK_DUMP_XML;
proc(?REMOTE_PROC_NETWORK_GET_AUTOSTART) -> network_get_autostart;
proc(network_get_autostart) -> ?REMOTE_PROC_NETWORK_GET_AUTOSTART;
proc(?REMOTE_PROC_NETWORK_GET_BRIDGE_NAME) -> network_get_bridge_name;
proc(network_get_bridge_name) -> ?REMOTE_PROC_NETWORK_GET_BRIDGE_NAME;
proc(?REMOTE_PROC_NETWORK_LOOKUP_BY_NAME) -> network_lookup_by_name;
proc(network_lookup_by_name) -> ?REMOTE_PROC_NETWORK_LOOKUP_BY_NAME;
proc(?REMOTE_PROC_NETWORK_LOOKUP_BY_UUID) -> network_lookup_by_uuid;
proc(network_lookup_by_uuid) -> ?REMOTE_PROC_NETWORK_LOOKUP_BY_UUID;
proc(?REMOTE_PROC_NETWORK_SET_AUTOSTART) -> network_set_autostart;
proc(network_set_autostart) -> ?REMOTE_PROC_NETWORK_SET_AUTOSTART;
proc(?REMOTE_PROC_NETWORK_UNDEFINE) -> network_undefine;
proc(network_undefine) -> ?REMOTE_PROC_NETWORK_UNDEFINE;
proc(?REMOTE_PROC_NUM_OF_DEFINED_NETWORKS) -> num_of_defined_networks;
proc(num_of_defined_networks) -> ?REMOTE_PROC_NUM_OF_DEFINED_NETWORKS;
proc(?REMOTE_PROC_NUM_OF_DOMAINS) -> num_of_domains;
proc(num_of_domains) -> ?REMOTE_PROC_NUM_OF_DOMAINS;
proc(?REMOTE_PROC_NUM_OF_NETWORKS) -> num_of_networks;
proc(num_of_networks) -> ?REMOTE_PROC_NUM_OF_NETWORKS;
proc(?REMOTE_PROC_DOMAIN_CORE_DUMP) -> domain_core_dump;
proc(domain_core_dump) -> ?REMOTE_PROC_DOMAIN_CORE_DUMP;
proc(?REMOTE_PROC_DOMAIN_RESTORE) -> domain_restore;
proc(domain_restore) -> ?REMOTE_PROC_DOMAIN_RESTORE;
proc(?REMOTE_PROC_DOMAIN_SAVE) -> domain_save;
proc(domain_save) -> ?REMOTE_PROC_DOMAIN_SAVE;
proc(?REMOTE_PROC_DOMAIN_GET_SCHEDULER_TYPE) -> domain_get_scheduler_type;
proc(domain_get_scheduler_type) -> ?REMOTE_PROC_DOMAIN_GET_SCHEDULER_TYPE;
proc(?REMOTE_PROC_DOMAIN_GET_SCHEDULER_PARAMETERS) -> domain_get_scheduler_parameters;
proc(domain_get_scheduler_parameters) -> ?REMOTE_PROC_DOMAIN_GET_SCHEDULER_PARAMETERS;
proc(?REMOTE_PROC_DOMAIN_SET_SCHEDULER_PARAMETERS) -> domain_set_scheduler_parameters;
proc(domain_set_scheduler_parameters) -> ?REMOTE_PROC_DOMAIN_SET_SCHEDULER_PARAMETERS;
proc(?REMOTE_PROC_GET_HOSTNAME) -> get_hostname;
proc(get_hostname) -> ?REMOTE_PROC_GET_HOSTNAME;
proc(?REMOTE_PROC_SUPPORTS_FEATURE) -> supports_feature;
proc(supports_feature) -> ?REMOTE_PROC_SUPPORTS_FEATURE;
proc(?REMOTE_PROC_DOMAIN_MIGRATE_PREPARE) -> domain_migrate_prepare;
proc(domain_migrate_prepare) -> ?REMOTE_PROC_DOMAIN_MIGRATE_PREPARE;
proc(?REMOTE_PROC_DOMAIN_MIGRATE_PERFORM) -> domain_migrate_perform;
proc(domain_migrate_perform) -> ?REMOTE_PROC_DOMAIN_MIGRATE_PERFORM;
proc(?REMOTE_PROC_DOMAIN_MIGRATE_FINISH) -> domain_migrate_finish;
proc(domain_migrate_finish) -> ?REMOTE_PROC_DOMAIN_MIGRATE_FINISH;
proc(?REMOTE_PROC_DOMAIN_BLOCK_STATS) -> domain_block_stats;
proc(domain_block_stats) -> ?REMOTE_PROC_DOMAIN_BLOCK_STATS;
proc(?REMOTE_PROC_DOMAIN_INTERFACE_STATS) -> domain_interface_stats;
proc(domain_interface_stats) -> ?REMOTE_PROC_DOMAIN_INTERFACE_STATS;
proc(?REMOTE_PROC_AUTH_LIST) -> auth_list;
proc(auth_list) -> ?REMOTE_PROC_AUTH_LIST;
proc(?REMOTE_PROC_AUTH_SASL_INIT) -> auth_sasl_init;
proc(auth_sasl_init) -> ?REMOTE_PROC_AUTH_SASL_INIT;
proc(?REMOTE_PROC_AUTH_SASL_START) -> auth_sasl_start;
proc(auth_sasl_start) -> ?REMOTE_PROC_AUTH_SASL_START;
proc(?REMOTE_PROC_AUTH_SASL_STEP) -> auth_sasl_step;
proc(auth_sasl_step) -> ?REMOTE_PROC_AUTH_SASL_STEP;
proc(?REMOTE_PROC_AUTH_POLKIT) -> auth_polkit;
proc(auth_polkit) -> ?REMOTE_PROC_AUTH_POLKIT;
proc(?REMOTE_PROC_NUM_OF_STORAGE_POOLS) -> num_of_storage_pools;
proc(num_of_storage_pools) -> ?REMOTE_PROC_NUM_OF_STORAGE_POOLS;
proc(?REMOTE_PROC_LIST_STORAGE_POOLS) -> list_storage_pools;
proc(list_storage_pools) -> ?REMOTE_PROC_LIST_STORAGE_POOLS;
proc(?REMOTE_PROC_NUM_OF_DEFINED_STORAGE_POOLS) -> num_of_defined_storage_pools;
proc(num_of_defined_storage_pools) -> ?REMOTE_PROC_NUM_OF_DEFINED_STORAGE_POOLS;
proc(?REMOTE_PROC_LIST_DEFINED_STORAGE_POOLS) -> list_defined_storage_pools;
proc(list_defined_storage_pools) -> ?REMOTE_PROC_LIST_DEFINED_STORAGE_POOLS;
proc(?REMOTE_PROC_FIND_STORAGE_POOL_SOURCES) -> find_storage_pool_sources;
proc(find_storage_pool_sources) -> ?REMOTE_PROC_FIND_STORAGE_POOL_SOURCES;
proc(?REMOTE_PROC_STORAGE_POOL_CREATE_XML) -> storage_pool_create_xml;
proc(storage_pool_create_xml) -> ?REMOTE_PROC_STORAGE_POOL_CREATE_XML;
proc(?REMOTE_PROC_STORAGE_POOL_DEFINE_XML) -> storage_pool_define_xml;
proc(storage_pool_define_xml) -> ?REMOTE_PROC_STORAGE_POOL_DEFINE_XML;
proc(?REMOTE_PROC_STORAGE_POOL_CREATE) -> storage_pool_create;
proc(storage_pool_create) -> ?REMOTE_PROC_STORAGE_POOL_CREATE;
proc(?REMOTE_PROC_STORAGE_POOL_BUILD) -> storage_pool_build;
proc(storage_pool_build) -> ?REMOTE_PROC_STORAGE_POOL_BUILD;
proc(?REMOTE_PROC_STORAGE_POOL_DESTROY) -> storage_pool_destroy;
proc(storage_pool_destroy) -> ?REMOTE_PROC_STORAGE_POOL_DESTROY;
proc(?REMOTE_PROC_STORAGE_POOL_DELETE) -> storage_pool_delete;
proc(storage_pool_delete) -> ?REMOTE_PROC_STORAGE_POOL_DELETE;
proc(?REMOTE_PROC_STORAGE_POOL_UNDEFINE) -> storage_pool_undefine;
proc(storage_pool_undefine) -> ?REMOTE_PROC_STORAGE_POOL_UNDEFINE;
proc(?REMOTE_PROC_STORAGE_POOL_REFRESH) -> storage_pool_refresh;
proc(storage_pool_refresh) -> ?REMOTE_PROC_STORAGE_POOL_REFRESH;
proc(?REMOTE_PROC_STORAGE_POOL_LOOKUP_BY_NAME) -> storage_pool_lookup_by_name;
proc(storage_pool_lookup_by_name) -> ?REMOTE_PROC_STORAGE_POOL_LOOKUP_BY_NAME;
proc(?REMOTE_PROC_STORAGE_POOL_LOOKUP_BY_UUID) -> storage_pool_lookup_by_uuid;
proc(storage_pool_lookup_by_uuid) -> ?REMOTE_PROC_STORAGE_POOL_LOOKUP_BY_UUID;
proc(?REMOTE_PROC_STORAGE_POOL_LOOKUP_BY_VOLUME) -> storage_pool_lookup_by_volume;
proc(storage_pool_lookup_by_volume) -> ?REMOTE_PROC_STORAGE_POOL_LOOKUP_BY_VOLUME;
proc(?REMOTE_PROC_STORAGE_POOL_GET_INFO) -> storage_pool_get_info;
proc(storage_pool_get_info) -> ?REMOTE_PROC_STORAGE_POOL_GET_INFO;
proc(?REMOTE_PROC_STORAGE_POOL_DUMP_XML) -> storage_pool_dump_xml;
proc(storage_pool_dump_xml) -> ?REMOTE_PROC_STORAGE_POOL_DUMP_XML;
proc(?REMOTE_PROC_STORAGE_POOL_GET_AUTOSTART) -> storage_pool_get_autostart;
proc(storage_pool_get_autostart) -> ?REMOTE_PROC_STORAGE_POOL_GET_AUTOSTART;
proc(?REMOTE_PROC_STORAGE_POOL_SET_AUTOSTART) -> storage_pool_set_autostart;
proc(storage_pool_set_autostart) -> ?REMOTE_PROC_STORAGE_POOL_SET_AUTOSTART;
proc(?REMOTE_PROC_STORAGE_POOL_NUM_OF_VOLUMES) -> storage_pool_num_of_volumes;
proc(storage_pool_num_of_volumes) -> ?REMOTE_PROC_STORAGE_POOL_NUM_OF_VOLUMES;
proc(?REMOTE_PROC_STORAGE_POOL_LIST_VOLUMES) -> storage_pool_list_volumes;
proc(storage_pool_list_volumes) -> ?REMOTE_PROC_STORAGE_POOL_LIST_VOLUMES;
proc(?REMOTE_PROC_STORAGE_VOL_CREATE_XML) -> storage_vol_create_xml;
proc(storage_vol_create_xml) -> ?REMOTE_PROC_STORAGE_VOL_CREATE_XML;
proc(?REMOTE_PROC_STORAGE_VOL_DELETE) -> storage_vol_delete;
proc(storage_vol_delete) -> ?REMOTE_PROC_STORAGE_VOL_DELETE;
proc(?REMOTE_PROC_STORAGE_VOL_LOOKUP_BY_NAME) -> storage_vol_lookup_by_name;
proc(storage_vol_lookup_by_name) -> ?REMOTE_PROC_STORAGE_VOL_LOOKUP_BY_NAME;
proc(?REMOTE_PROC_STORAGE_VOL_LOOKUP_BY_KEY) -> storage_vol_lookup_by_key;
proc(storage_vol_lookup_by_key) -> ?REMOTE_PROC_STORAGE_VOL_LOOKUP_BY_KEY;
proc(?REMOTE_PROC_STORAGE_VOL_LOOKUP_BY_PATH) -> storage_vol_lookup_by_path;
proc(storage_vol_lookup_by_path) -> ?REMOTE_PROC_STORAGE_VOL_LOOKUP_BY_PATH;
proc(?REMOTE_PROC_STORAGE_VOL_GET_INFO) -> storage_vol_get_info;
proc(storage_vol_get_info) -> ?REMOTE_PROC_STORAGE_VOL_GET_INFO;
proc(?REMOTE_PROC_STORAGE_VOL_DUMP_XML) -> storage_vol_dump_xml;
proc(storage_vol_dump_xml) -> ?REMOTE_PROC_STORAGE_VOL_DUMP_XML;
proc(?REMOTE_PROC_STORAGE_VOL_GET_PATH) -> storage_vol_get_path;
proc(storage_vol_get_path) -> ?REMOTE_PROC_STORAGE_VOL_GET_PATH;
proc(?REMOTE_PROC_NODE_GET_CELLS_FREE_MEMORY) -> node_get_cells_free_memory;
proc(node_get_cells_free_memory) -> ?REMOTE_PROC_NODE_GET_CELLS_FREE_MEMORY;
proc(?REMOTE_PROC_NODE_GET_FREE_MEMORY) -> node_get_free_memory;
proc(node_get_free_memory) -> ?REMOTE_PROC_NODE_GET_FREE_MEMORY;
proc(?REMOTE_PROC_DOMAIN_BLOCK_PEEK) -> domain_block_peek;
proc(domain_block_peek) -> ?REMOTE_PROC_DOMAIN_BLOCK_PEEK;
proc(?REMOTE_PROC_DOMAIN_MEMORY_PEEK) -> domain_memory_peek;
proc(domain_memory_peek) -> ?REMOTE_PROC_DOMAIN_MEMORY_PEEK;
proc(?REMOTE_PROC_DOMAIN_EVENTS_REGISTER) -> domain_events_register;
proc(domain_events_register) -> ?REMOTE_PROC_DOMAIN_EVENTS_REGISTER;
proc(?REMOTE_PROC_DOMAIN_EVENTS_DEREGISTER) -> domain_events_deregister;
proc(domain_events_deregister) -> ?REMOTE_PROC_DOMAIN_EVENTS_DEREGISTER;
proc(?REMOTE_PROC_DOMAIN_EVENT) -> domain_event;
proc(domain_event) -> ?REMOTE_PROC_DOMAIN_EVENT;
proc(?REMOTE_PROC_DOMAIN_MIGRATE_PREPARE2) -> domain_migrate_prepare2;
proc(domain_migrate_prepare2) -> ?REMOTE_PROC_DOMAIN_MIGRATE_PREPARE2;
proc(?REMOTE_PROC_DOMAIN_MIGRATE_FINISH2) -> domain_migrate_finish2;
proc(domain_migrate_finish2) -> ?REMOTE_PROC_DOMAIN_MIGRATE_FINISH2;
proc(?REMOTE_PROC_GET_URI) -> get_uri;
proc(get_uri) -> ?REMOTE_PROC_GET_URI;
proc(?REMOTE_PROC_NODE_NUM_OF_DEVICES) -> node_num_of_devices;
proc(node_num_of_devices) -> ?REMOTE_PROC_NODE_NUM_OF_DEVICES;
proc(?REMOTE_PROC_NODE_LIST_DEVICES) -> node_list_devices;
proc(node_list_devices) -> ?REMOTE_PROC_NODE_LIST_DEVICES;
proc(?REMOTE_PROC_NODE_DEVICE_LOOKUP_BY_NAME) -> node_device_lookup_by_name;
proc(node_device_lookup_by_name) -> ?REMOTE_PROC_NODE_DEVICE_LOOKUP_BY_NAME;
proc(?REMOTE_PROC_NODE_DEVICE_DUMP_XML) -> node_device_dump_xml;
proc(node_device_dump_xml) -> ?REMOTE_PROC_NODE_DEVICE_DUMP_XML;
proc(?REMOTE_PROC_NODE_DEVICE_GET_PARENT) -> node_device_get_parent;
proc(node_device_get_parent) -> ?REMOTE_PROC_NODE_DEVICE_GET_PARENT;
proc(?REMOTE_PROC_NODE_DEVICE_NUM_OF_CAPS) -> node_device_num_of_caps;
proc(node_device_num_of_caps) -> ?REMOTE_PROC_NODE_DEVICE_NUM_OF_CAPS;
proc(?REMOTE_PROC_NODE_DEVICE_LIST_CAPS) -> node_device_list_caps;
proc(node_device_list_caps) -> ?REMOTE_PROC_NODE_DEVICE_LIST_CAPS;
proc(?REMOTE_PROC_NODE_DEVICE_DETTACH) -> node_device_dettach;
proc(node_device_dettach) -> ?REMOTE_PROC_NODE_DEVICE_DETTACH;
proc(?REMOTE_PROC_NODE_DEVICE_RE_ATTACH) -> node_device_re_attach;
proc(node_device_re_attach) -> ?REMOTE_PROC_NODE_DEVICE_RE_ATTACH;
proc(?REMOTE_PROC_NODE_DEVICE_RESET) -> node_device_reset;
proc(node_device_reset) -> ?REMOTE_PROC_NODE_DEVICE_RESET;
proc(?REMOTE_PROC_DOMAIN_GET_SECURITY_LABEL) -> domain_get_security_label;
proc(domain_get_security_label) -> ?REMOTE_PROC_DOMAIN_GET_SECURITY_LABEL;
proc(?REMOTE_PROC_NODE_GET_SECURITY_MODEL) -> node_get_security_model;
proc(node_get_security_model) -> ?REMOTE_PROC_NODE_GET_SECURITY_MODEL;
proc(?REMOTE_PROC_NODE_DEVICE_CREATE_XML) -> node_device_create_xml;
proc(node_device_create_xml) -> ?REMOTE_PROC_NODE_DEVICE_CREATE_XML;
proc(?REMOTE_PROC_NODE_DEVICE_DESTROY) -> node_device_destroy;
proc(node_device_destroy) -> ?REMOTE_PROC_NODE_DEVICE_DESTROY;
proc(?REMOTE_PROC_STORAGE_VOL_CREATE_XML_FROM) -> storage_vol_create_xml_from;
proc(storage_vol_create_xml_from) -> ?REMOTE_PROC_STORAGE_VOL_CREATE_XML_FROM;
proc(?REMOTE_PROC_NUM_OF_INTERFACES) -> num_of_interfaces;
proc(num_of_interfaces) -> ?REMOTE_PROC_NUM_OF_INTERFACES;
proc(?REMOTE_PROC_LIST_INTERFACES) -> list_interfaces;
proc(list_interfaces) -> ?REMOTE_PROC_LIST_INTERFACES;
proc(?REMOTE_PROC_INTERFACE_LOOKUP_BY_NAME) -> interface_lookup_by_name;
proc(interface_lookup_by_name) -> ?REMOTE_PROC_INTERFACE_LOOKUP_BY_NAME;
proc(?REMOTE_PROC_INTERFACE_LOOKUP_BY_MAC_STRING) -> interface_lookup_by_mac_string;
proc(interface_lookup_by_mac_string) -> ?REMOTE_PROC_INTERFACE_LOOKUP_BY_MAC_STRING;
proc(?REMOTE_PROC_INTERFACE_GET_XML_DESC) -> interface_get_xml_desc;
proc(interface_get_xml_desc) -> ?REMOTE_PROC_INTERFACE_GET_XML_DESC;
proc(?REMOTE_PROC_INTERFACE_DEFINE_XML) -> interface_define_xml;
proc(interface_define_xml) -> ?REMOTE_PROC_INTERFACE_DEFINE_XML;
proc(?REMOTE_PROC_INTERFACE_UNDEFINE) -> interface_undefine;
proc(interface_undefine) -> ?REMOTE_PROC_INTERFACE_UNDEFINE;
proc(?REMOTE_PROC_INTERFACE_CREATE) -> interface_create;
proc(interface_create) -> ?REMOTE_PROC_INTERFACE_CREATE;
proc(?REMOTE_PROC_INTERFACE_DESTROY) -> interface_destroy;
proc(interface_destroy) -> ?REMOTE_PROC_INTERFACE_DESTROY;
proc(?REMOTE_PROC_DOMAIN_XML_FROM_NATIVE) -> domain_xml_from_native;
proc(domain_xml_from_native) -> ?REMOTE_PROC_DOMAIN_XML_FROM_NATIVE;
proc(?REMOTE_PROC_DOMAIN_XML_TO_NATIVE) -> domain_xml_to_native;
proc(domain_xml_to_native) -> ?REMOTE_PROC_DOMAIN_XML_TO_NATIVE;
proc(?REMOTE_PROC_NUM_OF_DEFINED_INTERFACES) -> num_of_defined_interfaces;
proc(num_of_defined_interfaces) -> ?REMOTE_PROC_NUM_OF_DEFINED_INTERFACES;
proc(?REMOTE_PROC_LIST_DEFINED_INTERFACES) -> list_defined_interfaces;
proc(list_defined_interfaces) -> ?REMOTE_PROC_LIST_DEFINED_INTERFACES;
proc(?REMOTE_PROC_NUM_OF_SECRETS) -> num_of_secrets;
proc(num_of_secrets) -> ?REMOTE_PROC_NUM_OF_SECRETS;
proc(?REMOTE_PROC_LIST_SECRETS) -> list_secrets;
proc(list_secrets) -> ?REMOTE_PROC_LIST_SECRETS;
proc(?REMOTE_PROC_SECRET_LOOKUP_BY_UUID) -> secret_lookup_by_uuid;
proc(secret_lookup_by_uuid) -> ?REMOTE_PROC_SECRET_LOOKUP_BY_UUID;
proc(?REMOTE_PROC_SECRET_DEFINE_XML) -> secret_define_xml;
proc(secret_define_xml) -> ?REMOTE_PROC_SECRET_DEFINE_XML;
proc(?REMOTE_PROC_SECRET_GET_XML_DESC) -> secret_get_xml_desc;
proc(secret_get_xml_desc) -> ?REMOTE_PROC_SECRET_GET_XML_DESC;
proc(?REMOTE_PROC_SECRET_SET_VALUE) -> secret_set_value;
proc(secret_set_value) -> ?REMOTE_PROC_SECRET_SET_VALUE;
proc(?REMOTE_PROC_SECRET_GET_VALUE) -> secret_get_value;
proc(secret_get_value) -> ?REMOTE_PROC_SECRET_GET_VALUE;
proc(?REMOTE_PROC_SECRET_UNDEFINE) -> secret_undefine;
proc(secret_undefine) -> ?REMOTE_PROC_SECRET_UNDEFINE;
proc(?REMOTE_PROC_SECRET_LOOKUP_BY_USAGE) -> secret_lookup_by_usage;
proc(secret_lookup_by_usage) -> ?REMOTE_PROC_SECRET_LOOKUP_BY_USAGE;
proc(?REMOTE_PROC_DOMAIN_MIGRATE_PREPARE_TUNNEL) -> domain_migrate_prepare_tunnel;
proc(domain_migrate_prepare_tunnel) -> ?REMOTE_PROC_DOMAIN_MIGRATE_PREPARE_TUNNEL;
proc(?REMOTE_PROC_IS_SECURE) -> is_secure;
proc(is_secure) -> ?REMOTE_PROC_IS_SECURE;
proc(?REMOTE_PROC_DOMAIN_IS_ACTIVE) -> domain_is_active;
proc(domain_is_active) -> ?REMOTE_PROC_DOMAIN_IS_ACTIVE;
proc(?REMOTE_PROC_DOMAIN_IS_PERSISTENT) -> domain_is_persistent;
proc(domain_is_persistent) -> ?REMOTE_PROC_DOMAIN_IS_PERSISTENT;
proc(?REMOTE_PROC_NETWORK_IS_ACTIVE) -> network_is_active;
proc(network_is_active) -> ?REMOTE_PROC_NETWORK_IS_ACTIVE;
proc(?REMOTE_PROC_NETWORK_IS_PERSISTENT) -> network_is_persistent;
proc(network_is_persistent) -> ?REMOTE_PROC_NETWORK_IS_PERSISTENT;
proc(?REMOTE_PROC_STORAGE_POOL_IS_ACTIVE) -> storage_pool_is_active;
proc(storage_pool_is_active) -> ?REMOTE_PROC_STORAGE_POOL_IS_ACTIVE;
proc(?REMOTE_PROC_STORAGE_POOL_IS_PERSISTENT) -> storage_pool_is_persistent;
proc(storage_pool_is_persistent) -> ?REMOTE_PROC_STORAGE_POOL_IS_PERSISTENT;
proc(?REMOTE_PROC_INTERFACE_IS_ACTIVE) -> interface_is_active;
proc(interface_is_active) -> ?REMOTE_PROC_INTERFACE_IS_ACTIVE;
proc(?REMOTE_PROC_GET_LIB_VERSION) -> get_lib_version;
proc(get_lib_version) -> ?REMOTE_PROC_GET_LIB_VERSION;
proc(?REMOTE_PROC_CPU_COMPARE) -> cpu_compare;
proc(cpu_compare) -> ?REMOTE_PROC_CPU_COMPARE;
proc(?REMOTE_PROC_DOMAIN_MEMORY_STATS) -> domain_memory_stats;
proc(domain_memory_stats) -> ?REMOTE_PROC_DOMAIN_MEMORY_STATS;

proc(_) -> unknown.

type(?REMOTE_CALL) -> call;
type(call) -> ?REMOTE_CALL;
type(?REMOTE_REPLY) -> reply;
type(reply) -> ?REMOTE_REPLY;
type(?REMOTE_MESSAGE) -> message;
type(message) -> ?REMOTE_MESSAGE;
type(?REMOTE_STREAM) -> stream;
type(stream) -> ?REMOTE_STREAM;

type(_) -> unknown.

status(?REMOTE_OK) -> ok;
status(ok) -> ?REMOTE_OK;
status(?REMOTE_ERROR) -> error;
status(error) -> ?REMOTE_ERROR;
status(?REMOTE_CONTINUE) -> continue;
status(continue) -> ?REMOTE_CONTINUE;

status(_) -> unknown.

