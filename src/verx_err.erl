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
-module(verx_err).
-export([level/1,from/1,code/1]).
-include("verx_err.hrl").

level(?VIR_ERR_NONE) -> none;
level(none) -> ?VIR_ERR_NONE;
level(?VIR_ERR_WARNING) -> warning;
level(warning) -> ?VIR_ERR_WARNING;
level(?VIR_ERR_ERROR) -> error;
level(error) -> ?VIR_ERR_ERROR;

level(_) -> unknown.


from(?VIR_FROM_NONE) -> none;
from(none) -> ?VIR_FROM_NONE;
from(?VIR_FROM_XEN) -> xen;
from(xen) -> ?VIR_FROM_XEN;
from(?VIR_FROM_XEND) -> xend;
from(xend) -> ?VIR_FROM_XEND;
from(?VIR_FROM_XENSTORE) -> xenstore;
from(xenstore) -> ?VIR_FROM_XENSTORE;
from(?VIR_FROM_SEXPR) -> sexpr;
from(sexpr) -> ?VIR_FROM_SEXPR;
from(?VIR_FROM_XML) -> xml;
from(xml) -> ?VIR_FROM_XML;
from(?VIR_FROM_DOM) -> dom;
from(dom) -> ?VIR_FROM_DOM;
from(?VIR_FROM_RPC) -> rpc;
from(rpc) -> ?VIR_FROM_RPC;
from(?VIR_FROM_PROXY) -> proxy;
from(proxy) -> ?VIR_FROM_PROXY;
from(?VIR_FROM_CONF) -> conf;
from(conf) -> ?VIR_FROM_CONF;
from(?VIR_FROM_QEMU) -> qemu;
from(qemu) -> ?VIR_FROM_QEMU;
from(?VIR_FROM_NET) -> net;
from(net) -> ?VIR_FROM_NET;
from(?VIR_FROM_TEST) -> test;
from(test) -> ?VIR_FROM_TEST;
from(?VIR_FROM_REMOTE) -> remote;
from(remote) -> ?VIR_FROM_REMOTE;
from(?VIR_FROM_OPENVZ) -> openvz;
from(openvz) -> ?VIR_FROM_OPENVZ;
from(?VIR_FROM_XENXM) -> xenxm;
from(xenxm) -> ?VIR_FROM_XENXM;
from(?VIR_FROM_STATS_LINUX) -> stats_linux;
from(stats_linux) -> ?VIR_FROM_STATS_LINUX;
from(?VIR_FROM_LXC) -> lxc;
from(lxc) -> ?VIR_FROM_LXC;
from(?VIR_FROM_STORAGE) -> storage;
from(storage) -> ?VIR_FROM_STORAGE;
from(?VIR_FROM_NETWORK) -> network;
from(network) -> ?VIR_FROM_NETWORK;
from(?VIR_FROM_DOMAIN) -> domain;
from(domain) -> ?VIR_FROM_DOMAIN;
from(?VIR_FROM_UML) -> uml;
from(uml) -> ?VIR_FROM_UML;
from(?VIR_FROM_NODEDEV) -> nodedev;
from(nodedev) -> ?VIR_FROM_NODEDEV;
from(?VIR_FROM_XEN_INOTIFY) -> xen_inotify;
from(xen_inotify) -> ?VIR_FROM_XEN_INOTIFY;
from(?VIR_FROM_SECURITY) -> security;
from(security) -> ?VIR_FROM_SECURITY;
from(?VIR_FROM_VBOX) -> vbox;
from(vbox) -> ?VIR_FROM_VBOX;
from(?VIR_FROM_INTERFACE) -> interface;
from(interface) -> ?VIR_FROM_INTERFACE;
from(?VIR_FROM_ONE) -> one;
from(one) -> ?VIR_FROM_ONE;
from(?VIR_FROM_ESX) -> esx;
from(esx) -> ?VIR_FROM_ESX;
from(?VIR_FROM_PHYP) -> phyp;
from(phyp) -> ?VIR_FROM_PHYP;
from(?VIR_FROM_SECRET) -> secret;
from(secret) -> ?VIR_FROM_SECRET;
from(?VIR_FROM_CPU) -> cpu;
from(cpu) -> ?VIR_FROM_CPU;

from(_) -> unknown.


code(?VIR_ERR_OK) -> ok;
code(ok) -> ?VIR_ERR_OK;
code(?VIR_ERR_INTERNAL_ERROR) -> internal_error;
code(internal_error) -> ?VIR_ERR_INTERNAL_ERROR;
code(?VIR_ERR_NO_MEMORY) -> no_memory;
code(no_memory) -> ?VIR_ERR_NO_MEMORY;
code(?VIR_ERR_NO_SUPPORT) -> no_support;
code(no_support) -> ?VIR_ERR_NO_SUPPORT;
code(?VIR_ERR_UNKNOWN_HOST) -> unknown_host;
code(unknown_host) -> ?VIR_ERR_UNKNOWN_HOST;
code(?VIR_ERR_NO_CONNECT) -> no_connect;
code(no_connect) -> ?VIR_ERR_NO_CONNECT;
code(?VIR_ERR_INVALID_CONN) -> invalid_conn;
code(invalid_conn) -> ?VIR_ERR_INVALID_CONN;
code(?VIR_ERR_INVALID_DOMAIN) -> invalid_domain;
code(invalid_domain) -> ?VIR_ERR_INVALID_DOMAIN;
code(?VIR_ERR_INVALID_ARG) -> invalid_arg;
code(invalid_arg) -> ?VIR_ERR_INVALID_ARG;
code(?VIR_ERR_OPERATION_FAILED) -> operation_failed;
code(operation_failed) -> ?VIR_ERR_OPERATION_FAILED;
code(?VIR_ERR_GET_FAILED) -> get_failed;
code(get_failed) -> ?VIR_ERR_GET_FAILED;
code(?VIR_ERR_POST_FAILED) -> post_failed;
code(post_failed) -> ?VIR_ERR_POST_FAILED;
code(?VIR_ERR_HTTP_ERROR) -> http_error;
code(http_error) -> ?VIR_ERR_HTTP_ERROR;
code(?VIR_ERR_SEXPR_SERIAL) -> sexpr_serial;
code(sexpr_serial) -> ?VIR_ERR_SEXPR_SERIAL;
code(?VIR_ERR_NO_XEN) -> no_xen;
code(no_xen) -> ?VIR_ERR_NO_XEN;
code(?VIR_ERR_XEN_CALL) -> xen_call;
code(xen_call) -> ?VIR_ERR_XEN_CALL;
code(?VIR_ERR_OS_TYPE) -> os_type;
code(os_type) -> ?VIR_ERR_OS_TYPE;
code(?VIR_ERR_NO_KERNEL) -> no_kernel;
code(no_kernel) -> ?VIR_ERR_NO_KERNEL;
code(?VIR_ERR_NO_ROOT) -> no_root;
code(no_root) -> ?VIR_ERR_NO_ROOT;
code(?VIR_ERR_NO_SOURCE) -> no_source;
code(no_source) -> ?VIR_ERR_NO_SOURCE;
code(?VIR_ERR_NO_TARGET) -> no_target;
code(no_target) -> ?VIR_ERR_NO_TARGET;
code(?VIR_ERR_NO_NAME) -> no_name;
code(no_name) -> ?VIR_ERR_NO_NAME;
code(?VIR_ERR_NO_OS) -> no_os;
code(no_os) -> ?VIR_ERR_NO_OS;
code(?VIR_ERR_NO_DEVICE) -> no_device;
code(no_device) -> ?VIR_ERR_NO_DEVICE;
code(?VIR_ERR_NO_XENSTORE) -> no_xenstore;
code(no_xenstore) -> ?VIR_ERR_NO_XENSTORE;
code(?VIR_ERR_DRIVER_FULL) -> driver_full;
code(driver_full) -> ?VIR_ERR_DRIVER_FULL;
code(?VIR_ERR_CALL_FAILED) -> call_failed;
code(call_failed) -> ?VIR_ERR_CALL_FAILED;
code(?VIR_ERR_XML_ERROR) -> xml_error;
code(xml_error) -> ?VIR_ERR_XML_ERROR;
code(?VIR_ERR_DOM_EXIST) -> dom_exist;
code(dom_exist) -> ?VIR_ERR_DOM_EXIST;
code(?VIR_ERR_OPERATION_DENIED) -> operation_denied;
code(operation_denied) -> ?VIR_ERR_OPERATION_DENIED;
code(?VIR_ERR_OPEN_FAILED) -> open_failed;
code(open_failed) -> ?VIR_ERR_OPEN_FAILED;
code(?VIR_ERR_READ_FAILED) -> read_failed;
code(read_failed) -> ?VIR_ERR_READ_FAILED;
code(?VIR_ERR_PARSE_FAILED) -> parse_failed;
code(parse_failed) -> ?VIR_ERR_PARSE_FAILED;
code(?VIR_ERR_CONF_SYNTAX) -> conf_syntax;
code(conf_syntax) -> ?VIR_ERR_CONF_SYNTAX;
code(?VIR_ERR_WRITE_FAILED) -> write_failed;
code(write_failed) -> ?VIR_ERR_WRITE_FAILED;
code(?VIR_ERR_XML_DETAIL) -> xml_detail;
code(xml_detail) -> ?VIR_ERR_XML_DETAIL;
code(?VIR_ERR_INVALID_NETWORK) -> invalid_network;
code(invalid_network) -> ?VIR_ERR_INVALID_NETWORK;
code(?VIR_ERR_NETWORK_EXIST) -> network_exist;
code(network_exist) -> ?VIR_ERR_NETWORK_EXIST;
code(?VIR_ERR_SYSTEM_ERROR) -> system_error;
code(system_error) -> ?VIR_ERR_SYSTEM_ERROR;
code(?VIR_ERR_RPC) -> rpc;
code(rpc) -> ?VIR_ERR_RPC;
code(?VIR_ERR_GNUTLS_ERROR) -> gnutls_error;
code(gnutls_error) -> ?VIR_ERR_GNUTLS_ERROR;
code(?VIR_ERR_NO_DOMAIN) -> no_domain;
code(no_domain) -> ?VIR_ERR_NO_DOMAIN;
code(?VIR_ERR_NO_NETWORK) -> no_network;
code(no_network) -> ?VIR_ERR_NO_NETWORK;
code(?VIR_ERR_INVALID_MAC) -> invalid_mac;
code(invalid_mac) -> ?VIR_ERR_INVALID_MAC;
code(?VIR_ERR_AUTH_FAILED) -> auth_failed;
code(auth_failed) -> ?VIR_ERR_AUTH_FAILED;
code(?VIR_ERR_INVALID_STORAGE_POOL) -> invalid_storage_pool;
code(invalid_storage_pool) -> ?VIR_ERR_INVALID_STORAGE_POOL;
code(?VIR_ERR_INVALID_STORAGE_VOL) -> invalid_storage_vol;
code(invalid_storage_vol) -> ?VIR_ERR_INVALID_STORAGE_VOL;
code(?VIR_ERR_NO_STORAGE_POOL) -> no_storage_pool;
code(no_storage_pool) -> ?VIR_ERR_NO_STORAGE_POOL;
code(?VIR_ERR_NO_STORAGE_VOL) -> no_storage_vol;
code(no_storage_vol) -> ?VIR_ERR_NO_STORAGE_VOL;
code(?VIR_ERR_INVALID_NODE_DEVICE) -> invalid_node_device;
code(invalid_node_device) -> ?VIR_ERR_INVALID_NODE_DEVICE;
code(?VIR_ERR_NO_NODE_DEVICE) -> no_node_device;
code(no_node_device) -> ?VIR_ERR_NO_NODE_DEVICE;
code(?VIR_ERR_NO_SECURITY_MODEL) -> no_security_model;
code(no_security_model) -> ?VIR_ERR_NO_SECURITY_MODEL;
code(?VIR_ERR_OPERATION_INVALID) -> operation_invalid;
code(operation_invalid) -> ?VIR_ERR_OPERATION_INVALID;
code(?VIR_ERR_NO_INTERFACE) -> no_interface;
code(no_interface) -> ?VIR_ERR_NO_INTERFACE;
code(?VIR_ERR_INVALID_INTERFACE) -> invalid_interface;
code(invalid_interface) -> ?VIR_ERR_INVALID_INTERFACE;
code(?VIR_ERR_MULTIPLE_INTERFACES) -> multiple_interfaces;
code(multiple_interfaces) -> ?VIR_ERR_MULTIPLE_INTERFACES;
code(?VIR_ERR_INVALID_SECRET) -> invalid_secret;
code(invalid_secret) -> ?VIR_ERR_INVALID_SECRET;
code(?VIR_ERR_NO_SECRET) -> no_secret;
code(no_secret) -> ?VIR_ERR_NO_SECRET;
code(?VIR_ERR_CONFIG_UNSUPPORTED) -> config_unsupported;
code(config_unsupported) -> ?VIR_ERR_CONFIG_UNSUPPORTED;
code(?VIR_ERR_OPERATION_TIMEOUT) -> operation_timeout;
code(operation_timeout) -> ?VIR_ERR_OPERATION_TIMEOUT;
code(?VIR_ERR_MIGRATE_PERSIST_FAILED) -> migrate_persist_failed;
code(migrate_persist_failed) -> ?VIR_ERR_MIGRATE_PERSIST_FAILED;

code(_) -> unknown.

