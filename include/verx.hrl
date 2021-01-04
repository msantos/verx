%% Copyright (c) 2010-2021, Michael Santos <michael.santos@gmail.com>
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
-include_lib("verx/include/remote_protocol.hrl").

-define(LIBVIRT_SOCK_PATH, <<"/var/run/libvirt/libvirt-sock">>).
-define(LIBVIRT_TCP_PORT, 16509).
-define(LIBVIRT_TLS_PORT, 16514).

-define(UINT32(N), (N):4 / unsigned - integer - unit:8).
-define(UINT64(N), (N):8 / unsigned - integer - unit:8).
-define(INT32(N), (N):4 / signed - integer - unit:8).
-define(INT64(N), (N):8 / signed - integer - unit:8).

-define(REMOTE_MESSAGE_HEADER_MAX, 24).
-define(REMOTE_MESSAGE_PAYLOAD_MAX, 262120).
-define(REMOTE_MESSAGE_HEADER_XDR_LEN, 4).

%% remote_message_type

% client -> server. args from a method call
-define(REMOTE_CALL, 0).
% server -> client. reply/error from a method call
-define(REMOTE_REPLY, 1).
% either direction. async notification
-define(REMOTE_MESSAGE, 2).
% either direction. stream data packet
-define(REMOTE_STREAM, 3).
% client -> server. args from a method call, with passed FDs
-define(REMOTE_CALL_WITH_FDS, 4).
% server -> client. reply/error from a method call, with passed FDs
-define(REMOTE_REPLY_WITH_FDS, 5).

%% remote_message_status
-define(REMOTE_OK, 0).
-define(REMOTE_ERROR, 1).
-define(REMOTE_CONTINUE, 2).

%% Message format:
%%  4 bytes: total length of message in bytes (incl. length)
%%  24 bytes: header
%%      4 bytes: program
%%      4 bytes: version
%%      4 bytes: procedure
%%      4 bytes: type
%%      4 bytes: serial
%%      4 bytes: status
%%  Variable: procedure arguments
-record(remote_message_header, {
    prog = <<?REMOTE_PROGRAM:32>>,
    vers = <<?REMOTE_PROTOCOL_VERSION:32>>,
    proc = <<0:32>>,
    type = <<?REMOTE_CALL:32>>,
    serial = <<0:32>>,
    status = <<?REMOTE_OK:32>>
}).
