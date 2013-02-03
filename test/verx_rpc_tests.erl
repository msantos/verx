%% Copyright (c) 2012-2013, Michael Santos <michael.santos@gmail.com>
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
-module(verx_rpc_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("verx.hrl").

args_open_encode_decode_test() ->
    % Use <<>> instead "" so the decode will match up
    Args = [<<>>, 0],

    {Header, Payload} = verx_rpc:call('REMOTE_PROC_OPEN', Args),
    Msg = verx_rpc:encode({Header, Payload}),

    {Header, Args} = verx_rpc:decode(Msg).

args_domain_create_xml_encode_decode_test() ->
    UUID = <<"97da79eb-5b19-7be8-cb76-a1acff00e4d3">>,
    Args = [UUID, byte_size(UUID)],

    {Header, Enc} = verx_rpc:call('REMOTE_PROC_DOMAIN_CREATE_XML', Args),
    Msg = verx_rpc:encode({Header, Enc}),

    {Header, Args} = verx_rpc:decode(Msg).

% Composite type: enc_remote_nonnull_domain composed of
%   enc_remote_nonnull_string, enc_remote_uuid, length of string
ret_domain_create_xml_encode_decode_test() ->
    UUIDString = <<"97da79eb-5b19-7be8-cb76-a1acff00e4d3">>,
    UUID = <<1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6>>, % 16 bytes

    % an XDR remote_nonnull_domain type
    Args = [{UUIDString, UUID, byte_size(UUIDString)}],

    {Header, Enc} = verx_rpc:reply('REMOTE_PROC_DOMAIN_CREATE_XML', Args),
    Msg = verx_rpc:encode({Header, Enc}),

    {Header, Args} = verx_rpc:decode(Msg).
