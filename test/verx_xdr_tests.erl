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
-module(verx_xdr_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").
-include("verx.hrl").


int_encode_decode_test() ->
    N = <<0,0,0,12>>,
    N = verx_xdr:encode({int, 12}),
    {{int,12},<<>>} = verx_xdr:decode({int, N}),

    N = verx_xdr:encode({ushort, 12}),
    {{ushort,12},<<>>} = verx_xdr:decode({ushort, N}).

ints_decode_test() ->
    {{int,[1,2,3,4,5,6]},<<>>} = verx_xdr:decode({int, {
                <<0,0,0,6,
                0,0,0,1,
                0,0,0,2,
                0,0,0,3,
                0,0,0,4,
                0,0,0,5,
                0,0,0,6>>, 10}}).

simple_encode_decode_test() ->
    Struct = {remote_vcpu_info, [
            {uint, 1},
            {int, 2},
            {uhyper,1000},
            {int, 10}
        ]},
    Bin = verx_xdr:encode(Struct),
    {Struct, <<>>} = verx_xdr:decode({remote_vcpu_info, Bin}).

composite_encode_decode_test() ->
    Struct = {remote_error, [
            {int, 0},
            {int, 10},
            {remote_string, <<"This is a message">>},
            {int, 1},
            {remote_domain, [
                    {remote_nonnull_string, <<"localvm">>},
                    {remote_uuid, <<152,218,121,235,91,25,123,232,202,118,161,172,255,0,228,210>>},
                    {int, 31337}
                ]},
            {remote_string, <<>>},
            {remote_string, <<>>},
            {remote_string, <<>>},
            {int, 0},
            {int, 0},
            {remote_network, <<>>}
        ]},
    Bin = verx_xdr:encode(Struct),
    {Struct, <<>>} = verx_xdr:decode({remote_error, Bin}).
    %{Struct1, <<>>} = verx_xdr:decode({remote_error, Bin}),
    %?debugFmt("~p~n~p~n", [Struct, Struct1]).
