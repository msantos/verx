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
-module(verx_xdr).
-include("verx.hrl").

-export([
        encode/1,
        decode/1,

        arg/2,
        struct/2
    ]).
-export([
    remote_auth_type/0,
    remote_domain_memory_stat/0,
    remote_domain/0,
    remote_nonnull_domain/0,
    remote_interface/0,
    remote_nonnull_interface/0,
    remote_network/0,
    remote_nonnull_network/0,
    remote_node_device/0,
    remote_nonnull_node_device/0,
    remote_secret/0,
    remote_nonnull_secret/0,
    remote_storage_pool/0,
    remote_nonnull_storage_pool/0,
    remote_storage_vol/0,
    remote_nonnull_storage_vol/0,
    remote_string/0,
    remote_nonnull_string/0,
    remote_sched_param/0,
    remote_uuid/0,
    remote_vcpu_info/0,

    remote_error/0
]).

-define(STRUCT, [
    remote_auth_type,
    remote_domain_memory_stat,
    remote_nonnull_domain,
    remote_nonnull_interface,
    remote_nonnull_network,
    remote_nonnull_node_device,
    remote_nonnull_secret,
    remote_nonnull_storage_pool,
    remote_nonnull_storage_vol,
    remote_nonnull_string,
    remote_sched_param,
    remote_string,
    remote_uuid,
    remote_vcpu_info,

    remote_domain,
    remote_error
]).


%%-------------------------------------------------------------------------
%%% Encoding
%%-------------------------------------------------------------------------

%%
%% Basic XDR types
%%
% each byte is padded to 4 bytes
encode({Type, Buf}) when is_binary(Buf), ( Type == char orelse Type == uchar ) ->
    char_encode(Buf, []);
% fixed length buffer
encode({opaque, Buf}) when is_binary(Buf) ->
    Len = byte_size(Buf),
    Pad = pad(Len),
    <<Buf:Len/bytes, 0:Pad>>;
% Variable length buffer
encode({string, Buf}) when is_list(Buf) ->
    encode({string, list_to_binary(Buf)});
encode({string, Buf}) when is_binary(Buf) ->
    Len = byte_size(Buf),
    Pad = pad(Len),
    <<Len:32, Buf/bytes, 0:Pad>>;
encode({int, N}) when is_integer(N) ->
    <<N:4/signed-big-integer-unit:8>>;
encode({uint, N}) when is_integer(N) ->
    <<N:4/unsigned-big-integer-unit:8>>;
encode({hyper, N}) when is_integer(N) ->
    <<N:64/signed-big-integer-unit:8>>;
encode({uhyper, N}) when is_integer(N) ->
    <<N:64/unsigned-big-integer-unit:8>>;
% shorts will be padded to 4 bytes
encode({short, Buf}) when is_integer(Buf), Buf =< 16#FFFF ->
    encode({int, Buf});
encode({ushort, Buf}) when is_integer(Buf), Buf =< 16#FFFF ->
    encode({int, Buf});

encode({boolean, true}) ->
    <<1:32>>;
encode({boolean, false}) ->
    <<0:32>>;

encode({optional, Buf}) ->
    list_to_binary([<<1:32>>, Buf]);

%%
%% libivrt composite types
%%
encode({remote_uuid, Buf}) when is_binary(Buf), byte_size(Buf) == ?VIR_UUID_BUFLEN ->
    encode({opaque, Buf});

encode({Type, Struct}) ->
    verx_util:arg(Struct, ?MODULE:Type()).


%%-------------------------------------------------------------------------
%%% Decoding
%%-------------------------------------------------------------------------
decode({Type, {Buf, Len}}) when is_binary(Buf), ( Type == char orelse Type == uchar ) ->
    char_decode(Buf, Len, []);
decode({opaque, {Buf, Len}}) when is_binary(Buf) ->
    Pad = pad(Len),
    <<Bin:Len/bytes, 0:Pad, Rest/binary>> = Buf,
    {Bin, Rest};
decode({Type, <<Len:32, Buf/binary>>}) when Type == string; Type == opaque ->
    Pad = pad(Len),
    <<String:Len/bytes, 0:Pad, Rest/binary>> = Buf,
    {String, Rest};
decode({int, <<N:4/signed-big-integer-unit:8, Buf/binary>>}) ->
    {N, Buf};
decode({uint, <<N:4/unsigned-big-integer-unit:8, Buf/binary>>}) ->
    {N, Buf};
decode({hyper, <<N:8/signed-big-integer-unit:8, Buf/binary>>}) ->
    {N, Buf};
decode({uhyper, <<N:8/unsigned-big-integer-unit:8, Buf/binary>>}) ->
    {N, Buf};
decode({short, Buf}) when is_binary(Buf) ->
    decode({int, Buf});
decode({ushort, Buf}) when is_binary(Buf) ->
    decode({int, Buf});

decode({boolean, <<1:32, Buf/binary>>}) ->
    {true, Buf};
decode({boolean, <<0:32, Buf/binary>>}) ->
    {false, Buf};

%%
%% libivrt composite types
%%

% XXX Included structures seem to begin with <<1:32>>. Why?
% XXX How to handle the padding?? Look at the padding rules for structs
decode({remote_uuid, <<Buf/binary>>}) ->
    decode({opaque, {Buf, ?VIR_UUID_BUFLEN}});

decode({Type, <<Buf/binary>>}) ->
    struct(Buf, ?MODULE:Type()).


% Decode an XDR binary into a proplist
struct(Buf, Struct) when is_binary(Buf), is_list(Struct) ->
    struct1(Buf, Struct, []).

struct1(Buf, [], Acc) ->
    {lists:reverse(Acc), Buf};
struct1(Buf, [Field|Struct], Acc) when is_binary(Field) ->
    Len = bit_size(Field),
    <<Field:Len/bits, Rest/binary>> = Buf,
    struct1(Rest, Struct, Acc);
struct1(Buf, [{Field, {Type, Len}}|Struct], Acc) ->
    try verx_xdr:decode({Type, {Buf, Len}}) of
        {Val, <<>>} ->
            struct1(<<>>, [], [{Field, Val}|Acc]);
        {Val, Rest} ->
            struct1(Rest, Struct, [{Field, Val}|Acc])
    catch
        error:_ ->
            {error, Type, lists:reverse(Acc), Buf}
    end;
struct1(Buf, [{Field, Type}|Struct], Acc) ->
    try verx_xdr:decode({Type, Buf}) of
        {Val, <<>>} ->
            struct1(<<>>, [], [{Field, Val}|Acc]);
        {Val, Rest} ->
            struct1(Rest, Struct, [{Field, Val}|Acc]);
        % Can be returned by recursively included entries
        {error, _Field, _Acc, _Buf} = Error ->
            Error
    catch
        error:_ ->
            io:format("~p~n", [erlang:get_stacktrace()]),
            {error, Field, lists:reverse(Acc), Buf}
    end.


% Encode a proplist representation of a struct as an
% an XDR binary
arg(Struct, Template) ->
    case struct_types(Struct, Template) of
        ok ->
            struct_to_bin(Struct);
        {error, S, T} ->
            {error, {invalid_type, S, T}, Template}
    end.

struct_types([], []) ->
    ok;
struct_types([{Type, _Val}|Fields], [{_Field, Type}|Template]) ->
    struct_types(Fields, Template);
struct_types([{Type1, Val}|_Fields] = Struct, [{Field, Type2}|_Template]) ->
    case lists:member(Type2, ?STRUCT) of
        true ->
            struct_types(Struct, ?MODULE:Type2());
        false ->
            {error, {struct, Type1, Val}, {template, Type2, Field}}
    end.

struct_to_bin(Struct) ->
    list_to_binary([
        [ verx_xdr:encode(N) || N <- Struct ]
    ]).

%%-------------------------------------------------------------------------
%%% Utility function
%%-------------------------------------------------------------------------

char_encode(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
char_encode(<<Byte:1/bytes, Bytes/binary>>, Acc) ->
    char_encode(Bytes, [<<0:24, Byte/bytes>>|Acc]).

char_decode(Bytes, 0, Acc) ->
    {list_to_binary(lists:reverse(Acc)), Bytes};
char_decode(<<0:24, Byte:1/bytes, Bytes/binary>>, N, Acc) ->
    char_decode(Bytes, N-1, [Byte|Acc]);
char_decode(<<_:24, Byte:1/bytes, Bytes/binary>>, N, Acc) ->
    char_decode(Bytes, N-1, [Byte|Acc]).


% size in bits
pad(N) when N rem 4 == 0 -> 0;
pad(N) -> (4 - (N rem 4)) * 8.


%% Composite types
optional(Struct) ->
    [<<1:32>>] ++ Struct.

remote_uuid() ->
    [
        {uuid, {opaque, ?VIR_UUID_BUFLEN}}
    ].

remote_string() ->
    optional(remote_nonnull_string()).
remote_nonnull_string() ->
    [
        {string, string}
    ].

remote_auth_type() ->
    [
        {type, uint}
    ].

remote_domain_memory_stat() ->
    [
        {tag, int},
        {val, uhyper}
    ].

remote_domain() ->
    optional(remote_nonnull_domain()).
remote_nonnull_domain() ->
    [
        {name, remote_nonnull_string},
        {uuid, remote_uuid},
        {id, int}
    ].

remote_interface() ->
    optional(remote_nonnull_interface()).
remote_nonnull_interface() ->
    [
        {name, remote_nonnull_string},
        {mac, remote_nonnull_string}
    ].

remote_network() ->
    optional(remote_nonnull_network()).
remote_nonnull_network() ->
    [
        {name, remote_nonnull_string},
        {uuid, remote_uuid}
    ].

remote_node_device() ->
    optional(remote_nonnull_node_device()).
remote_nonnull_node_device() ->
    [
        {name, remote_nonnull_string}
    ].

remote_secret() ->
    optional(remote_nonnull_secret()).
remote_nonnull_secret() ->
    [
        {uuid, remote_uuid},
        {usageType, int},
        {usageID, remote_nonnull_string}
    ].

remote_storage_pool() ->
    optional(remote_nonnull_storage_pool()).
remote_nonnull_storage_pool() ->
    [
        {name, remote_nonnull_string},
        {uuid, remote_uuid}
    ].

remote_storage_vol() ->
    optional(remote_nonnull_storage_vol()).
remote_nonnull_storage_vol() ->
    [
        {pool, remote_nonnull_string},
        {name, remote_nonnull_string},
        {key, remote_nonnull_string}
    ].

remote_sched_param() ->
    [
        {field, remote_nonnull_string},
        {value, remote_sched_param_value}
    ].

remote_vcpu_info() ->
    [
        {number, uint},
        {state, int},
        {cpu_time, uhyper},
        {cpu, int}
    ].

remote_error() ->
    [
        {code, int},
        {domain, int},
        {message, remote_string},
        {level, int},
        <<0:32>>, % XXX padding
        {dom, remote_domain}

        % XXX deal with truncated structs
        %       {str1, remote_string},
        %       {str2, remote_string},
        %       {str3, remote_string},
        %       {int1, int},
        %       {int2, int},
        %       {net, remote_network}
    ].

