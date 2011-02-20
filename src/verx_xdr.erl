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
    param/1, struct_to_proplist/1
]).

-define(STRUCT, [
    remote_domain_memory_stat,
    remote_nonnull_domain,
    remote_nonnull_interface,
    remote_nonnull_network,
    remote_nonnull_node_device,
    remote_nonnull_secret,
    remote_nonnull_storage_pool,
    remote_nonnull_storage_vol,
    remote_sched_param,
    remote_vcpu_info,

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
    <<N:8/signed-big-integer-unit:8>>;
encode({uhyper, N}) when is_integer(N) ->
    <<N:8/unsigned-big-integer-unit:8>>;
% shorts will be padded to 4 bytes
encode({short, Buf}) when is_integer(Buf), Buf =< 16#FFFF ->
    encode({int, Buf});
encode({ushort, Buf}) when is_integer(Buf), Buf =< 16#FFFF ->
    encode({int, Buf});

encode({boolean, true}) ->
    <<1:32>>;
encode({boolean, false}) ->
    <<0:32>>;

encode({optional_data, {_Type, Term}}) when Term == false; Term == <<>> ->
    encode({boolean, false});
encode({optional_data, {Type, Buf}}) ->
    list_to_binary([
        encode({boolean, true}),
        encode({Type, Buf})
    ]);

%%
%% libivrt composite types
%%
encode({remote_auth_type, N}) when is_integer(N) ->
    encode({uint, N});

encode({remote_uuid, Buf}) when is_binary(Buf), byte_size(Buf) == ?VIR_UUID_BUFLEN ->
    encode({opaque, Buf});

encode({remote_string, Buf}) ->
    encode({optional_data, {remote_nonnull_string, Buf}});
encode({remote_nonnull_string, Buf}) ->
    encode({string, Buf});

encode({remote_domain, Buf}) ->
    encode({optional_data, {remote_nonnull_domain, Buf}});

encode({remote_interface, Buf}) ->
    encode({optional_data, {remote_nonnull_interface, Buf}});

encode({remote_network, Buf}) ->
    encode({optional_data, {remote_nonnull_network, Buf}});

encode({remote_node_device, Buf}) ->
    encode({optional_data, {remote_nonnull_device, Buf}});

encode({remote_secret, Buf}) ->
    encode({optional_data, {remote_nonnull_secret, Buf}});

encode({remote_storage_pool, Buf}) ->
    encode({optional_data, {remote_nonnull_storage_pool, Buf}});

encode({remote_storage_vol, Buf}) ->
    encode({optional_data, {remote_nonnull_storage_vol, Buf}});

encode({Type, Struct}) ->
    arg(Struct, param(Type)).


%%-------------------------------------------------------------------------
%%% Decoding
%%-------------------------------------------------------------------------
decode({Type, {Buf, Len}}) when is_binary(Buf), ( Type == char orelse Type == uchar ) ->
    {Char, Rest} = char_decode(Buf, Len, []),
    {{Type, Char}, Rest};
decode({opaque, {Buf, Len}}) when is_binary(Buf) ->
    Pad = pad(Len),
    <<Bin:Len/bytes, 0:Pad, Rest/binary>> = Buf,
    {{opaque, Bin}, Rest};
decode({Type, <<Len:32, Buf/binary>>}) when Type == string; Type == opaque ->
    Pad = pad(Len),
    <<String:Len/bytes, 0:Pad, Rest/binary>> = Buf,
    {{string, String}, Rest};
decode({int, <<N:4/signed-big-integer-unit:8, Buf/binary>>}) ->
    {{int, N}, Buf};
decode({uint, <<N:4/unsigned-big-integer-unit:8, Buf/binary>>}) ->
    {{uint, N}, Buf};
decode({hyper, <<N:8/signed-big-integer-unit:8, Buf/binary>>}) ->
    {{hyper, N}, Buf};
decode({uhyper, <<N:8/unsigned-big-integer-unit:8, Buf/binary>>}) ->
    {{uhyper, N}, Buf};
decode({short, Buf}) when is_binary(Buf) ->
    decode({int, Buf});
decode({ushort, Buf}) when is_binary(Buf) ->
    decode({int, Buf});

decode({boolean, <<1:32, Buf/binary>>}) ->
    {{boolean, true}, Buf};
decode({boolean, <<0:32, Buf/binary>>}) ->
    {{boolean, false}, Buf};

decode({optional_data, {{_Type, <<>>}, _Buf} = Type}) ->
    Type;
decode({optional_data, {Type, Buf}}) ->
    case decode({boolean, Buf}) of
        {{boolean, true}, Buf1} ->
            decode({Type, Buf1});
        {{boolean, false}, Buf1} ->
            {{Type, <<>>}, Buf1}
    end;

%%
%% libivrt composite types
%%
decode({remote_auth_type, <<Buf/binary>>}) ->
    composite(remote_auth_type, decode({uint, Buf}));

decode({remote_uuid, <<Buf/binary>>}) ->
    composite(remote_uuid, decode({opaque, {Buf, ?VIR_UUID_BUFLEN}}));

decode({remote_string, Buf}) ->
    composite(remote_string, decode({optional_data, {remote_nonnull_string, Buf}}));
decode({remote_nonnull_string, Buf}) ->
    composite(remote_nonnull_string, decode({string, Buf}));

decode({remote_domain, Buf}) ->
    composite(remote_domain, decode({optional_data, {remote_nonnull_domain, Buf}}));

decode({remote_interface, Buf}) ->
    composite(remote_interface, decode({optional_data, {remote_nonnull_interface, Buf}}));

decode({remote_network, Buf}) ->
    composite(remote_network, decode({optional_data, {remote_nonnull_network, Buf}}));

decode({remote_node_device, Buf}) ->
    composite(remote_node_device, decode({optional_data, {remote_nonnull_node_device, Buf}}));

decode({remote_secret, Buf}) ->
    composite(remote_secret, decode({optional_data, {remote_nonnull_secret, Buf}}));

decode({remote_storage_pool, Buf}) ->
    composite(remote_storage_pool, decode({optional_data, {remote_nonnull_storage_pool, Buf}}));

decode({remote_storage_vol, Buf}) ->
    composite(remote_storage_vol, decode({optional_data, {remote_nonnull_storage_vol, Buf}}));

decode({Type, <<Buf/binary>>}) ->
    case struct(Buf, param(Type)) of
        {error, _, _, _} = Error ->
            Error;
        {Struct, Rest} ->
            {{Type, Struct}, Rest}
    end.

composite(CompositeType, {{_Type, Value}, Buf}) ->
    {{CompositeType, Value}, Buf}.


% Decode an XDR binary into a proplist
struct(Buf, Struct) when is_binary(Buf), is_list(Struct) ->
    struct1(Buf, Struct, []).

struct1(Buf, [], Acc) ->
    {lists:reverse(Acc), Buf};
struct1(Buf, [Field|Struct], Acc) when is_binary(Field) ->
    Len = bit_size(Field),
    <<Field:Len/bits, Rest/binary>> = Buf,
    struct1(Rest, Struct, Acc);
struct1(Buf, [{Field, Type}|Struct], Acc) ->
    try struct_decode(Type, Buf) of
        {Val, <<>>} ->
            struct1(<<>>, [], [Val|Acc]);
        {Val, Rest} ->
            struct1(Rest, Struct, [Val|Acc]);
        % Can be returned by recursively included entries
        {error, _Field, _Acc, _Buf} = Error ->
            Error
    catch
        error:_ ->
            {error, {Type, Field}, lists:reverse(Acc), Buf}
    end.

struct_decode({Type, Len}, Buf) ->
    verx_xdr:decode({Type, {Buf, Len}});
struct_decode(Type, Buf) ->
    verx_xdr:decode({Type, Buf}).


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
            struct_types(Struct, param(Type2));
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
param(remote_domain_memory_stat) ->
    [
        {tag, int},
        {val, uhyper}
    ];

param(remote_nonnull_domain) ->
    [
        {name, remote_nonnull_string},
        {uuid, remote_uuid},
        {id, int}
    ];

param(remote_nonnull_interface) ->
    [
        {name, remote_nonnull_string},
        {mac, remote_nonnull_string}
    ];

param(remote_nonnull_network) ->
    [
        {name, remote_nonnull_string},
        {uuid, remote_uuid}
    ];

param(remote_nonnull_node_device) ->
    [
        {name, remote_nonnull_string}
    ];

param(remote_nonnull_secret) ->
    [
        {uuid, remote_uuid},
        {usageType, int},
        {usageID, remote_nonnull_string}
    ];

param(remote_nonnull_storage_pool) ->
    [
        {name, remote_nonnull_string},
        {uuid, remote_uuid}
    ];

param(remote_nonnull_storage_vol) ->
    [
        {pool, remote_nonnull_string},
        {name, remote_nonnull_string},
        {key, remote_nonnull_string}
    ];

param(remote_sched_param) ->
    [
        {field, remote_nonnull_string},
        {value, remote_sched_param_value}
    ];

param(remote_vcpu_info) ->
    [
        {number, uint},
        {state, int},
        {cpu_time, uhyper},
        {cpu, int}
    ];

param(remote_error) ->
    [
        {code, int},
        {domain, int},
        {message, remote_string},
        {level, int},
        {dom, remote_domain},

        {str1, remote_string},
        {str2, remote_string},
        {str3, remote_string},
        {int1, int},
        {int2, int},
        {net, remote_network}
    ];

param(_) ->
    {error, unsupported}.


struct_to_proplist({Type, Val}) ->
    case param(Type) of
        {error, _} = Err -> Err;
        N -> proplist(Val, N, [])
    end.

proplist([], [], Acc) ->
    lists:reverse(Acc);
proplist([{Type, [Val|_] = Vals}|T1], [{Field, Type}|T2], Acc) when is_tuple(Val) ->
    case struct_to_proplist({Type, Vals}) of
        {error, _} ->
            {error, {Type, Field}, lists:reverse(Acc)};
        L ->
            proplist(T1, T2, [{Field, L}|Acc])
    end;
proplist([{Type, Val}|T1], [{Field, Type}|T2], Acc) ->
    proplist(T1, T2, [{Field, Val}|Acc]).

