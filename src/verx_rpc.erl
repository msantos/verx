%% Copyright (c) 2011-2013, Michael Santos <michael.santos@gmail.com>
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
-module(verx_rpc).
-include("verx.hrl").

-export([
    call/1, call/2,
    reply/1, reply/2,

    encode/1,
    decode/1,
    header/1,

    field/2, status/1
    ]).


%%-------------------------------------------------------------------------
%%% API
%%-------------------------------------------------------------------------

call(Proc) ->
    call(Proc, []).

call(Proc, Arg) when is_atom(Proc) ->
    Call = remote_protocol_xdr:enc_remote_procedure(Proc),
    CallArg = payload(proc_to_call(Proc, "enc", "args"), Arg),
    Header = #remote_message_header{proc = Call},
    {Header, CallArg}.

reply(Proc) ->
    call(Proc, []).

reply(Proc, Arg) when is_atom(Proc) ->
    Call = remote_protocol_xdr:enc_remote_procedure(Proc),
    CallArg = payload(proc_to_call(Proc, "enc", "ret"), Arg),
    Header = #remote_message_header{proc = Call, type = <<?REMOTE_REPLY:32>>},
    {Header, CallArg}.


%%-------------------------------------------------------------------------
%%% Encode/decode RPC messages
%%-------------------------------------------------------------------------

% The packet if prefaced by the length of the packet:
%
% 4 bytes : packet length (including the length)
% 24 bytes : header (6 unsigned integers)
% Length = 4 + 24 + iolist_size(CallArg),
%
% It is up to the transport layer to add the 4 byte length.
%
encode({#remote_message_header{} = Header, CallArg}) ->
    encode({header(Header), CallArg});
encode({Header, CallArg}) ->
    iolist_to_binary([
            Header,
            CallArg
            ]).

decode(<<Header:?REMOTE_MESSAGE_HEADER_MAX/bytes, Rest/binary>>) ->
    decode(header(Header), Rest).

% REMOTE_CALL
decode(#remote_message_header{proc = Proc0,
                              type = <<?REMOTE_CALL:32>>,
                              status = <<?REMOTE_OK:32>>
                             } = Header, Rest) ->
    {Proc, 4} = remote_protocol_xdr:dec_remote_procedure(Proc0, 0),
    Args = proc_to_call(Proc, "dec", "args"),
    {Header, decode_payload(Args, Rest)};

% REMOTE_REPLY
decode(#remote_message_header{proc = Proc0,
                              type = <<?REMOTE_REPLY:32>>,
                              status = <<?REMOTE_OK:32>>} = Header, Rest) ->
    {Proc, 4} = remote_protocol_xdr:dec_remote_procedure(Proc0, 0),
    Ret = proc_to_call(Proc, "dec", "ret"),
    {Header, decode_payload(Ret, Rest)};
decode(#remote_message_header{type = <<?REMOTE_REPLY:32>>,
                              status = <<?REMOTE_ERROR:32>>} = Header, Rest) ->
    {Header, decode_payload(dec_remote_error, Rest)};

% REMOTE_MESSAGE
decode(#remote_message_header{proc = Proc0,
                              type = <<?REMOTE_MESSAGE:32>>,
                              status = <<?REMOTE_OK:32>>} = Header, Rest) ->
    {Proc, 4} = remote_protocol_xdr:dec_remote_procedure(Proc0, 0),
    Ret = proc_to_call(Proc, "dec", "msg"),
    {Header, decode_payload(Ret, Rest)};

% REMOTE_STREAM
% contains binary data
decode(#remote_message_header{type = <<?REMOTE_STREAM:32>>,
                              status = <<?REMOTE_CONTINUE:32>>} = Header, Rest) ->
    {Header, Rest};
% error
decode(#remote_message_header{type = <<?REMOTE_STREAM:32>>,
                              status = <<?REMOTE_ERROR:32>>} = Header, Rest) ->
    {Header, decode_payload(dec_remote_error, Rest)};
% eof
decode(#remote_message_header{type = <<?REMOTE_STREAM:32>>,
                              status = <<?REMOTE_OK:32>>} = Header, <<>>) ->
    {Header, []};

% REMOTE_CALL_WITH_FDS
decode(#remote_message_header{proc = Proc0,
                              type = <<?REMOTE_CALL_WITH_FDS:32>>} = Header, <<NumFD:8, Rest/binary>>) ->
    {Proc, 4} = remote_protocol_xdr:dec_remote_procedure(Proc0, 0),
    Args = proc_to_call(Proc, "dec", "args"),
    {Header, [NumFD|decode_payload(Args, Rest)]};

% REMOTE_REPLY_WITH_FDS
decode(#remote_message_header{proc = Proc0,
                              type = <<?REMOTE_REPLY_WITH_FDS:32>>,
                              status = <<?REMOTE_OK:32>>} = Header, <<NumFD:8, Rest/binary>>) ->
    {Proc, 4} = remote_protocol_xdr:dec_remote_procedure(Proc0, 0),
    Args = proc_to_call(Proc, "dec", "args"),
    {Header, [NumFD|decode_payload(Args, Rest)]};
% XXX returns num fd in an error message?
decode(#remote_message_header{type = <<?REMOTE_REPLY_WITH_FDS:32>>,
                              status = <<?REMOTE_ERROR:32>>} = Header, <<_NumFD:8, Rest/binary>>) ->
    {Header, decode_payload(dec_remote_error, Rest)}.

% XXX shouldn't be a payload if no return values
decode_payload(none, _Payload) ->
    [];
decode_payload(Fun, Payload) ->
    {Val, _Off} = remote_protocol_xdr:Fun(Payload, 0),
    tuple_to_list(Val).


%%-------------------------------------------------------------------------
%%% RPC protocol
%%-------------------------------------------------------------------------

%% Remote protocol essage header
header(#remote_message_header{
        prog = Prog,
        vers = Vers,
        proc = Proc,
        type = Type,
        serial = Serial,
        status = Status
    }) ->
    <<Prog:4/bytes, Vers:4/bytes, Proc:4/bytes,
      Type:4/bytes, Serial:4/bytes, Status:4/bytes>>;
header(<<
    Prog:4/bytes, Vers:4/bytes, Proc:4/bytes,
    Type:4/bytes, Serial:4/bytes, Status:4/bytes
    >>) ->
    #remote_message_header{
        prog = Prog,
        vers = Vers,
        proc = Proc,
        type = Type,
        serial = Serial,
        status = Status
    }.

%%-------------------------------------------------------------------------
%%% Utility functions
%%-------------------------------------------------------------------------
field(program, <<?UINT32(N)>>) -> N;
field(version, <<?UINT32(N)>>) -> N;
field(procedure, <<?UINT32(N)>>) -> N;
field(type, <<?UINT32(N)>>) -> N;
field(serial, <<?UINT32(N)>>) -> N;

field(status, <<?UINT32(?REMOTE_OK)>>) -> ok;
field(status, <<?UINT32(?REMOTE_ERROR)>>) -> error;
field(status, <<?UINT32(?REMOTE_CONTINUE)>>) -> continue.

status({#remote_message_header{
                    status = Status
                    }, []}) ->
        verx_rpc:field(status, Status);
status({#remote_message_header{
                    status = Status
                    }, Reply}) ->
        {verx_rpc:field(status, Status), Reply}.

%%-------------------------------------------------------------------------
%%% Internal functions
%%-------------------------------------------------------------------------

% 'REMOTE_PROC_DOMAIN_CREATE_XML' -> enc_remote_domain_create_xml_args/1
% 'REMOTE_PROC_DOMAIN_CREATE_XML' -> dec_remote_domain_create_xml_ret/2
proc_to_call(Proc, "enc", Suffix) ->
    proc_to_call_1(atom_to_list(Proc), "enc", Suffix);
proc_to_call(Proc, "dec", Suffix) ->
    call_exists(proc_to_call_1(atom_to_list(Proc), "dec", Suffix)).

proc_to_call_1("REMOTE_PROC_" ++ Proc, Prefix, Suffix) ->
    list_to_atom(Prefix ++ "_remote_" ++ string:to_lower(Proc) ++ "_" ++ Suffix).


% Procedures with no arguments do not have stubs generated in
% the XDR spec, so there is no way to check if the procedures
% are valid.
call_exists(Call) ->
    Exports = proplists:get_value(exports, remote_protocol_xdr:module_info()),

    % Check if the call exists in the modules export list. If it doesn't,
    % assume the response does not include a payload.
    case proplists:get_value(Call, Exports) of
        undefined -> none;
        _ -> Call
    end.

% Procedure payload (call arguments or reply values)
payload(_Call, []) ->
    [];
% Multiple arguments are passed as tuples
payload(Call, Arg) ->
    remote_protocol_xdr:Call(list_to_tuple(Arg)).
