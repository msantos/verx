%% Copyright (c) 2011-2012, Michael Santos <michael.santos@gmail.com>
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
-module(verx_client).

-export([start_link/0, start_link/1]).
-export([start/0, start/1, stop/1]).
-export([
    call/2, call/3,

    send/2,
    recv/1, recv/2,
    recvall/1, recvall/2,
    finish/1
    ]).


%%-------------------------------------------------------------------------
%%% API
%%-------------------------------------------------------------------------
call({Module, Ref}, Proc) ->
    Module:call(Ref, Proc).
call({Module, Ref}, Proc, Arg) ->
    Module:call(Ref, Proc, Arg).

start() ->
    start([]).

start(Arg) ->
    Transport = proplists:get_value(transport, Arg, verx_client_unix),
    case Transport:start(Arg) of
        {ok, Ref} -> {ok, {Transport, Ref}};
        Error -> Error
    end.

start_link() ->
    start_link([]).

start_link(Arg) ->
    Transport = proplists:get_value(transport, Arg, verx_client_unix),
    case Transport:start_link(Arg) of
        {ok, Ref} -> {ok, {Transport, Ref}};
        Error -> Error
    end.

stop({Module, Ref}) ->
    Module:stop(Ref).

send({Module, Ref}, Buf) ->
    Module:send(Ref, Buf).

recv({Module, Ref}) ->
    Module:recv(Ref).

recv({Module, Ref}, Timeout) ->
    Module:recv(Ref, Timeout).

recvall({Module, Ref}) ->
    Module:recvall(Ref).

recvall({Module, Ref}, Timeout) ->
    Module:recvall(Ref, Timeout).

finish({Module, Ref}) ->
    Module:finish(Ref).
