%% -*- erlang -*-
%%
%% Copyright (c) 2011 Invectorate LLC. All rights reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(ringmq_vnode).

-behaviour(riak_core_vnode).

%% API
-export([push/3,
         pop/2,
         length/2]).

-include_lib("riak_core/include/riak_core_vnode.hrl").

%% Ring handler callbacks
-export([start_vnode/1]).

%% VNode callbacks
-export([init/1,
         handle_command/3,
         handoff_starting/2, handoff_cancelled/1, handoff_finished/2,
         handle_handoff_command/3, handle_handoff_data/2, encode_handoff_item/2,
         is_empty/1,
         terminate/2, delete/1]).

%% Internal structures
-record(state, {index}).

-record('ringmq.push_v1', {key, message}).
-record('ringmq.pop_v1', {key}).
-record('ringmq.length_v1', {key}).

-define(HANDOFF_VSN, 1).

%% ===================================================================
%% API
%% ===================================================================

push(Preflist, Key, Message) ->
    Request = #'ringmq.push_v1'{
      key = Key,
      message = Message
     },
    riak_core_vnode_master:command(Preflist,
                                   Request,
                                   {fsm, undefined, self()},
                                   ringmq_vnode_master).

pop(Preflist, Key) ->
    Request = #'ringmq.pop_v1'{
      key = Key
     },
    riak_core_vnode_master:command(Preflist,
                                   Request,
                                   {fsm, undefined, self()},
                                   ringmq_vnode_master).

length(Preflist, Key) ->
    Request = #'ringmq.length_v1'{
      key = Key
     },
    riak_core_vnode_master:command(Preflist,
                                   Request,
                                   {fsm, undefined, self()},
                                   ringmq_vnode_master).

%% ===================================================================
%% Ring handler callbacks
%% ===================================================================

start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ringmq_vnode).

%% ===================================================================
%% VNode callbacks
%% ===================================================================

init([Index]) ->
    {ok, #state{index = Index}}.

handle_command(_Command, _Sender, VState) ->
    {noreply, VState}.

handoff_starting(_TargetNode, VState) ->
    {true, VState}.

handoff_cancelled(VState) ->
    {ok, VState}.

handoff_finished(_TargetNode, VState) ->
    {ok, VState}.

handle_handoff_command(_Request, _Sender, VState) ->
    {forward, VState}.

handle_handoff_data(<<?HANDOFF_VSN:8, _Object/binary>>, VState) ->
    {reply, ok, VState}.

encode_handoff_item(_Item, VPKList) ->
    Object = term_to_binary(VPKList),
    <<?HANDOFF_VSN:8, Object/binary>>.

is_empty(VState) ->
    {true, VState}.

terminate(_Reason, _VState) ->
    ok.

delete(VState) ->
    {ok, VState}.
