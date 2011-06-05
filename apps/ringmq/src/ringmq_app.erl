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

-module(ringmq_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case ringmq_sup:start_link() of
        {ok, Pid} ->
            %% Make this node available to the rest of the Riak world
            riak_core:register_vnode_module(ringmq_vnode),
            riak_core_node_watcher:service_up(ringmq, self()),
            
            %% Register cluster information callbacks
            cluster_info:register_app(ringmq_cinfo),
            
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.
