%% Copyright 2018 Octavo Labs AG Zurich Switzerland (https://octavolabs.com)
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

-module(vmq_discovery_manual).

-author("Dairon Medina <me@dairon.org>").

-behaviour(vmq_discovery_backend).

-export([list_nodes/0, supports_registration/0,
        register/0, unregister/0]).

%%%===================================================================
%%% API
%%%===================================================================

list_nodes() ->
    case application:get_env(vmq_discovery, cluster_nodes) of
        Nodes when is_list(Nodes) ->
            {ok, Nodes};
        undefined ->
            {ok, []}
    end.

supports_registration() ->
    false.

register() ->
    ok.

unregister() ->
    ok.
