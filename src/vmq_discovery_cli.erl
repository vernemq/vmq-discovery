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

-module(vmq_discovery_cli).

-author("Dairon Medina <me@dairon.org>").

-behaviour(clique_handler).

-export([register_cli/0]).


-spec register_cli() -> ok.
register_cli() ->
    register_config().

register_config() ->
    ConfigKeys = [
     "vmq_discovery.discovery_enabled",
     "vmq_discovery.autoclean_interval"
    ],
    [clique:register_config([Key],
        fun register_config_callback/3) || Key <- ConfigKeys
    ],
    ok = clique:register_config_whitelist(ConfigKeys).

register_config_callback(_, _, _) ->
    ok.
