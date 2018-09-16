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

-module(vmq_discovery_utils).

-export([append_node_prefix/1, node_prefix/0]).

-define(NODENAME_SEPARATOR, "@").


-spec node_prefix() -> string().
node_prefix() ->
    case string:tokens(atom_to_list(node()), ?NODENAME_SEPARATOR) of
        [Prefix, _] -> Prefix;
        [_]         -> "vernemq"
    end.


-spec append_node_prefix(binary() | list()) -> atom().
append_node_prefix(Node) when is_binary(Node) ->
  append_node_prefix(binary:bin_to_list(Node));
append_node_prefix(Node) ->
  Hostname = case string:tokens(Node, ?NODENAME_SEPARATOR) of
    [_ExistingPrefix, Node] ->
      Node;
    [Node] ->
      Node
  end,
  string:join([node_prefix(), Hostname], ?NODENAME_SEPARATOR).
