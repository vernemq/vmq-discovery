%% Copyright 2018 Octavo Labs AG (https://vernemq.com/company.html)
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
-module(vmq_discovery_autocluster).

-author("Dairon Medina <me@dairon.org>").

%% table is owned by vmq_cluster_mon
-define(VMQ_CLUSTER_STATUS, vmq_status).


init() ->
    case is_first_node() of
        true ->
            vmq_discovery:maybe_init();
        false ->
            vmq_discovery:maybe_init(),
            vmq_discovery:maybe_register()
    end.


%% Attempts to join discovered and reachable cluster nodes.
join_cluster(DiscoveryNodes) ->
    case find_node_to_cluster_with(exclude_me(DiscoveryNodes)) of
        {ok, Node} ->
            lager:info("Node '~s' chossen as discovery node for autoclustering", [Node]),
            case rpc:call(vmq_peer_service, join, [Node]) of
                ok ->
                    rpc:call(vmq_cluster, recheck, []),
                    lager:info("Joined cluster succesfully!");
                {error, Reason} ->
                    lager:error("Couldn't join cluster due to ~p~n", [Reason])
            end;
        none ->
            lager:warning(
                "Could not successfully contact any node of: ~s. "
                "Starting as a standalone node...~n",
                [string:join(lists:map(fun atom_to_list/1, DiscoveryNodes), ",")]
            )
    end.


-spec get_cluster_nodes() -> [any()].
get_cluster_nodes() ->
    [Node || [{Node, true}]
        <- ets:match(?VMQ_CLUSTER_STATUS, '$1'), Node /= ready].

-spec me_in_nodes(list())-> boolean().
me_in_nodes(Nodes) ->
    lists:member(node(), Nodes).

exclude_me(Nodes) ->
    Nodes -- [node()].

-spec is_first_node() -> boolean().
is_first_node() ->
    Nodes = get_cluster_nodes(),
    exclude_me(Nodes) == [].

find_node_to_cluster_with([]) ->
    none;
find_node_to_cluster_with([Node|OtherNodes]) ->
    Fail = fun(Reason) ->
                lager:warning(
                    "Could not cluster with node: ~s, reason: ~p",
                    [Node, Reason]),
                find_node_to_cluster_with(OtherNodes)
            end,
    case rpc:call(Node, vmq_cluster, is_ready, []) of
        true ->
            {ok, Node};
        false ->
            Fail("Node not ready");
        {badrpc, _} = Reason ->
            Fail(Reason)
    end.
