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

-module(vmq_discovery_dns).

-author("Dairon Medina <me@dairon.org>").

-behaviour(vmq_discovery_backend).

-export([list_nodes/0, register/0,
	 supports_registration/0, unregister/0]).

%%%===================================================================
%%% API
%%%===================================================================

list_nodes() ->
  {ok, DiscoveryConf} = application:get_env(vmq_discovery, discovery_config),
  DNSConf = proplists:get_value(dns, DiscoveryConf),
  case proplists:get_value(seed_host, DNSConf) of
    undefined ->
      lager:warning(
        "Cannot discover any nodes using DNS because seed host is not setup."
      ),
      {error, undefined_seed_host};
    SeedHost ->
       {ok, discover_hostnames(SeedHost)}
  end.
supports_registration() ->
  false.

register() ->
  ok.

unregister() ->
  ok.

discover_hostnames(SeedHostname) ->
    Hostnames = lookup(SeedHostname, ipv4) ++ lookup(SeedHostname, ipv6),
    [list_to_atom(vmq_discovery_utils:append_node_prefix(H)) || H <- Hostnames].

lookup(SeedHost, IPAddr) ->
    UseLongName = net_kernel:longnames(),
    RecordType = record_type(IPAddr),
    IPs = inet_res:lookup(SeedHost, in, RecordType),
    lager:info(
      "Addresses discovered via ~s records of ~s: ~s",
		  [string:to_upper(atom_to_list(RecordType)),
      SeedHost, string:join([inet_parse:ntoa(IP) || IP <- IPs], ", ")]
    ),
    Hosts = [extract_host(inet:gethostbyaddr(A), UseLongName, A) || A <- IPs],
    lists:filter(fun (E) -> E =/= error end, Hosts).

record_type(ipv4) -> a;
record_type(ipv6) -> aaaa.

%% Using long names.
extract_host({ok, {hostent, FQDN, _, _, _, _}}, true, _Address) ->
  FQDN;
%% Using short names.
extract_host({ok, {hostent, FQDN, _, _, _, _}}, false, _Address) ->
  lists:nth(1, string:tokens(FQDN, "."));
extract_host({error, Error}, _, Address) ->
  lager:error("Reverse DNS lookup for address ~s failed: ~p",
              [inet_parse:ntoa(Address), Error]),
  error.
