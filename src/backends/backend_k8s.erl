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

-module(backend_k8s).

-author("Dairon Medina <me@dairon.org>").

-behaviour(vmq_discovery_backend).

-export([init/0, list_nodes/0, register/0,
	 supports_registration/0, unregister/0]).

-define(SERVICE_ACCOUNT_PATH, "/var/run/secrets/kubernetes.io/serviceaccount/").

%%%===================================================================
%%% API
%%%===================================================================

init() ->
    lager:info("Initializing Kubernetes peer discovery..."),
    ok = application:ensure_started(hackney).

list_nodes() ->
    {ok, DiscoveryConf} = application:get_env(vmq_discovery, discovery_config),
    K8sConf = proplists:get_value(k8s, DiscoveryConf),
    ApiServer = proplists:get_value(master_node_url, K8sConf),
    Namespace = proplists:get_value(namespace, K8sConf),
    Service = proplists:get_value(service_name, K8sConf),
    ServiceUrl = generate_service_url(Service, Namespace),
    AddressType = proplists:get_value(address_type, K8sConf, hostname),
    TokenPath = proplists:get_value(token_path, K8sConf),
    CertPath = proplists:get_value(certificate_path, K8sConf),
    %% Check if the CA certificate exists and set HTTP options accordingly.
    HttpOpts = case filelib:is_file(CertPath) of
        true  -> [{ssl, [{cacertfile, CertPath}]}];
        false -> [{ssl, [{verify, verify_none}]}]
    end,
    Url = lists:concat([ApiServer, ServiceUrl]),
    Headers = auth_headers(TokenPath),
    case api_request(Url, Headers, HttpOpts) of
      {ok, Response} ->
         Addresses = extract_addresses(AddressType, Response),
	     {ok, lists:map(fun node_name/2, Service, Addresses)};
      {error, Reason} ->
          lager:info("Failed to get nodes from Kubernetes - ~s", [Reason]),
          {error, Reason}
    end.

supports_registration() ->
     true.

-spec register() -> ok.
register() ->
    ok.

-spec unregister() -> ok.
unregister() -> ok.

generate_service_url(Service, Namespace) ->
    lists:concat(["api/v1/namespaces/", Namespace, "/endpoints/", Service]).

api_request(URL, Headers, Opts) ->
    Params = <<>>,
    lager:debug("Hitting Kubernetes endpoint: ~p.", [URL]),
    case hackney:get(URL, Headers, Params, Opts) of
        {ok, 200, _RespHeaders, Client} ->
            {ok, _Response} = hackney:body(Client);
        {ok, Status, _, _} ->
            {error, Status};
        {error, Reason} ->
            {error, Reason}
    end.

auth_headers(TokenPath) ->
    Token0 = read_file(TokenPath, undefined),
    Token = binary:replace(Token0, <<"\n">>, <<>>),
    [{"Authorization", "Bearer " ++ binary_to_list(Token)}].

read_file(Path, Default) ->
    case file:read_file(Path) of
        {ok, Data} ->
            Data;
        {error, Error} ->
            lager:error("Cannot read ~s. Reason: ~p", [Path, Error]),
            Default
    end.

node_name(ServiceName, Address) ->
    list_to_atom(ServiceName ++ "@" ++ binary_to_list(Address)).

get_ready_addresses(AddressType, Subset) ->
    case maps:get(<<"notReadyAddresses">>, Subset, undefined) of
      undefined -> ok;
      NotReadyAddresses ->
            Formatted = string:join([binary_to_list(get_address(AddressType, Address))
                                    || Address <- NotReadyAddresses], ", "),
            lager:info(
                "Kubernetes endpoint returned some nodes thst are not ready: ~s",
                [Formatted]
            )
    end,
    maps:get(<<"addresses">>, Subset, []).

extract_addresses(AddressType, Response) ->
    AddressList = [[get_address(AddressType, Address)
                  || Address <- get_ready_addresses(AddressType, Subset)]
                  || Subset <- maps:get(<<"subsets">>, Response, [])],
    sets:to_list(sets:union(lists:map(fun sets:from_list/1, AddressList))).

get_address(AddressType, Address) ->
    maps:get(atom_to_binary(AddressType, utf8), Address).

