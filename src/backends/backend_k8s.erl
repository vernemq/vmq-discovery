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

-export([list_nodes/0, register/0,
	 supports_registration/0, unregister/0]).

-define(SERVICE_ACCOUNT_PATH, "/var/run/secrets/kubernetes.io/serviceaccount/").

%%%===================================================================
%%% API
%%%===================================================================

init() ->
    lager:debug("Peer discovery Kubernetes: initialising..."),
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
	     {ok, lists:map(fun get_nodes/1, Response)};
      {error, Reason} ->
          lager:info("Failed to get nodes from k8s - ~s", [Reason]),
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
    lager:debug("Hitting kubernetes endpoint: ~p.", [URL]),
    case hackney:get(URL, Headers, Params, Opts) of
        {ok, 200, _RespHeaders, Client} ->
            {ok, Response} = hackney:body(Client);
        {ok, Status, _, _} ->
            {error, Status};
        {error, Reason} ->
            {error, Reason}
    end.

-spec auth_headers() -> binary().
auth_headers(TokenPath) ->
    Token0 = read_file(TokenPath),
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
