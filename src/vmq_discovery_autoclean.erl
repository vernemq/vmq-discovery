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

-module(vmq_discovery_autoclean).

-author("Dairon Medina <me@dairon.org>").

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-define(APP, vmq_discovery).

-record(state, {
    interval::integer
}).

-type state() :: #state{}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    case application:get_env(?APP, autoclean_interval) of
        0 ->
            {ok, #state{}};
        Interval ->
            State = #state{interval = Interval},
            {ok, State}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return the nodes discovered by the cluster discovery backend.
%% @end
%%--------------------------------------------------------------------
-spec discovered_nodes() -> [node()].
discovered_nodes() ->
    Backend = vmq_discovery:get_backend(),
    case Backend:list_nodes() of
        {ok, Nodes} ->
            lager:debug("Cluster discovery backend: ~p returned ~p",
                        [Backend, Nodes]),
            Nodes;
        {error, Reason} ->
            lager:debug("Cluster discovery backend: ~p returned error ~p",
                        [Backend, Reason]),
            []
    end.