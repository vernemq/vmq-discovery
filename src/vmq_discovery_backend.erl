
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

-module(vmq_discovery_backend).

-author("Dairon Medina <me@dairon.org>").

-callback init() -> ok | {error, Reason :: string()}.

-callback list_nodes() -> {ok, Nodes :: list()} | {error, Reason :: string()}.

-callback register() -> ok | {error, Reason :: string()}.

-callback unregister() -> ok | {error, Reason :: string()}.

-callback before_registration() -> ok | {error, Reason :: string()}.

-callback after_registration() -> ok | {error, Reason :: string()}.

-optional_callbacks([init/0]).
