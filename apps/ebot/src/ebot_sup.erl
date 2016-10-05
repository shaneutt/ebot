%%%-------------------------------------------------------------------
%% @doc ebot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(ebot_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Module, Type, Args), {I, {Module, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Configs) when is_list(Configs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Configs]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Configs]) ->
    Workers = [?CHILD(list_to_atom("ebot_slack_server_" ++ maps:get(name, Config)), ebot_slack_server, worker,  [Config]) || Config <- Configs, is_map(Config)],
    {ok, {{one_for_one, 10, 10}, Workers}}.
