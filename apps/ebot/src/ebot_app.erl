%%%-------------------------------------------------------------------
%% @doc ebot public API
%% @end
%%%-------------------------------------------------------------------
-module(ebot_app).

-behaviour(application).

-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Configs} = application:get_env(ebot, bots),
    ebot_sup:start_link(Configs).

stop(_State) ->
    ok.
