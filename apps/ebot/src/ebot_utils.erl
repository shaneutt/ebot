%% @doc
%%
%% Basic utility and convenience functions for Ebot
%%
%% @end
-module(ebot_utils).

-export([is_function_defined/3, map_or_env/3]).

%%====================================================================
%% API Functions - Utilities
%%====================================================================

%% @doc
%%
%% Returns whether or not a Module with Function of a specific Arity is defined
%%
%% @end
-spec is_function_defined(Module :: atom(), Function :: atom(), Arity :: pos_integer()) -> boolean() | {error, Reason :: term()}.
is_function_defined(Module, Function, Arity) when is_atom(Module), is_atom(Function), is_integer(Arity) ->
    case code:ensure_loaded(Module) of
        {module, _} ->
            case proplists:get_value(exports, Module:module_info()) of
                undefined ->
                    {error, no_exports};
                Exports ->
                    case proplists:get_value(Function, Exports) of
                        undefined ->
                            {error, no_fun};
                        Arity ->
                            true;
                        Other ->
                            {error, {bad_arity, Other}}
                    end
            end;
        {error, LoadErr} ->
            {error, {load, Module, LoadErr}}
    end.

%% @doc
%%
%% Returns a value from a map or falls back to an ENV variable. If neither is available
%% throws an error.
%%
%% @end
-spec map_or_env(Key :: atom(), Map :: map(), Env :: list(term())) -> Val :: string().
map_or_env(Key, Map, Env) when is_atom(Key), is_map(Map), is_list(Env) ->
    case maps:get(Key, Map, {env, os:getenv(Env)}) of
        {env, false} ->
            lager:error("could not find configuration for ~s or ENV ~s", [Key, Env]),
            throw("missing configuration");
        {env, EnvVal} ->
            EnvVal;
        MapVal ->
            MapVal
    end.
