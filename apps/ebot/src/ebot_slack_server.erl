%% @doc
%%
%% This server manages a web socket stream for a specific Slack user provided
%% a slack API token. It reads from the websocket and passes messages to a
%% default router and any number of custom handlers.
%%
%% @end
-module(ebot_slack_server).

-behaviour(websocket_client_handler).

-export([start_link/1, init/2, websocket_handle/3, websocket_info/3, websocket_terminate/3]).

%%====================================================================
%% API :: bot initialization
%%====================================================================

%% @doc
%%
%% Starts a new ebot_slack_server provided an API Auth Token
%%
%% @end
-spec start_link(Config :: map()) -> {ok, pid()}.
start_link(#{name := Name} = Config) ->
    %% get config data
    lager:info("starting bot ~p", [ident(Name)]),
    Token = ebot_utils:map_or_env(token, Config, "EBOT_SLACK_TOKEN"),
    Modules = ebot_utils:map_or_env(token, Config, "EBOT_MODULES_DIR"),

    %% test authentication (and connection) to Slack
    AuthResp = ebot_slack_utils:slack_try(slacker_auth, test, [Token]),

    %% start the websocket connection
    RtmResp = ebot_slack_utils:slack_try(slacker_request, send, ["rtm.start", [{"token", Token}], []]),
    WebSocketURL = maps:get(<<"url">>, RtmResp),
    UserID = maps:get(<<"user_id">>, AuthResp),
    websocket_client:start_link(WebSocketURL, ?MODULE, #{user_id => binary_to_list(UserID), token => Token, name => Name, modules => Modules}, [{local, ident(Name)}]).

%% initialization
init(#{modules := Modules, name := Name} = InitialState, _ConnState) ->
    lager:info("initializing ~p with module dir ~p", [Name, Modules]),
    true = code:add_pathz(Modules),
    {ok, InitialState}.

%%====================================================================
%% API :: websocket callbacks
%%====================================================================

websocket_handle({ping, _}, _ConnState, #{user_id := BotID} = State) ->
    {reply, {text, jsx:encode(#{id => BotID, type => ping})}, State};
websocket_handle({text, Msg}, ConnState, State) when is_binary(Msg) ->
    Decoded = jsx:decode(Msg, [return_maps]),
    NewState = ebot_slack_default_router:run(Decoded, ConnState, State),
    {ok, NewState};
websocket_handle(Unknown, _ConnState, State) ->
    lager:warning("received unknown message: ~p", [Unknown]),
    {ok, State}.

websocket_info(Msg, _ConnState, State) ->
    lager:warning("message received: ~p", [Msg]),
    {ok, State}.

websocket_terminate(Msg, _ConnState, _State) ->
    lager:warning("Server terminated: ~p", [Msg]),
    ok.

%%====================================================================
%% Private Functions
%%====================================================================

%% @private returns the unique identifier for this server
ident(Name) ->
    list_to_atom("ebot_slack_server_" ++ Name).
