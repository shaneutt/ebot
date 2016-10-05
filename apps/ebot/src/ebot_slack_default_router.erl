%% @doc
%%
%% A default router provided with Ebot that has some simple, general rules
%%
%% @end
-module(ebot_slack_default_router).

-export([run/3]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc
%%
%% A simple default router for incoming messages
%%
%% @end
-spec run({Type :: atom(), Message :: map()}, ConnState :: term(), State :: map()) -> State :: map().
run (#{<<"type">> := <<"file_change">>, <<"file_id">> := FileID} = FileInfo, _ConnState, State) ->
    lager:info("noticed a file change for file: ~p", [FileID]),
    maps:put(recent_file, FileInfo, State);
run(#{<<"type">> := <<"file_shared">>, <<"file_id">> := FileID} = FileInfo, _ConnState, State) ->
    lager:info("noticed a new file: ~p", [FileID]),
    maps:put(recent_file, FileInfo, State);
run(#{<<"type">> := <<"message">>, <<"text">> := MessageText, <<"user">> := _} = Message, ConnState, #{user_id := BotID} = State) ->
    BinBotID = list_to_binary(BotID),
    BinBotIDSize = size(BinBotID),
    case MessageText of
        <<"<@", BinBotID:BinBotIDSize/binary, "> ", Command/binary>> ->
            handle_command(binary:split(Command, <<" ">>, [global]), Message, State);
        _ ->
            case maps:get(custom_handler, State, undefined) of
                undefined ->
                    State;
                {Module, Function} ->
                    erlang:apply(Module, Function, [{text, Message}, ConnState, State])
            end
    end;
run(_Message, _ConnState, State) ->
    State.

%%====================================================================
%% Private Functions
%%====================================================================

%% @private handles the incoming Slack command either sending it to module or a predefined function
handle_command([<<"assimilate">>|_], #{<<"channel">> := Channel}, State) ->
    ebot_code:assimilate_recent(Channel, State),
    State;
handle_command([<<"custom_handler">>, Module], Message, State) ->
    handle_command([<<"custom_handler">>, Module, <<"run">>], Message, State);
handle_command([<<"custom_handler">>, Module, Function|_], #{<<"channel">> := Channel}, #{token := Token} = State) ->
    NewModule = binary_to_atom(Module, utf8),
    NewFunction = binary_to_atom(Function, utf8),
    case ebot_utils:is_function_defined(NewModule, NewFunction, 3) of
        {error, _} = FullError ->
            ebot_slack_utils:msg(Token, Channel, lists:flatten(io_lib:format("error: could not load ~p:~p/3 ~p", [NewModule, NewFunction, FullError]))),
            State;
        true ->
            ebot_slack_utils:msg(Token, Channel, lists:flatten(io_lib:format("~p:~p/3 is my new custom handler.", [NewModule, NewFunction]))),
            maps:put(custom_handler, {NewModule, NewFunction}, State)
    end;
handle_command([Module], Message, State) ->
    handle_command([Module, <<"run">>], Message, State);
handle_command([M, F| A], #{<<"channel">> := Channel}, #{token := Token} = State) ->
    %% convert the call to erlang:apply/3 format
    Module = binary_to_atom(M, utf8),
    Function = binary_to_atom(F, utf8),
    Args = [binary_to_list(Arg) || Arg <- A],
    %% try to ensure the code is loaded, and convert the module name if needed
    case code:ensure_loaded(Module) of
        {error, _} ->
            %% converted modules are modules uploaded from Slack Erlang snippets, all are prefixed with "ebot_slack_module_"
            ConvertedModuleName = binary_to_atom(<<"ebot_slack_module_", M/binary>>, utf8),
            case code:ensure_loaded(ConvertedModuleName) of
                {error, LoadErr} ->
                    ebot_slack_utils:msg(Token, Channel, lists:flatten(io_lib:format("error: ~p", [LoadErr]))),
                    State;
                {module, _} ->
                    ok = do_command_call(ConvertedModuleName, Function, Args, Token, Channel),
                    State
            end;
        {module, _} ->
            %% modules that were added outside of Slack land here
            ok = do_command_call(Module, Function, Args, Token, Channel),
            State
    end.

%% @private runs the requested function
do_command_call(Module, Function, Args, Token, Channel) ->
    BotResponse = case proplists:get_value(exports, Module:module_info()) of
        undefined ->
            "error: could not find exports for" ++ atom_to_list(Module);
        Exports ->
            Arity = length(Args),
            case proplists:get_value(Function, Exports) of
                Arity ->
                    case erlang:apply(Module, Function, Args) of
                        Response when is_list(Response) ->
                            Response;
                        Other ->
                            lists:flatten(io_lib:format("error: response from function was in a bad format: ~p", [Other]))
                    end;
                _ -> "error: module " ++ atom_to_list(Module) ++ " has no function " ++ atom_to_list(Function) ++ "/" ++ integer_to_list(Arity)
            end
    end,
    ebot_slack_utils:msg(Token, Channel, BotResponse),
    ok.
