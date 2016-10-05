%% @doc
%%
%% This module is responsible for loading new code into a running Ebot
%%
%% @end
-module(ebot_code).

-export([assimilate_recent/2]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc
%%
%% Download, compile, and load the most recent Erlang snippet dropped in channel
%%
%% @end
-spec assimilate_recent(Channel :: string(), State :: map()) -> {ok, Module :: atom()} | {error, Reason :: term()}.
assimilate_recent(Channel, #{token := Token, modules := Modules} = State) ->
    case maps:get(recent_file, State, "") of
        #{<<"file_id">> := FileID} ->
            FileInfoResp = ebot_slack_utils:slack_try(slacker_file, info, [Token, FileID, []]),
            FileInfoData = maps:from_list(maps:get(<<"file">>, FileInfoResp, [])),
            case FileInfoData of
                #{<<"url_private_download">> := Link, <<"filetype">> := <<"erlang">>} ->
                    %% GET the file from Slack
                    GetRequest = {binary_to_list(Link), [{"Authorization", "Bearer " ++ Token}]},
                    {ok, GetResponse} = httpc:request(get, GetRequest, [], []),
                    Contents = list_to_binary(element(3, GetResponse)),
                    %% do a rough check to make sure the contents of the file looks like an Erlang module
                    case Contents of
                        <<"-module(", Name/binary>> ->
                            %% store and assimilate the file
                            [Module|_] = binary:split(Name, <<")">>),
                            FileName = Modules ++ "/" ++ "ebot_slack_module_" ++ binary_to_list(Module) ++ ".erl",
                            NewContents = binary:replace(Contents, <<"-module(", Module/binary>>, <<"-module(ebot_slack_module_", Module/binary>>),
                            ok = file:write_file(FileName, NewContents),
                            lager:info("compiling ~p", [FileName]),
                            assimilate(Token, Channel, FileName, Modules);
                        _ ->
                            ebot_slack_utils:msg(Token, Channel, "error: not a valid Erlang module"),
                            {error, invalid}
                    end;
                #{<<"filetype">> := Other} ->
                    ebot_slack_utils:msg(Token, Channel, "error: that doesn't look like an erlang file, it looks like " ++ binary_to_list(Other)),
                    {error, no_erlang};
                _ ->
                    ebot_slack_utils:msg(Token, Channel, "error: no file found"),
                    {error, nofile}
            end;
        _ ->
            ebot_slack_utils:msg(Token, Channel, "error: could not find recent file"),
            {error, no_recent_file}
    end.

%%====================================================================
%% Private Functions
%%====================================================================

%% @private helper function for get_file/2 to report on code assimilation
assimilate(Token, Channel, FileName, Modules) ->
    case compile:file(FileName, [verbose, return_errors, return_warnings, {outdir, Modules}]) of
        {ok, Module, Warnings} ->
            case code:soft_purge(Module) of
                true ->
                    {module, Module} = code:load_file(Module),
                    lager:info("assimilated ~p", [FileName]),
                    case Warnings of
                        [] -> ok;
                        _ -> ebot_slack_utils:msg(Token, Channel, lists:flatten(io_lib:format("Warnings: ~p", [Warnings])))
                    end,
                    ebot_slack_utils:msg(Token, Channel, "code assimilated: " ++ atom_to_list(Module)),
                    {ok, Module};
                false ->
                    ebot_slack_utils:msg(Token, Channel, "error: could not purge " ++ atom_to_list(Module)),
                    {error, no_purge}
            end;
        {error, Errors, Warnings} ->
            ok = file:delete(FileName),
            ebot_slack_utils:msg(Token, Channel, lists:flatten(io_lib:format("Errors: ~p", [Errors]))),
            ebot_slack_utils:msg(Token, Channel, lists:flatten(io_lib:format("Warnings: ~p", [Warnings]))),
            {error, [Errors, Warnings]};
        error ->
            ok = file:delete(FileName),
            ebot_slack_utils:msg(Token, Channel, "error: unable to assimilate code for unknown reasons. check my logs."),
            {error, unknown}
    end.
