%% @doc
%%
%% This module contains several Slack related utility and convenience functions
%%
%% @end
-module(ebot_slack_utils).

-export([msg/3, slack_try/3, username/2]).

%%====================================================================
%% API Functions - Slack Utilities
%%====================================================================

%% @doc
%%
%% Sends a provided message to a given channel.
%%
%% @end
-spec msg(Token :: string(), Channel :: string(), Message :: string()) -> ok.
msg(Token, Channel, Message) when is_binary(Message) ->
    msg(Token, Channel, binary_to_list(Message));
msg(Token, Channel, Message) when is_binary(Channel) ->
    msg(Token, binary_to_list(Channel), Message);
msg(Token, Channel, Message) when is_list(Token), is_list(Channel), is_list(Message) ->
    slack_try(slacker_chat, post_message, [Token, Channel, Message, [{"as_user", "true"}]]).

%% @doc
%%
%% Tries a Slack API method throwing an error if a non-200 response occurs.
%%
%% @end
-spec slack_try(Module :: atom(), Function :: atom(), Args :: list(term())) -> Response :: map().
slack_try(Module, Function, Args) when is_atom(Module), is_atom(Function), is_list(Args) ->
    case erlang:apply(Module, Function, Args) of
        {ok, 200, _, Data} ->
            lager:info("successfully executed ~p:~p/~p", [Module, Function, length(Args)]),
            maps:from_list(Data);
        Other ->
            lager:error("failed to execute ~p:~p/~p. args: [~p]. error: ~p", [Module, Function, length(Args), Args, Other]),
            #{}
    end.

%% @doc
%%
%% Consults the Slack API to return the name of any user by UserID.
%%
%% @end
-spec username(Token :: string(), UserID :: string()) -> UserName :: string() | undefined.
username(Token, UserID) when is_list(UserID) ->
    Response = slack_try(slack_user, info, [Token, UserID]),
    case Response of
        #{<<"user">> := #{<<"name">> := UserName}} ->
            UserName;
        Other ->
            lager:error("userid [~p] was not defined. error: ~p", [UserID, Other]),
            undefined
    end.
