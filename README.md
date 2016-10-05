ebot
=====

A modular bot for the Slack chat platform

Description
---

Ebot is a Slack chat bot that can be extended via modules uploaded as code snippets. You can message Ebot directly (or
via a channel that it is joined to) with a code snippet and ask it to "assimilate" that snippet, adding it permanently to
it's modules.

Once a module is assimilated by Ebot you can call that code from Slack with the following format:

```
@ebot [module_name] [optional: func] [optional: arg1, arg2, arg3...]
```

If you don't add function and args, Ebot assumes to run `module_name:run/0`.


For instance if you uploaded the following snippet:

```erlang
-module(my_test_command).

-export([test_fun/1]).

test_fun(String) ->
    "I got your string: " ++ String.
```

And then asked Ebot to assimilate it:

```
@ebot assimilate
```

You would get a response like this:

```
code assimilated: ebot_slack_module_my_test_command
```

From then on you can run that module as an Ebot command:

```
me: @ebot my_test_command test_fun Hello!
ebot: I got your string: Hello!
```

Requirements
-----

* Linux
* Erlang/OTP 18+

-----

Configuration is currenty (TODO) done via `sys.config`:

```erlang
[
    {ebot, [
        {bots, [
            #{name => "ebot"}
        ]}
    ]}
].
```

Config maps can have the following fields:

* name - string() - (Required) the name that will identify the bot. The bot will be registered to 'ebot_slack_server_' + Name.
* token - string() - a Slack API token generated via the integration pages.
* modules - string() - the directory that modules are going to be stored in (defaults to "/var/lib/ebot/default/modules")

`token` and `modules` can be provided via ENV variables instead:

```shell
export EBOT_SLACK_TOKEN="myslacktoken"
export EBOT_MODULES_DIR="/path/to/modules"
```
