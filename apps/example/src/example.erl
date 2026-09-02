%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc
%%%
%%% @end
%%% Created : 11 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(example).

%% API
-export([init/0]).
-export([cfg_schema/0, kernel_schema/0, oper_schema/0]).

-include_lib("mgmtd/include/mgmtd.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc initialise the configuration and CLI systems
%%
%% @end
%%--------------------------------------------------------------------
init() ->
    %% Schema must be loaded before the sys_config backend opens the DB:
    %% existing db/sys.config is validated and imported against the schema.
    ok = mgmtd:load_function_schema(fun cfg_schema/0, #{config => true}),
    ok = mgmtd:load_function_schema(fun kernel_schema/0,
                                    #{namespace => kernel, config => true}),
    ok = mgmtd:load_function_schema(fun oper_schema/0),
    SchemaFile = filename:join(code:priv_dir(example), "example_schema.json"),
    ok = mgmtd:load_json_schema(SchemaFile, #{namespace => example_json,
                                              config => true}),
    ok = mgmtd_cfg_db:init("db", [{backend, sys_config}]),
    {ok, _Pid} = ecli:open("/var/tmp/mgmtd.cli.socket", example_cli).

cfg_schema() ->
    [#container{name = "interface",
                desc = "Interface configuration",
                config = true,
                children = fun interface_schema/0},
     #container{name = "server",
                desc = "Server configuration",
                config = true,
                children = fun server_list_schema/0},
     #container{name = "client",
                desc = "Client configuration",
                config = true,
                children = fun client_list_schema/0}].

oper_schema() ->
    example_provider:schema().

%% Prefix `kernel`. Models the OTP logger handler subset used in:
%%
%%     [{kernel,
%%       [{logger,
%%         [{handler, default, logger_std_h,
%%           #{level => error, config => #{file => "log/erlang.log"}}},
%%          {handler, info, logger_std_h,
%%           #{level => debug, config => #{file => "log/debug.log"}}}]}]}].
kernel_schema() ->
    [#list{name = "logger",
           desc = "OTP logger handlers",
           key_names = ["id"],
           config = true,
           opts = [{codec, example_codec_logger}],
           children = fun logger_handler/0}].

%%%===================================================================
%%% Internal functions
%%%===================================================================

interface_schema() ->
    [#leaf{name = "speed",
           desc = "Interface speed",
           type = {enum, [{"1GbE", "1 Gigabit/s Ethernet"}]},
           default = "1GbE"}].

server_list_schema() ->
    [#list{name = "servers",
           desc = "Server list",
           key_names = ["name"],
           data_callback = mgmtd,
           children = fun server_schema/0}].

server_schema() ->
    [#leaf{name = "name",
           desc = "Server name",
           type = string},
     #leaf{name = "host",
           desc = "Server hostname",
           type = 'inet:ip-address',
           default = "127.0.0.1"},
     #leaf{name = "port",
           desc = "Listen port",
           type = 'inet:port-number',
           default = 80}].

client_list_schema() ->
    [#list{name = "clients",
           desc = "Client list",
           key_names = ["host", "port"],
           children = fun client_schema/0}].

client_schema() ->
    [#leaf{name = "name",
           desc = "Server name",
           type = string},
     #leaf{name = "host",
           desc = "Server hostname",
           type = 'inet:ip-address',
           default = "127.0.0.1"},
     #leaf{name = "port",
           desc = "Server port",
           type = 'inet:port-number',
           default = 8080}].

logger_handler() ->
    [#leaf{name = "id",
           desc = "Handler id",
           type = string},
     #leaf{name = "module",
           desc = "Handler callback module",
           type = string,
           default = "logger_std_h"},
     #leaf{name = "level",
           desc = "Handler log level",
           type = {enum, ["emergency", "alert", "critical", "error",
                          "warning", "notice", "info", "debug", "all", "none"]}},
     #container{name = "config",
                desc = "Handler-specific config",
                children = fun logger_std_h_config/0}].

logger_std_h_config() ->
    [#leaf{name = "file",
           desc = "Log file",
           type = string}].
