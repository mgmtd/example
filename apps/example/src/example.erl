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
-export([cfg_schema/0]).

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
    mgmtd_cfg_db:init("db", [{backend, mnesia}]),
    mgmtd:load_function_schema(fun() -> cfg_schema() end),
    % mgmtd:load_json_schema("../mgmtd/test/aeternity_config_schema.json"),
    mgmtd:load_json_schema("apps/example/priv/example_schema.json"),
    {ok, _Pid} = ecli:open("/var/tmp/mgmtd.cli.socket", example_cli).

cfg_schema() ->
    [#container{name = "interface",
                desc = "Interface configuration",
                children = fun() -> interface_schema() end},
     #container{name = "server",
                desc = "Server configuration",
                children = fun() -> server_list_schema() end},
     #container{name = "client",
                desc = "Client configuration",
                children = fun() -> client_list_schema() end}].

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
           children = fun() -> server_schema() end}].

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
           children = fun() -> client_schema() end}].

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
