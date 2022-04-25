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

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc initialise the configuration and CLI systems
%%
%% @end
%%--------------------------------------------------------------------
init() ->
    cfg:init(mnesia, [{db_path, "db"}]),
    cfg:load_schema(fun() -> cfg_schema() end),
    {ok, _Pid} = cli:open("/var/tmp/mgmtd.cli.socket", example_cli).

cfg_schema() ->
    [
     cfg:container("interface", "Interface configuration", fun() -> interface_schema() end),
     cfg:container("server", "Server configuration",
                   fun() -> server_list_schema() end),
     cfg:container("client", "Client configuration",
                   fun() -> client_list_schema() end)
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

interface_schema() ->
    [
     cfg:leaf( "speed", "Interface speed", {enum, [{"1GbE", "1 Gigabit/s Ethernet"}]}, "1GbE")
    ].

server_list_schema() ->
    [
     cfg:list("servers", "Server list", ["name"],
              fun() -> server_schema() end, [])
    ].

server_schema() ->
   [
    cfg:leaf( "name", "Server name", string, ""),
    cfg:leaf( "host", "Server hostname", ip_addr, "127.0.0.1"),
    cfg:leaf( "port", "Listen port", inet_port, 80)
   ].

client_list_schema() ->
    [
     cfg:list("clients", "Client list", ["host", "port"],
              fun() -> client_schema() end, [])
    ].

client_schema() ->
   [
    cfg:leaf( "name", "Server name", string, ""),
    cfg:leaf( "host", "Server hostname", ip_addr, "127.0.0.1"),
    cfg:leaf( "port", "Server port", inet_port, 8080)
   ].
