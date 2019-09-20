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
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
init() ->
    cfg:load_schema({?MODULE, cfg_schema}, "http://example.org"),
    cfg:load_schema(fun() -> cfg_schema() end),
    {ok, _Pid} = cli:open("/var/tmp/example.socket", example_cli).

cfg_schema() ->
    [
     cfg:container("interface", "Interface configuration", fun() -> interface_schema() end),
     cfg:container("server", "Server configuration", fun() -> server_schema() end)
    ].

%%%===================================================================
%%% Internal functions
%%%===================================================================

interface_schema() ->
    [
     cfg:leaf( "speed", "Interface speed", {enum, [{"1GbE", "1 Gigabit/s Ethernet"}]})
    ].


server_schema() ->
   [
    cfg:leaf( "name", "Server common name", string),
    cfg:leaf( "host", "Server hostname", ip_addr),
    cfg:leaf( "port", "Listen port", inet_port)
   ].

