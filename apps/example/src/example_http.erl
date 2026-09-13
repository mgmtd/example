%%%-------------------------------------------------------------------
%% @doc Host HTTP listener for the mgmtd HTML UI.
%%
%% Cowboy lives here. Callbacks call `mgmtd_ui` / `mgmtd_ui_handler`;
%% RESTCONF stays on mgmtd's own listener.
%%
%% sys.config (`example`):
%%
%%     {http, [{enabled, true}, {port, 8080}]}
%% @end
%%%-------------------------------------------------------------------
-module(example_http).

-export([start/0, stop/0, port/0, default_port/0, enabled/0, init/2]).

-define(LISTENER, example_http).
-define(DEFAULT_PORT, 8080).

-spec default_port() -> inet:port_number().
default_port() ->
    ?DEFAULT_PORT.

-spec enabled() -> boolean().
enabled() ->
    proplists:get_value(enabled, config(), true).

-spec start() -> ok | {error, term()}.
start() ->
    case enabled() of
        false ->
            ok;
        true ->
            start_listener()
    end.

-spec stop() -> ok.
stop() ->
    try cowboy:stop_listener(?LISTENER) of
        ok ->
            ok;
        {error, not_found} ->
            ok
    catch
        _:_ ->
            ok
    end.

-spec port() -> inet:port_number().
port() ->
    ranch:get_port(?LISTENER).

%% Cowboy callback: `/` is host chrome; UI actions go to mgmtd.
init(Req, home) ->
    Req1 = cowboy_req:reply(303, #{<<"location">> => <<"/mgmtd/ui">>}, <<>>, Req),
    {ok, Req1, home};
init(Req, Action) ->
    mgmtd_ui_handler:init(Req, Action).

start_listener() ->
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile(
                 [{'_', [
                         {"/", ?MODULE, home}
                         | mgmtd_ui:cowboy_routes(?MODULE)
                        ]}]),
    case cowboy:start_clear(?LISTENER, [{port, listen_port()}],
                            #{env => #{dispatch => Dispatch}}) of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

listen_port() ->
    proplists:get_value(port, config(), ?DEFAULT_PORT).

config() ->
    case application:get_env(example, http, []) of
        true ->
            [{enabled, true}];
        false ->
            [{enabled, false}];
        List when is_list(List) ->
            List
    end.
