%%%-------------------------------------------------------------------
%%% @doc example Cowboy host for the mgmtd HTML UI.
%%% @end
%%%-------------------------------------------------------------------
-module(example_http_test).

-include_lib("eunit/include/eunit.hrl").

http_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [fun default_port/0,
      fun root_redirects_to_ui/0,
      fun ui_is_served/0,
      fun css_is_served/0]}.

setup() ->
    start_mgmtd(),
    _ = application:load(example),
    Prev = application:get_env(example, http),
    ok = application:set_env(example, http, [{enabled, true}, {port, 0}]),
    ok = mgmtd_ui:compile(),
    ok = example_http:start(),
    {ok, _} = application:ensure_all_started(inets),
    Prev.

teardown(Prev) ->
    ok = example_http:stop(),
    case Prev of
        undefined -> application:unset_env(example, http);
        {ok, Val} -> application:set_env(example, http, Val)
    end.

start_mgmtd() ->
    case mgmtd_sup:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

default_port() ->
    ?assertEqual(8080, example_http:default_port()).

root_redirects_to_ui() ->
    {Code, Headers, _} = http_req(get, "/"),
    ?assertEqual(303, Code),
    Loc = proplists:get_value("location", Headers),
    ?assertEqual("/mgmtd/ui", Loc).

ui_is_served() ->
    {Code, Headers, Body} = http_req(get, "/mgmtd/ui"),
    ?assertEqual(200, Code),
    CT = proplists:get_value("content-type", Headers),
    ?assert(is_list(CT) andalso string:find(CT, "text/html") =/= nomatch),
    ?assert(binary:match(Body, <<"<h1>mgmtd</h1>">>) =/= nomatch).

css_is_served() ->
    {Code, _, Body} = http_req(get, "/mgmtd/ui/static/mgmtd_ui.css"),
    ?assertEqual(200, Code),
    ?assert(binary:match(Body, <<".mgmtd-ui">>) =/= nomatch).

http_req(Method, Path) ->
    Url = lists:flatten(
            io_lib:format("http://127.0.0.1:~p~s",
                          [example_http:port(), Path])),
    {ok, {{_, Code, _}, Headers, Body}} =
        httpc:request(Method, {Url, []},
                      [{timeout, 5000}, {autoredirect, false}],
                      [{body_format, binary}]),
    {Code, Headers, Body}.
