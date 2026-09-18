%%%-------------------------------------------------------------------
%%% @doc example-rpc YANG: Erlang invoke and RESTCONF POST echo.
%%% @end
%%%-------------------------------------------------------------------
-module(example_rpc_test).

-include_lib("eunit/include/eunit.hrl").

-define(DB, "test_db_example_rpc").

rpc_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [fun echo_erlang/0,
      fun unknown_rpc/0,
      fun operations_lists_echo/0,
      fun post_echo/0]}.

setup() ->
    start_mgmtd(),
    lists:foreach(fun mgmtd:remove_schema/1, mgmtd:registered_schemas()),
    ok = mgmtd_cfg_db:remove_db(?DB, [{backend, mnesia}]),
    ok = example:load_rpc_yang(),
    ok = mgmtd_cfg_db:init(?DB, [{backend, mnesia}]),
    PrevRest = application:get_env(mgmtd, restconf),
    PrevAaa = application:get_env(mgmtd, aaa),
    ok = application:set_env(mgmtd, restconf, [{enabled, true}, {port, 0}]),
    application:unset_env(mgmtd, aaa),
    ok = mgmtd_restconf:start(),
    {ok, _} = application:ensure_all_started(inets),
    {PrevRest, PrevAaa}.

teardown({PrevRest, PrevAaa}) ->
    ok = mgmtd_restconf:stop(),
    restore(mgmtd, restconf, PrevRest),
    restore(mgmtd, aaa, PrevAaa),
    lists:foreach(fun mgmtd:remove_schema/1, mgmtd:registered_schemas()),
    ok = mgmtd_cfg_db:remove_db(?DB, [{backend, mnesia}]),
    ok.

start_mgmtd() ->
    case mgmtd_sup:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

restore(App, Key, undefined) ->
    application:unset_env(App, Key);
restore(App, Key, {ok, Val}) ->
    application:set_env(App, Key, Val).

echo_erlang() ->
    #{node_type := rpc, data_callback := example_rpc} =
        mgmtd_schema:lookup(["rpc", "echo"]),
    {ok, #{"out" := "echo:hi"}} =
        mgmtd:rpc(["rpc", "echo"], #{"in" => "hi"}).

unknown_rpc() ->
    {error, #{http := 404}} = mgmtd:rpc(["rpc", "nope"], #{}).

operations_lists_echo() ->
    {Code, _, Body} = http_get("/restconf/operations"),
    ?assertEqual(200, Code),
    #{<<"ietf-restconf:operations">> := Ops} = mgmtd_json:decode(Body),
    ?assertEqual([null], maps:get(<<"example-rpc:echo">>, Ops)).

post_echo() ->
    {Code, _, Body} =
        http_post("/restconf/operations/example-rpc:echo",
                  #{<<"example-rpc:input">> => #{<<"in">> => <<"hi">>}}),
    ?assertEqual(200, Code),
    ?assertEqual(#{<<"example-rpc:output">> => #{<<"out">> => <<"echo:hi">>}},
                 mgmtd_json:decode(Body)).

http_get(Path) ->
    Url = url(Path),
    {ok, {{_, Code, _}, Headers, Body}} =
        httpc:request(get, {Url, []}, [{timeout, 2000}],
                      [{body_format, binary}]),
    {Code, Headers, Body}.

http_post(Path, Map) ->
    Url = url(Path),
    Body = iolist_to_binary(mgmtd_json:encode(Map)),
    {ok, {{_, Code, _}, Headers, Resp}} =
        httpc:request(post,
                      {Url,
                       [{"Content-Type", "application/yang-data+json"}],
                       "application/yang-data+json", Body},
                      [{timeout, 2000}], [{body_format, binary}]),
    {Code, Headers, Resp}.

url(Path) ->
    lists:flatten(io_lib:format("http://127.0.0.1:~p~s",
                                [mgmtd_restconf:port(), Path])).
