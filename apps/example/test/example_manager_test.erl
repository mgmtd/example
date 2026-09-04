%%%-------------------------------------------------------------------
%%% @doc example_manager applies {config_change, Ref, Ops} from mgmtd.
%%% @end
%%%-------------------------------------------------------------------
-module(example_manager_test).

-include_lib("eunit/include/eunit.hrl").

-define(DB_DIR, "test_db_example_manager").

setup() ->
    start_mgmtd(),
    stop_reg(example_manager),
    stop_reg(example_server_sup),
    lists:foreach(fun mgmtd:remove_schema/1, mgmtd:registered_schemas()),
    ok = mgmtd_cfg_db:remove_db(?DB_DIR, [{backend, mnesia}]),
    ok = mgmtd:load_function_schema(fun example:cfg_schema/0,
                                    #{config => true}),
    ok = mgmtd_cfg_db:init(?DB_DIR, [{backend, mnesia}]),
    {ok, Sup} = example_server_sup:start_link(),
    unlink(Sup),
    {ok, Mgr} = example_manager:start_link(example_server_sup),
    unlink(Mgr),
    wait_servers([]),
    ok.

teardown(_) ->
    stop_reg(example_manager),
    stop_reg(example_server_sup),
    lists:foreach(fun mgmtd:remove_schema/1, mgmtd:registered_schemas()),
    ok = mgmtd_cfg_db:remove_db(?DB_DIR, [{backend, mnesia}]),
    ok.

start_mgmtd() ->
    case mgmtd_sup:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

stop_reg(Name) ->
    case erlang:whereis(Name) of
        undefined ->
            ok;
        Pid ->
            unlink(Pid),
            exit(Pid, shutdown),
            wait_down(Pid)
    end.

wait_down(Pid) ->
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            timer:sleep(10),
            wait_down(Pid)
    end.

manager_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun add_starts_child/0,
      fun set_port_reconfigures/0,
      fun delete_stops_child/0,
      fun two_servers_independent/0,
      fun delete_then_readd_reconfigures/0]}.

add_starts_child() ->
    Key = {"echo1"},
    {ok, _} = commit_set(["server", "servers", Key, "port", "19001"]),
    wait_servers([Key]),
    {ok, Pid} = example_server_sup:child_pid(example_server_sup, Key),
    ?assertEqual(#{port => 19001, listening => true},
                 example_echo_server:status(Pid)).

set_port_reconfigures() ->
    Key = {"echo1"},
    {ok, Txn} = commit_set(["server", "servers", Key, "port", "19002"]),
    wait_servers([Key]),
    {ok, Pid} = example_server_sup:child_pid(example_server_sup, Key),
    ?assertEqual(#{port => 19002, listening => true},
                 example_echo_server:status(Pid)),

    {ok, _} = txn_set_commit(Txn, ["server", "servers", Key, "port", "19003"]),
    wait_fun(fun() ->
                     example_echo_server:status(Pid) =:=
                         #{port => 19003, listening => true}
             end),
    ?assertEqual(#{port => 19003, listening => true},
                 example_echo_server:status(Pid)),
    ?assertEqual([Key], lists:sort(example_manager:which_servers())).

delete_stops_child() ->
    Key = {"echo1"},
    {ok, Txn} = commit_set(["server", "servers", Key, "port", "19004"]),
    wait_servers([Key]),
    {ok, _} = delete_commit(Txn, ["server", "servers", Key]),
    wait_servers([]),
    ?assertEqual(undefined,
                 example_server_sup:child_pid(example_server_sup, Key)).

two_servers_independent() ->
    KeyA = {"a"},
    KeyB = {"b"},
    {ok, Txn} = commit_set(["server", "servers", KeyA, "port", "19005"]),
    {ok, Txn2} = txn_set_commit(Txn, ["server", "servers", KeyB, "port", "19006"]),
    wait_servers([KeyA, KeyB]),

    {ok, _} = delete_commit(Txn2, ["server", "servers", KeyA]),
    wait_servers([KeyB]),
    ?assertEqual(undefined,
                 example_server_sup:child_pid(example_server_sup, KeyA)),
    {ok, PidB} = example_server_sup:child_pid(example_server_sup, KeyB),
    ?assertEqual(#{port => 19006, listening => true},
                 example_echo_server:status(PidB)).

%% Same key deleted then re-added in one session is a net set, not a
%% restart. The running child is reconfigured in place.
delete_then_readd_reconfigures() ->
    Key = {"echo1"},
    Item = ["server", "servers", Key],
    {ok, Txn} = commit_set(Item ++ ["port", "19008"]),
    wait_servers([Key]),
    {ok, Pid} = example_server_sup:child_pid(example_server_sup, Key),
    ?assertEqual(#{port => 19008, listening => true},
                 example_echo_server:status(Pid)),

    {ok, Txn2} = txn_delete(Txn, Item),
    {ok, _} = txn_set_commit(Txn2, Item ++ ["port", "19009"]),
    wait_fun(fun() ->
                     example_echo_server:status(Pid) =:=
                         #{port => 19009, listening => true}
             end),
    ?assertEqual({ok, Pid},
                 example_server_sup:child_pid(example_server_sup, Key)),
    ?assertEqual([Key], lists:sort(example_manager:which_servers())).

existing_config_test_() ->
    {setup, fun existing_setup/0, fun teardown/1,
     [fun starts_from_initial_snapshot/0]}.

existing_setup() ->
    start_mgmtd(),
    stop_reg(example_manager),
    stop_reg(example_server_sup),
    lists:foreach(fun mgmtd:remove_schema/1, mgmtd:registered_schemas()),
    ok = mgmtd_cfg_db:remove_db(?DB_DIR, [{backend, mnesia}]),
    ok = mgmtd:load_function_schema(fun example:cfg_schema/0,
                                    #{config => true}),
    ok = mgmtd_cfg_db:init(?DB_DIR, [{backend, mnesia}]),
    {ok, _} = commit_set(["server", "servers", {"pre"}, "port", "19007"]),
    {ok, Sup} = example_server_sup:start_link(),
    unlink(Sup),
    {ok, Mgr} = example_manager:start_link(example_server_sup),
    unlink(Mgr),
    ok.

starts_from_initial_snapshot() ->
    wait_servers([{"pre"}]),
    {ok, Pid} = example_server_sup:child_pid(example_server_sup, {"pre"}),
    ?assertEqual(#{port => 19007, listening => true},
                 example_echo_server:status(Pid)).

commit_set(Path) ->
    txn_set_commit(mgmtd:txn_new(), Path).

txn_set_commit(Txn, Path) ->
    {ok, SchemaPath} = mgmtd_schema:lookup_path(Path),
    {ok, Txn2} = mgmtd:txn_set(Txn, SchemaPath),
    mgmtd:txn_commit(Txn2).

delete_commit(Txn, Path) ->
    {ok, Txn2} = txn_delete(Txn, Path),
    mgmtd:txn_commit(Txn2).

txn_delete(Txn, Path) ->
    {ok, SchemaPath} = mgmtd_schema:lookup_path(Path),
    mgmtd:txn_delete(Txn, SchemaPath).

wait_servers(Expected) ->
    wait_fun(fun() ->
                     lists:sort(Expected) =:=
                         lists:sort(example_manager:which_servers())
             end).

wait_fun(Pred) ->
    wait_fun(Pred, 50).

wait_fun(_Pred, 0) ->
    error({timeout, example_manager:which_servers(),
           supervisor:which_children(example_server_sup)});
wait_fun(Pred, N) ->
    case Pred() of
        true ->
            ok;
        false ->
            timer:sleep(20),
            wait_fun(Pred, N - 1)
    end.
