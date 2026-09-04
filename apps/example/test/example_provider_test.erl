%%%-------------------------------------------------------------------
%%% @doc Operational-data provider used by the example `status` schema.
%%% @end
%%%-------------------------------------------------------------------
-module(example_provider_test).

-include_lib("eunit/include/eunit.hrl").

setup() ->
    start_mgmtd(),
    lists:foreach(fun mgmtd:remove_schema/1, mgmtd:registered_schemas()),
    ok = mgmtd:load_function_schema(fun example:oper_schema/0),
    ok.

teardown(_) ->
    case erlang:whereis(example_server_sup) of
        undefined ->
            ok;
        Pid ->
            exit(Pid, shutdown),
            wait_down(Pid)
    end,
    ok = mgmtd:remove_schema(),
    ok.

start_mgmtd() ->
    case mgmtd_sup:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

wait_down(Pid) ->
    case is_process_alive(Pid) of
        false ->
            ok;
        true ->
            timer:sleep(10),
            wait_down(Pid)
    end.

schema_callback_test() ->
    start_mgmtd(),
    lists:foreach(fun mgmtd:remove_schema/1, mgmtd:registered_schemas()),
    ok = mgmtd:load_function_schema(fun example:oper_schema/0),
    try
        #{config := false, data_callback := example_provider} =
            mgmtd_schema:lookup(["status"]),
        ?assertEqual(example_provider,
                     mgmtd_schema:data_callback(["status", "system", "node"])),
        ?assertEqual(example_provider,
                     mgmtd_schema:data_callback(["status", "servers"])),
        ?assertEqual(example_provider,
                     mgmtd_schema:data_callback(["status", "interfaces", "mtu"]))
    after
        mgmtd:remove_schema()
    end.

provider_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [fun system_leafs/0,
      fun interfaces_from_host/0,
      fun servers_empty_without_supervisor/0,
      fun running_server_status/0,
      fun show_status_tree/0]}.

system_leafs() ->
    {ok, Node} = mgmtd:lookup(["status", "system", "node"]),
    ?assertEqual(atom_to_list(node()), Node),
    {ok, Rel} = mgmtd:lookup(["status", "system", "otp-release"]),
    ?assertEqual(erlang:system_info(otp_release), Rel),
    {ok, Uptime} = mgmtd:lookup(["status", "system", "uptime"]),
    ?assert(is_list(Uptime) andalso Uptime =/= []),
    {ok, N} = mgmtd:lookup(["status", "system", "processes"]),
    ?assert(is_integer(N) andalso N > 0),
    {ok, Mem} = mgmtd:lookup(["status", "system", "memory"]),
    ?assert(is_integer(Mem) andalso Mem > 0).

interfaces_from_host() ->
    {ok, Keys} = mgmtd:lookup(["status", "interfaces"]),
    ?assert(is_list(Keys) andalso Keys =/= []),
    [Key | _] = Keys,
    {ok, Names} = mgmtd:lookup(["status", "interfaces", Key]),
    ?assertEqual(["address", "flags", "mac", "mtu", "name"], lists:sort(Names)),
    ?assertEqual({ok, element(1, Key)},
                 mgmtd:lookup(["status", "interfaces", Key, "name"])),
    {ok, Flags} = mgmtd:lookup(["status", "interfaces", Key, "flags"]),
    ?assert(is_list(Flags)).

servers_empty_without_supervisor() ->
    ?assertEqual({ok, []}, mgmtd:lookup(["status", "servers"])).

running_server_status() ->
    {ok, _Sup} = example_server_sup:start_link(),
    Key = {"echo1"},
    {ok, Pid} = example_server_sup:start_child(
                  example_server_sup, Key, [{"port", 19991}]),
    ?assertEqual({ok, [Key]}, mgmtd:lookup(["status", "servers"])),
    ?assertEqual({ok, pid_to_list(Pid)},
                 mgmtd:lookup(["status", "servers", Key, "pid"])),
    ?assertEqual({ok, 19991},
                 mgmtd:lookup(["status", "servers", Key, "port"])),
    ?assertEqual({ok, "listening"},
                 mgmtd:lookup(["status", "servers", Key, "state"])),
    ?assertEqual([Key],
                 mgmtd:list_keys(undefined, ["status", "servers"], '$1')).

show_status_tree() ->
    {ok, Path} = mgmtd_schema:lookup_path(["status"]),
    {ok, Tree} = mgmtd:txn_show(undefined, Path),
    Status = proplists:get_value("status", Tree),
    System = proplists:get_value("system", Status),
    ?assertEqual({value, atom_to_list(node())},
                 proplists:get_value("node", System)),
    ?assertMatch({_, _}, lists:keyfind("servers", 1, Status)),
    ?assertMatch({_, _}, lists:keyfind("interfaces", 1, Status)).
