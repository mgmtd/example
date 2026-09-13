%%%-------------------------------------------------------------------
%%% @doc CLI expand after completing a leaf name shows the leaf and
%%%      any value already stored in the session txn.
%%% @end
%%%-------------------------------------------------------------------
-module(example_cli_test).

-include_lib("eunit/include/eunit.hrl").

-define(DB_DIR, "test_db_example_cli").

setup() ->
    start_mgmtd(),
    lists:foreach(fun mgmtd:remove_schema/1, mgmtd:registered_schemas()),
    ok = mgmtd_cfg_db:remove_db(?DB_DIR, [{backend, mnesia}]),
    ok = mgmtd:load_function_schema(fun example:cfg_schema/0,
                                    #{config => true}),
    ok = example:load_firewall_yang(),
    ok = mgmtd_cfg_db:init(?DB_DIR, [{backend, mnesia}]),
    ok.

teardown(_) ->
    lists:foreach(fun mgmtd:remove_schema/1, mgmtd:registered_schemas()),
    ok = mgmtd_cfg_db:remove_db(?DB_DIR, [{backend, mnesia}]),
    ok.

start_mgmtd() ->
    case mgmtd_sup:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

cli_expand_test_() ->
    {setup, fun setup/0, fun teardown/1,
     [fun expand_set_leaf_shows_name/0,
      fun expand_set_leaf_shows_existing_value/0,
      fun expand_set_offers_insert_pipes/0,
      fun set_term_pipe_first/0,
      fun set_term_pipe_after/0,
      fun set_pipe_rejected_on_system_list/0,
      fun expand_move_only_user_ordered_trees/0,
      fun expand_list_keys_in_user_order/0,
      fun expand_move_offers_positions/0,
      fun move_term_first/0,
      fun move_term_after/0,
      fun move_incomplete_without_where/0]}.

expand_set_leaf_shows_name() ->
    {ok, J} = example_cli:init(),
    {ok, _, J1} = example_cli:execute("configure", J),
    {yes, " ", Menu, _} =
        example_cli:expand("set server servers web1 port", J1),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(true, binary:match(MenuBin, <<"port">>) =/= nomatch),
    ?assertEqual(true, binary:match(MenuBin, <<"Listen port">>) =/= nomatch),
    ?assertEqual(nomatch, binary:match(MenuBin, <<"[">>)).

expand_set_leaf_shows_existing_value() ->
    {ok, J} = example_cli:init(),
    {ok, _, J1} = example_cli:execute("configure", J),
    {ok, _, J2} = example_cli:execute("set server servers web1 port 81", J1),
    {yes, "", Menu, _} =
        example_cli:expand("set server servers web1 port ", J2),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(true, binary:match(MenuBin, <<"port">>) =/= nomatch),
    ?assertEqual(true, binary:match(MenuBin, <<"Listen port [81]">>) =/= nomatch).

expand_set_offers_insert_pipes() ->
    {ok, J} = example_cli:init(),
    {ok, _, J1} = example_cli:execute("configure", J),
    {ok, _, J2} =
        example_cli:execute("set firewall filter acl1 term deny action discard", J1),
    {yes, " ", Menu, _} =
        example_cli:expand("set firewall filter acl1 term deny action discard |", J2),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(true, binary:match(MenuBin, <<"first">>) =/= nomatch),
    ?assertEqual(true, binary:match(MenuBin, <<"last">>) =/= nomatch),
    ?assertEqual(true, binary:match(MenuBin, <<"before">>) =/= nomatch),
    ?assertEqual(true, binary:match(MenuBin, <<"after">>) =/= nomatch).

set_term_pipe_first() ->
    {ok, J} = example_cli:init(),
    {ok, _, J1} = example_cli:execute("configure", J),
    {ok, _, J2} =
        example_cli:execute("set firewall filter acl1 term deny action discard", J1),
    {ok, _, J3} =
        example_cli:execute(
          "set firewall filter acl1 term allow action accept | first", J2),
    {ok, _, _} = example_cli:execute("commit", J3),
    ?assertEqual([{"allow"}, {"deny"}],
                 mgmtd_cfg_db:list_keys(
                   ["firewall", "filter", {"acl1"}, "term"])).

set_term_pipe_after() ->
    {ok, J} = example_cli:init(),
    {ok, _, J1} = example_cli:execute("configure", J),
    {ok, _, J2} =
        example_cli:execute("set firewall filter acl2 term a action accept", J1),
    {ok, _, J3} =
        example_cli:execute("set firewall filter acl2 term c action accept", J2),
    {ok, _, J4} =
        example_cli:execute(
          "set firewall filter acl2 term b action accept | after a", J3),
    {ok, _, _} = example_cli:execute("commit", J4),
    ?assertEqual([{"a"}, {"b"}, {"c"}],
                 mgmtd_cfg_db:list_keys(
                   ["firewall", "filter", {"acl2"}, "term"])).

set_pipe_rejected_on_system_list() ->
    {ok, J} = example_cli:init(),
    {ok, _, J1} = example_cli:execute("configure", J),
    {ok, Out, J2} =
        example_cli:execute("set server servers web1 port 81 | first", J1),
    Flat = lists:flatten(Out),
    ?assertEqual(true, string:str(Flat, "ordered-by user") > 0),
    {ok, _, _} = example_cli:execute("commit", J2),
    ?assertEqual([], mgmtd_cfg_db:list_keys(["server", "servers"])).

expand_move_only_user_ordered_trees() ->
    {ok, J} = example_cli:init(),
    {ok, _, J1} = example_cli:execute("configure", J),
    {yes, " ", Menu, _} = example_cli:expand("move", J1),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(true, binary:match(MenuBin, <<"firewall">>) =/= nomatch),
    ?assertEqual(nomatch, binary:match(MenuBin, <<"server">>)),
    ?assertEqual(nomatch, binary:match(MenuBin, <<"client">>)),
    ?assertEqual(nomatch, binary:match(MenuBin, <<"interface">>)).

expand_list_keys_in_user_order() ->
    {ok, J} = example_cli:init(),
    {ok, _, J1} = example_cli:execute("configure", J),
    {ok, _, J2} =
        example_cli:execute("set firewall filter ordmenu term z action discard", J1),
    {ok, _, J3} =
        example_cli:execute("set firewall filter ordmenu term a action accept", J2),
    {ok, _, J4} =
        example_cli:execute("set firewall filter ordmenu term m action accept", J3),
    {ok, _, J5} =
        example_cli:execute("move firewall filter ordmenu term m first", J4),
    {yes, "", SetMenu, _} =
        example_cli:expand("set firewall filter ordmenu term ", J5),
    ?assertEqual(["m", "z", "a"], existing_list_keys(SetMenu)),
    {yes, "", MoveMenu, _} =
        example_cli:expand("move firewall filter ordmenu term ", J5),
    ?assertEqual(["m", "z", "a"], existing_list_keys(MoveMenu)),
    {yes, "", AfterMenu, _} =
        example_cli:expand("move firewall filter ordmenu term z after ", J5),
    ?assertEqual(["m", "a"], sibling_list_keys(AfterMenu)).

expand_move_offers_positions() ->
    {ok, J} = example_cli:init(),
    {ok, _, J1} = example_cli:execute("configure", J),
    {ok, _, J2} =
        example_cli:execute("set firewall filter acl1 term deny action discard", J1),
    {ok, _, J3} =
        example_cli:execute("set firewall filter acl1 term allow action accept", J2),
    {yes, "", Menu, _} =
        example_cli:expand("move firewall filter acl1 term deny ", J3),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(true, binary:match(MenuBin, <<"first">>) =/= nomatch),
    ?assertEqual(true, binary:match(MenuBin, <<"last">>) =/= nomatch),
    ?assertEqual(true, binary:match(MenuBin, <<"before">>) =/= nomatch),
    ?assertEqual(true, binary:match(MenuBin, <<"after">>) =/= nomatch).

move_term_first() ->
    {ok, J} = example_cli:init(),
    {ok, _, J1} = example_cli:execute("configure", J),
    {ok, _, J2} =
        example_cli:execute("set firewall filter acl3 term deny action discard", J1),
    {ok, _, J3} =
        example_cli:execute("set firewall filter acl3 term allow action accept", J2),
    {ok, _, J4} =
        example_cli:execute("move firewall filter acl3 term allow first", J3),
    {ok, _, _} = example_cli:execute("commit", J4),
    ?assertEqual([{"allow"}, {"deny"}],
                 mgmtd_cfg_db:list_keys(
                   ["firewall", "filter", {"acl3"}, "term"])).

move_term_after() ->
    {ok, J} = example_cli:init(),
    {ok, _, J1} = example_cli:execute("configure", J),
    {ok, _, J2} =
        example_cli:execute("set firewall filter acl4 term a action accept", J1),
    {ok, _, J3} =
        example_cli:execute("set firewall filter acl4 term c action accept", J2),
    {ok, _, J4} =
        example_cli:execute("set firewall filter acl4 term b action accept", J3),
    {ok, MoveOut, J5} =
        example_cli:execute("move firewall filter acl4 term b after a", J4),
    ?assertEqual("updated\r\n", lists:flatten(MoveOut)),
    {ok, _, _} = example_cli:execute("commit", J5),
    ?assertEqual([{"a"}, {"b"}, {"c"}],
                 mgmtd_cfg_db:list_keys(
                   ["firewall", "filter", {"acl4"}, "term"])).

move_incomplete_without_where() ->
    {ok, J} = example_cli:init(),
    {ok, _, J1} = example_cli:execute("configure", J),
    {ok, _, J2} =
        example_cli:execute("set firewall filter acl5 term deny action discard", J1),
    {ok, Out, _} =
        example_cli:execute("move firewall filter acl5 term deny", J2),
    Flat = lists:flatten(Out),
    ?assertEqual(true, string:str(Flat, "expected first") > 0).

existing_list_keys(Menu) ->
    Flat = lists:flatten(Menu),
    case string:find(Flat, "Select from the existing entries") of
        nomatch ->
            [];
        Rest ->
            [_Header | Lines] = string:tokens(Rest, "\r\n"),
            [string:trim(L) || L <- Lines,
                               string:trim(L) =/= "",
                               string:trim(L) =/= "|"]
    end.

sibling_list_keys(Menu) ->
    [hd(string:tokens(string:trim(L), " "))
     || L <- string:tokens(lists:flatten(Menu), "\r\n"),
        string:trim(L) =/= ""].
