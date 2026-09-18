%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Implementation of cli callbacks emulating a juniper like CLI
%%%
%%% @end
%%% Created : 31 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(example_cli).

-include_lib("ecli/include/ecli.hrl").

-define(DBG(DATA), io:format("[~p:~p] ~p~n", [?MODULE, ?LINE, DATA])).
-define(DBG(FORMAT, ARGS), io:format("[~p:~p] " ++ FORMAT, [?MODULE, ?LINE] ++ ARGS)).

-export([init/0, init/1, banner/1, prompt/1, mode_after_exit/1, expand/2, execute/2]).

-record(example_cli,
        {mode = operational,
         user_txn,             % Transaction store for command sequences that need one
         role = admin,
         user}).

%%--------------------------------------------------------------------
%% CLI behaviour mandatory callbacks
%%--------------------------------------------------------------------
init() ->
    init(#{}).

init(Peer) when is_map(Peer) ->
    Role = mgmtd:aaa_role(Peer),
    User = maps:get(user, Peer, undefined),
    {ok, #example_cli{role = Role, user = User}}.

banner(#example_cli{role = Role, user = User}) ->
    Who =
        case User of
            undefined -> "";
            Name -> " as " ++ Name
        end,
    Hint =
        case Role of
            read_only -> " (read-only)";
            _ -> ""
        end,
    {ok,
     "\r\nWelcome to the example system CLI" ++ Who ++ Hint ++
         "\r\n\nHit TAB, SPC or "
         "? at any time to see available options\r\n\r\n"}.

prompt(#example_cli{mode = Mode, user = User}) ->
    Suffix =
        case Mode of
            operational ->
                "> ";
            configuration ->
                "# "
        end,
    Host =
        case inet:gethostname() of
            {ok, Hostname} ->
                Hostname;
            _ ->
                ""
        end,
    Prefix =
        case User of
            undefined ->
                Host;
            Name when Host =:= "" ->
                Name;
            Name ->
                Name ++ "@" ++ Host
        end,
    {ok, Prefix ++ Suffix}.

mode_after_exit(#example_cli{mode = operational}) ->
    stop;
mode_after_exit(#example_cli{mode = configuration, user_txn = Txn} = J) ->
    mgmtd:txn_exit(Txn),
    J#example_cli{mode = operational, user_txn = undefined}.

expand([], #example_cli{mode = operational} = J) ->
    {no, [], ecli:format_menu(operational_menu(J)), J};
expand(Chars, #example_cli{mode = operational} = J) ->
    %% ?DBG("expand ~p~n",[Chars]),
    expand_cmd(Chars, operational_menu(J), J);
expand([], #example_cli{mode = configuration} = J) ->
    {no, [], ecli:format_menu(configuration_menu(J)), J};
expand(Chars, #example_cli{mode = configuration} = J) ->
    %% ?DBG("expand config ~p~n",[Chars]),
    expand_cmd(Chars, configuration_menu(J), J).

execute(CmdStr, #example_cli{mode = operational} = J) ->
    ?DBG("Executing operational Command ~p~n", [CmdStr]),
    execute_cmd(CmdStr, operational_menu(J), J);
execute(CmdStr, #example_cli{mode = configuration} = J) ->
    ?DBG("Executing configuration Command ~p~n", [CmdStr]),
    execute_cmd(CmdStr, configuration_menu(J), J).

%%--------------------------------------------------------------------
%% Menu definitions
%%
%% The Grammar list provides a mechanism to specify the various parts
%% of an entire command
%%--------------------------------------------------------------------
operational_menu(#example_cli{role = Role}) ->
    ecli:permit(operational_cmds(), mgmtd:aaa_accesses(Role)).

operational_cmds() ->
    [#cmd{name = "show",
          desc = "Show commands",
          access = read,
          action = fun show_operational/2,
          children = fun operational_show_menu/0,
          pipes = fun ecli_pipe:show_pipes/0},
     #cmd{name = "configure",
          desc = "Enter configuration mode",
          access = write,
          action = fun(J1, _) -> enter_config_mode(J1) end},
     #cmd{name = "echo",
          desc = "Echo a string",
          children = fun echo_input/0,
          action = fun echo_rpc/2,
          pipes = fun ecli_pipe:show_pipes/0},
     #cmd{name = "exit",
          desc = "Close session",
          action = fun enter_config_mode/1}].

operational_show_menu() ->
    [#cmd{name = "configuration",
          desc = "Show current configuration",
          children = fun show_config_children/1,
          action = fun show_config/3,
          pipes = fun config_show_pipes/0},
     #cmd{name = "rollback",
          desc = "Show configuration rollback snapshots",
          children = fun rollback_index_show_cmds/0,
          action = fun show_rollback_list/2},
     #cmd{name = "status",
          desc = "Operational status",
          children = fun oper_children/1,
          action = fun show_oper/2}].

configuration_menu(#example_cli{role = Role}) ->
    ecli:permit(configuration_cmds(), mgmtd:aaa_accesses(Role)).

configuration_cmds() ->
    [#cmd{name = "show",
          desc = "Show configuration",
          access = read,
          children = fun(Path) -> config_children(Path, show) end,
          action = fun show_config/3,
          pipes = fun config_show_pipes/0},
     #cmd{name = "set",
          desc = "Set a configuration parameter",
          access = write,
          children = fun(Path) -> config_children(Path, set) end,
          action = fun set_config/3,
          pipes = fun ecli_pipe:set_pipes/0},
     #cmd{name = "delete",
          desc = "Delete a list item",
          access = write,
          children = fun(Path) -> config_children(Path, delete) end,
          action = fun delete_config/2},
     #cmd{name = "move",
          desc = "Move an ordered-by user list entry",
          access = write,
          children = fun(Path) -> config_children(Path, move) end,
          action = fun move_config/2},
     #cmd{name = "commit",
          desc = "Commit current changes",
          access = write,
          action = fun(J, _) -> commit_config(J) end},
     #cmd{name = "rollback",
          desc = "Restore a previous configuration into this session",
          access = write,
          children = fun rollback_index_load_cmds/0},
     #cmd{name = "exit",
          desc = "Exit configuration mode",
          action = fun(J1, _) -> exit_config_mode(J1) end}].

%%--------------------------------------------------------------------
%% Action implementations
%%--------------------------------------------------------------------
enter_config_mode(#example_cli{role = Role} = J) ->
    case mgmtd:aaa_permits(Role, write) of
        false ->
            {ok, "Permission denied\r\n", J};
        true ->
            Txn = mgmtd:txn_new(),
            {ok, "", J#example_cli{mode = configuration, user_txn = Txn}}
    end.

set_config(#example_cli{user_txn = Txn} = J, Path, Pipes) ->
    case ecli_pipe:insert_where(Pipes) of
        undefined ->
            set_only(J, Txn, Path);
        Where ->
            case has_user_ordered(Path) of
                false ->
                    {ok, "insert is only valid for ordered-by user lists\r\n", J};
                true ->
                    set_and_move(J, Txn, Path, Where)
            end
    end.

set_only(#example_cli{} = J, Txn, Path) ->
    case mgmtd:txn_set(Txn, Path) of
        {ok, Txn1} ->
            {ok, "updated\r\n", J#example_cli{user_txn = Txn1}};
        {error, Reason} ->
            {ok, format_reason(Reason), J}
    end.

set_and_move(#example_cli{} = J, Txn, Path, Where) ->
    case mgmtd:txn_set(Txn, Path) of
        {ok, Txn1} ->
            case mgmtd:txn_move(Txn1, Path, Where) of
                {ok, Txn2} ->
                    {ok, "updated\r\n", J#example_cli{user_txn = Txn2}};
                {error, Reason} ->
                    {ok, format_reason(Reason), J#example_cli{user_txn = Txn1}}
            end;
        {error, Reason} ->
            {ok, format_reason(Reason), J}
    end.

has_user_ordered(Path) when is_list(Path) ->
    lists:any(fun(#{ordered_by := user, node_type := NT})
                   when NT =:= list; NT =:= leaf_list ->
                      true;
                 (_) ->
                      false
              end, Path);
has_user_ordered(_) ->
    false.

move_config(#example_cli{user_txn = Txn} = J, Path) ->
    case take_where(Path) of
        {error, Reason} ->
            {ok, format_reason(Reason), J};
        {Where, ItemPath} ->
            case mgmtd:txn_move(Txn, ItemPath, Where) of
                {ok, Txn1} ->
                    {ok, "updated\r\n", J#example_cli{user_txn = Txn1}};
                {error, Reason} ->
                    {ok, format_reason(Reason), J}
            end
    end.

take_where(Path) when is_list(Path) ->
    case lists:reverse(Path) of
        [#{action := {move, Where}} | Rest]
          when Where =:= first; Where =:= last ->
            {Where, lists:reverse(drop_pos(Rest))};
        [#{action := {move, {Side, Key}}} | Rest] ->
            {{Side, Key}, lists:reverse(drop_pos(Rest))};
        [#{name := Key}, #{name := Side} | Rest]
          when is_list(Key), (Side =:= "before" orelse Side =:= "after") ->
            {{pos_side(Side), {Key}}, lists:reverse(Rest)};
        _ ->
            {error, "expected first, last, before <key>, or after <key>"}
    end;
take_where(_) ->
    {error, "expected first, last, before <key>, or after <key>"}.

drop_pos([#{name := S} | Rest]) when S =:= "before"; S =:= "after" ->
    Rest;
drop_pos(Rest) ->
    Rest.

pos_side("before") -> before;
pos_side("after") -> 'after'.

delete_config(#example_cli{user_txn = Txn} = J, Path) ->
    %% io:format(user, "example_cli delete path ~p~n",[Path]),
    case mgmtd:txn_delete(Txn, Path) of
        {ok, UpdatedTxn} ->
            {ok, "deleted\r\n", J#example_cli{user_txn = UpdatedTxn}};
        {error, Reason} ->
            {ok, format_reason(Reason), J}
    end.

show_config(#example_cli{user_txn = Txn} = J, Path0, Pipes) ->
    Path =
        if Path0 == undefined ->
                [];
           true ->
                Path0
        end,
    case ecli_pipe:compare_against(Pipes) of
        false ->
            Opts = case ecli_pipe:wants_defaults(Pipes) of
                       true -> #{defaults => true};
                       false -> #{}
                   end,
            {ok, ConfigTree} = mgmtd:txn_show(Txn, Path, Opts),
            {ok, {data, ConfigTree}, J};
        Against ->
            case mgmtd:txn_diff_text(Txn, Path, #{against => Against}) of
                {ok, Text} ->
                    {ok, Text, J};
                {error, Reason} ->
                    {ok, format_reason(Reason), J}
            end
    end.

commit_config(#example_cli{user_txn = Txn} = J) ->
    case mgmtd:txn_commit(Txn) of
        {ok, Txn2} ->
            {ok, "ok\r\n", J#example_cli{user_txn = Txn2}};
        {error, Reason} ->
            {ok, format_reason(Reason), J}
    end.

exit_config_mode(#example_cli{user_txn = Txn} = J) ->
    mgmtd:txn_exit(Txn),
    {ok, "", J#example_cli{mode = operational, user_txn = undefined}}.

show_oper(#example_cli{} = J, Path0) ->
    Path = case Path0 of
               [] -> oper_root();
               undefined -> oper_root();
               _ -> oper_root() ++ Path0
           end,
    case mgmtd:txn_show(undefined, Path) of
        {ok, Tree} ->
            {ok, {data, Tree}, J};
        {error, Reason} ->
            {ok, format_reason(Reason), J}
    end.

show_operational(#example_cli{user_txn = _Txn}, Item) ->
    ?DBG("Executing show operational ~p~n", [Item]),
    {ok, "Operational statuses\r\n"}.

%% YANG rpc `echo` (`example-rpc`). CLI: `echo in hi`.
echo_input() ->
    mgmtd:schema_children(["rpc", "echo", "input"], set).

echo_rpc(#example_cli{} = J, Path) ->
    case mgmtd:rpc(["rpc", "echo"], rpc_input_from_path(Path)) of
        {ok, empty} ->
            {ok, "", J};
        {ok, Out} when is_map(Out) ->
            {ok, {data, rpc_output_tree(Out)}, J};
        {error, Reason} ->
            {ok, format_reason(Reason), J}
    end.

rpc_input_from_path(Path) when is_list(Path) ->
    lists:foldl(fun(#{name := Name, value := Val}, Acc) ->
                        Acc#{Name => Val};
                   (_, Acc) ->
                        Acc
                end, #{}, Path);
rpc_input_from_path(_) ->
    #{}.

rpc_output_tree(Map) ->
    [{Name, {value, Val}} || {Name, Val} <- maps:to_list(Map)].

%% Configuration menus hide `config = false` nodes so `set` / `show
%% configuration` do not offer operational data.
config_children(Path, CmdType) ->
    [C || C <- mgmtd:schema_children(Path, CmdType),
          maps:get(config, C, true)].

show_config_children(Path) when Path =:= []; Path =:= undefined ->
    rollback_show_cmds() ++ config_children([], show);
show_config_children(Path) ->
    config_children(Path, show).

%% Configuration `show` pipes: same as ecli, but `compare rollback`
%% completes existing snapshot numbers (with timestamps) rather than a
%% free-form integer.
config_show_pipes() ->
    [compare_pipe(C) || C <- ecli_pipe:config_show_pipes()].

compare_pipe(#cmd{name = "compare"} = C) ->
    C#cmd{children = fun compare_pipe_children/0};
compare_pipe(C) ->
    C.

compare_pipe_children() ->
    [#cmd{name = "rollback",
          desc = "Compare against a rollback snapshot",
          children = fun rollback_index_compare_cmds/0}].

rollback_index_compare_cmds() ->
    Zero = [#cmd{name = "0",
                 desc = "Currently committed configuration",
                 action = {pipe, {compare, {rollback, 0}}}}],
    Zero ++
        [#cmd{name = integer_to_list(N),
              desc = rollback_desc(N, Meta),
              action = {pipe, {compare, {rollback, N}}}}
         || {N, Meta} <- mgmtd:rollback_list()].

rollback_show_cmds() ->
    [#cmd{name = "rollback",
          desc = "Show a rollback snapshot",
          children = fun rollback_index_show_cmds/0,
          action = fun show_rollback_list/2}].

rollback_index_show_cmds() ->
    [#cmd{name = integer_to_list(N),
          desc = rollback_desc(N, Meta),
          action = fun(J, _) -> show_rollback_n(J, N) end}
     || {N, Meta} <- mgmtd:rollback_list()].

rollback_index_load_cmds() ->
    [#cmd{name = integer_to_list(N),
          desc = rollback_desc(N, Meta),
          action = fun(J, _) -> do_rollback(J, N) end}
     || {N, Meta} <- mgmtd:rollback_list()].

rollback_desc(N, #{time := T}) when is_integer(T) ->
    lists:flatten(io_lib:format("Commit ~p ago (~s)",
                                [N, calendar:system_time_to_rfc3339(
                                      T, [{unit, second}])]));
rollback_desc(N, _Meta) ->
    lists:flatten(io_lib:format("Commit ~p ago", [N])).

show_rollback_list(J, _) ->
    Lines = [format_rollback_entry(E) || E <- mgmtd:rollback_list()],
    {ok, Lines, J}.

format_rollback_entry({N, Meta}) ->
    Time = case maps:get(time, Meta, undefined) of
               T when is_integer(T) ->
                   " " ++ rfc3339_list(T);
               _ ->
                   ""
           end,
    io_lib:format("~p~s\r\n", [N, Time]).

rfc3339_list(T) ->
    case calendar:system_time_to_rfc3339(T, [{unit, second}]) of
        S when is_list(S) -> S;
        B when is_binary(B) -> binary_to_list(B)
    end.

show_rollback_n(J, N) ->
    case mgmtd:rollback_show(N) of
        {ok, Tree} ->
            {ok, {data, Tree}, J};
        {error, Reason} ->
            {ok, format_reason(Reason), J}
    end.

do_rollback(#example_cli{user_txn = Txn} = J, N) ->
    case mgmtd:txn_rollback(Txn, N) of
        {ok, Txn2} ->
            {ok, "ok\r\n", J#example_cli{user_txn = Txn2}};
        {error, Reason} ->
            {ok, format_reason(Reason), J}
    end.

%% First level under `show status` is the operational `status` container.
%% Deeper completion uses the schema maps' own children funs.
oper_children(_Path) ->
    mgmtd:schema_children(["status"], show).

oper_root() ->
    {ok, Path} = mgmtd_schema:lookup_path(["status"]),
    Path.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% mgmtd commit/set can return a string or a structured term
%% (`{export_error, {missing, module}}`). Never assume a string.
format_reason(#{message := Msg}) ->
    format_reason(Msg);
format_reason(Reason) when is_binary(Reason) ->
    format_reason(binary_to_list(Reason));
format_reason(Reason) when is_list(Reason) ->
    case io_lib:printable_unicode_list(Reason) of
        true ->
            Reason ++ "\r\n";
        false ->
            lists:flatten(io_lib:format("~p\r\n", [Reason]))
    end;
format_reason(Reason) ->
    lists:flatten(io_lib:format("~p\r\n", [Reason])).

%% Given a string from the user and a tree of menu items match the
%% command against the tree. Several outcomes:
%%
%% 1. The string matches the prefix of a single node - Fill the
%%    remaining part of the menu item. With a space at the end if the
%%    node is a container, not if it is a leaf
%%
%% 2. The string fully matches a single container - Prompt with the
%%    next level of menu items
%%
%% 3. The String fully matches a single leaf - nothing to do
%%
%% 4. The string matches nothing - do nothing
%%
%% 5. The string matches several possible items - complete as far as
%%    we can and prompt the user with the possible matches

expand_cmd(Str, Menu, J) ->
    %% ?DBG("match_cmd ~p~n",[Str]),
    %% Use the library function provided in cli.erl to take care of
    %% the expansion.
    case ecli:expand(Str, Menu, J#example_cli.user_txn) of
        no ->
            {no, [], [], J};
        {yes, Extra, MenuItems} ->
            {yes, Extra, MenuItems, J}
    end.

execute_cmd(CmdStr, Menu, #example_cli{user_txn = Txn} = J) ->
    ecli:run(CmdStr, Menu, Txn, J).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

full_expansion_top_level_test_() ->
    {ok, J} = init(),
    Result = expand("s", J),
    ?_assertMatch({yes, "how ", [], #example_cli{mode = operational}}, Result).

full_expansion_multi_chars_top_level_test_() ->
    {ok, J} = init(),
    Result = expand("sh", J),
    ?_assertMatch({yes, "ow ", [], #example_cli{mode = operational}}, Result).

no_match_top_level_test_() ->
    {ok, J} = init(),
    Result = expand("x", J),
    ?_assertMatch({no, "", [], #example_cli{mode = operational}}, Result).

add_space_top_level_test_() ->
    {ok, J} = init(),
    Result = expand("show", J),
    ?_assertMatch({yes, " ", ["\r\n", _], #example_cli{mode = operational}}, Result).

-endif.
