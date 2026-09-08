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

-export([init/0, banner/1, prompt/1, mode_after_exit/1, expand/2, execute/2]).

-record(example_cli,
        {mode = operational,
         user_txn}).             % Transaction store for command sequences that need one

%%--------------------------------------------------------------------
%% CLI behaviour mandatory callbacks
%%--------------------------------------------------------------------
init() ->
    {ok, #example_cli{}}.

banner(#example_cli{}) ->
    {ok,
     "\r\nWelcome to the example system CLI\r\n\nHit TAB, SPC or "
     "? at any time to see available options\r\n\r\n"}.

prompt(#example_cli{mode = Mode}) ->
    Suffix =
        case Mode of
            operational ->
                "> ";
            configuration ->
                "# "
        end,
    case inet:gethostname() of
        {ok, Hostname} ->
            {ok, Hostname ++ Suffix};
        _ ->
            {ok, Suffix}
    end.

mode_after_exit(#example_cli{mode = operational}) ->
    stop;
mode_after_exit(#example_cli{mode = configuration, user_txn = Txn} = J) ->
    mgmtd:txn_exit(Txn),
    J#example_cli{mode = operational, user_txn = undefined}.

expand([], #example_cli{mode = operational} = J) ->
    {no, [], ecli:format_menu(operational_menu()), J};
expand(Chars, #example_cli{mode = operational} = J) ->
    %% ?DBG("expand ~p~n",[Chars]),
    expand_cmd(Chars, operational_menu(), J);
expand([], #example_cli{mode = configuration} = J) ->
    {no, [], ecli:format_menu(configuration_menu()), J};
expand(Chars, #example_cli{mode = configuration} = J) ->
    %% ?DBG("expand config ~p~n",[Chars]),
    expand_cmd(Chars, configuration_menu(), J).

execute(CmdStr, #example_cli{mode = operational} = J) ->
    ?DBG("Executing operational Command ~p~n", [CmdStr]),
    execute_cmd(CmdStr, operational_menu(), J);
execute(CmdStr, #example_cli{mode = configuration} = J) ->
    ?DBG("Executing configuration Command ~p~n", [CmdStr]),
    execute_cmd(CmdStr, configuration_menu(), J).

%%--------------------------------------------------------------------
%% Menu definitions
%%
%% The Grammar list provides a mechanism to specify the various parts
%% of an entire command
%%--------------------------------------------------------------------
operational_menu() ->
    [#cmd{name = "show",
          desc = "Show commands",
          action = fun show_operational/2,
          children = fun operational_show_menu/0,
          pipes = fun ecli_pipe:show_pipes/0},
     #cmd{name = "configure",
          desc = "Enter configuration mode",
          action = fun(J1, _) -> enter_config_mode(J1) end},
     #cmd{name = "exit",
          desc = "Close session",
          action = fun enter_config_mode/1}].

operational_show_menu() ->
    [#cmd{name = "configuration",
          desc = "Show current configuration",
          children = fun(Path) -> config_children(Path, show) end,
          action = fun show_config/2,
          pipes = fun ecli_pipe:config_show_pipes/0},
     #cmd{name = "status",
          desc = "Operational status",
          children = fun oper_children/1,
          action = fun show_oper/2}].

configuration_menu() ->
    [#cmd{name = "show",
          desc = "Show configuration",
          children = fun(Path) -> config_children(Path, show) end,
          action = fun show_config/2,
          pipes = fun ecli_pipe:config_show_pipes/0},
     #cmd{name = "set",
          desc = "Set a configuration parameter",
          children = fun(Path) -> config_children(Path, set) end,
          action = fun set_config/2},
     #cmd{name = "delete",
          desc = "Delete a list item",
          children = fun(Path) -> config_children(Path, delete) end,
          action = fun delete_config/2},
     #cmd{name = "commit",
          desc = "Commit current changes",
          action = fun(J, _) -> commit_config(J) end},
     #cmd{name = "exit",
          desc = "Exit configuration mode",
          action = fun(J1, _) -> exit_config_mode(J1) end}].

%%--------------------------------------------------------------------
%% Action implementations
%%--------------------------------------------------------------------
enter_config_mode(#example_cli{} = J) ->
    Txn = mgmtd:txn_new(),
    {ok, "", J#example_cli{mode = configuration, user_txn = Txn}}.

set_config(#example_cli{user_txn = Txn} = J, Path) ->
    case mgmtd:txn_set(Txn, Path) of
        {ok, UpdatedTxn} ->
            {ok, "updated\r\n", J#example_cli{user_txn = UpdatedTxn}};
        {error, Reason} ->
            {ok, format_reason(Reason), J}
    end.

delete_config(#example_cli{user_txn = Txn} = J, Path) ->
    %% io:format(user, "example_cli delete path ~p~n",[Path]),
    case mgmtd:txn_delete(Txn, Path) of
        {ok, UpdatedTxn} ->
            {ok, "deleted\r\n", J#example_cli{user_txn = UpdatedTxn}};
        {error, Reason} ->
            {ok, format_reason(Reason), J}
    end.

show_config(#example_cli{user_txn = Txn} = J, Path0) ->
    Path =
        if Path0 == undefined ->
                [];
           true ->
                Path0
        end,
    {ok, ConfigTree} = mgmtd:txn_show(Txn, Path),
    {ok, {data, ConfigTree}, J}.

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

%% Configuration menus hide `config = false` nodes so `set` / `show
%% configuration` do not offer operational data.
config_children(Path, CmdType) ->
    [C || C <- mgmtd:schema_children(Path, CmdType),
          maps:get(config, C, true)].

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
