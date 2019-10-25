%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Implementation of cli callbacks emulating a juniper like CLI
%%%
%%% @end
%%% Created : 31 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(example_cli).

-define(DBG(DATA), io:format("[~p:~p] ~p~n",[?MODULE, ?LINE, DATA])).
-define(DBG(FORMAT, ARGS), io:format("[~p:~p] " ++ FORMAT,[?MODULE, ?LINE] ++ ARGS)).

-export([init/0,
         banner/1,
         prompt/1,
         expand/2,
         execute/2
        ]).

-record(example_cli,
        {
         mode = operational,
         user_txn             % Transaction store for command sequences that need one
        }).

%%--------------------------------------------------------------------
%% CLI behaviour mandatory callbacks
%%--------------------------------------------------------------------
init() ->
    {ok, #example_cli{}}.

banner(#example_cli{}) ->
    {ok, "\r\nWelcome to the example system CLI\r\n
Hit TAB, SPC or ? at any time to see available options\r\n\r\n"}.

prompt(#example_cli{mode = Mode}) ->
    Suffix = case Mode of
                 operational ->
                     "> ";
                 configuration ->
                     "# "
             end,
    case inet:gethostname() of
        {ok, Hostname} ->
            {ok, Hostname ++  Suffix};
        _ ->
            {ok, Suffix}
    end.


expand([], #example_cli{mode = operational} = J) ->
    {no, [], cli:format_menu(operational_menu()), J};
expand(Chars, #example_cli{mode = operational} = J) ->
    %% ?DBG("expand ~p~n",[Chars]),
    expand_cmd(Chars, operational_menu(), J);
expand([], #example_cli{mode = configuration} = J) ->
    {no, [], cli:format_menu(configuration_menu()), J};
expand(Chars, #example_cli{mode = configuration} = J) ->
    ?DBG("expand config ~p~n",[Chars]),
    expand_cmd(Chars, configuration_menu(), J).

execute(CmdStr, #example_cli{mode = operational} = J) ->
    ?DBG("Executing operational Command ~p~n",[CmdStr]),
    execute_cmd(CmdStr, operational_menu(), J);
execute(CmdStr, #example_cli{mode = configuration} = J) ->
    ?DBG("Executing configuration Command ~p~n",[CmdStr]),
    execute_cmd(CmdStr, configuration_menu(), J).



%%--------------------------------------------------------------------
%% Menu definitions
%%
%% The Grammar list provides a mechanism to specify the various parts
%% of an entire command
%%--------------------------------------------------------------------
operational_menu() ->
    [#{rec_type => cmd,
       node_type => container,
       name => "show",
       desc => "Show commands",
       action => fun(J, Item, _Value) ->
                        show_operational(J, Item)
                end,
       children => fun() -> operational_show_menu() end
      },
     #{rec_type => cmd,
       node_type => leaf,
       name => "configure",
       desc => "Enter configuration mode",
       action => fun(J1, _, _) -> enter_config_mode(J1) end
      },
     #{rec_type => cmd,
       node_type => leaf,
       name => "exit",
       desc => "Close session",
       action => fun(J1) -> enter_config_mode(J1) end
      }
    ].

operational_show_menu() ->
    [#{rec_type => cmd,
       node_type => leaf,
       name => "configuration",
       desc => "Show current configuration",
       children => fun(Path) -> cfg_schema:lookup(Path) end,
       action => fun(J1, Item, _) -> show_status(J1, Item) end
      },
     #{rec_type => cmd,
       node_type => leaf,
       name => "status",
       desc => "Status summary",
       action => fun(J1, Item, _) -> show_status(J1, Item) end
      },
     #{rec_type => cmd,
       node_type => leaf,
       name => "sockets",
       desc => "Open sockets",
       action => fun(J, Item, _) -> show_status(J, Item) end
      },
     #{rec_type => cmd,
       node_type => leaf,
       name => "interface",
       desc => "Interface status",
       action => fun(J, Item, _) -> show_interface_status(J, Item) end
      }
    ].

configuration_menu() ->
    [#{rec_type => cmd,
       node_type => container,
       name => "show",
       desc => "Show configuration",
       children => fun(Path) ->
                           [#{node_type => container,
                              rec_type => pipe_cmd,
                              name => "|",
                              desc => "Modify output",
                              children => []} |
                            cfg_schema:children(Path)]
                   end,
       action => fun(J, Path, _) -> show_config(J, Path) end
      },
     #{rec_type => cmd,
       node_type => container,
       name => "set",
       desc => "Set a configuration parameter",
       children => fun(Path) -> cfg_schema:children(Path) end,
       action => fun(J, Path, Value) -> set_config(J, Path, Value) end
      },
     #{rec_type => cmd,
       node_type => leaf,
       name => "commit",
       desc => "Commit current changes",
       action => fun(J, _, _) -> commit_config(J) end
      },
     #{rec_type => cmd,
       node_type => leaf,
       name => "exit",
       desc => "Exit configuration mode",
       action => fun(J1, _, _) -> exit_config_mode(J1) end
      }
    ].

%%--------------------------------------------------------------------
%% Action implementations
%%--------------------------------------------------------------------
enter_config_mode(#example_cli{} = J) ->
    Txn = cfg:transaction(),
    {ok, "", J#example_cli{mode = configuration, user_txn = Txn}}.

set_config(#example_cli{user_txn = Txn} = J, Path, Value) ->
    case cfg:set(Txn, Path, Value) of
        {ok, UpdatedTxn} ->
            {ok, "ok\r\n", J#example_cli{user_txn = UpdatedTxn}};
        {error, Reason} ->
            {ok, Reason ++ "\r\n", J}
    end.

show_config(#example_cli{user_txn = Txn} = J, Path0) ->
    Path = if Path0 == undefined -> []; true -> Path0 end,
    {ok, ConfigTree} = cfg:show(Txn, Path),
    Str = cli:format_simple_tree(ConfigTree),
    {ok, Str, J}.

commit_config(#example_cli{user_txn = Txn} = J) ->
    case cfg:commit(Txn) of
        ok ->
            {ok, "ok\r\n", J};
        {error, Reason} ->
            {ok, Reason ++ "\r\n", J}
    end.

exit_config_mode(#example_cli{user_txn = Txn} = J) ->
    cfg:exit_transaction(Txn),
    {ok, "", J#example_cli{mode = operational, user_txn = undefined}}.

show_status(#example_cli{} = J, _Item) ->
    {ok, "Status description\r\n", J}.

show_interface_status(#example_cli{} = J, _Item) ->
    {ok, "Interface statuses\r\n", J}.

show_operational(#example_cli{user_txn = _Txn}, Item) ->
    ?DBG("Executing show operational ~p~n",[Item]),
    {ok, "Operational statuses\r\n"}.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

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
    case cli:expand(Str, Menu, J#example_cli.user_txn) of
        no ->
            {no, [], [], J};
        {yes, Extra, MenuItems} ->
            {yes, Extra, MenuItems, J}
    end.

execute_cmd(CmdStr, Menu, #example_cli{user_txn = Txn} = J) ->
    case cli:lookup(CmdStr, Menu, Txn) of
        {error, Reason} ->
            {ok, Reason, J};
        {ok, Cmd, Leaf, Value} ->
            #{action := Action} = lists:last(Cmd),
            case catch Action(J, Leaf, Value) of
                {'EXIT', Reason} ->
                    ?DBG("Executing configuration exit ~p~n",[Reason]),
                    {ok, "Error executing command", J};
                {ok, Result} ->
                    {ok, Result, J};
                {ok, Result, #example_cli{} = J1} ->
                    {ok, Result, J1};
                {ok, Result, UserTxn} ->
                    {ok, Result, J#example_cli{user_txn = UserTxn}}
            end
    end.

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
    ?_assertMatch( {no, "", [], #example_cli{mode = operational}}, Result).

add_space_top_level_test_() ->
    {ok, J} = init(),
    Result = expand("show", J),
    ?_assertMatch({yes, " ",  ["\r\n", _], #example_cli{mode = operational}}, Result).


-endif.
