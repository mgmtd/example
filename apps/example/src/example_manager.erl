%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Example of applying configuration change ops to running processes.
%%%
%%% Subscribes to `["server", "servers"]` and handles
%%% `{config_change, Ref, Ops}`:
%%%
%%%   `{delete, Path, Key}` — stop the child
%%%   `{add,    Path, Key}` — start the child
%%%   `{set,    Path, Val}` — reconfigure host/port on a running child
%%%
%%% Deletes are applied first, then adds, then sets, matching the order
%%% mgmtd delivers. Expects a pre-existing supervisor for children.
%%% @end
%%% Created :  1 Nov 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(example_manager).

-behaviour(gen_server).

%% API
-export([start_link/1, which_servers/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(SERVERS, ["server", "servers"]).

-record(state, {
                sup,
                servers = #{},
                servers_cfg_ref
               }).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(pid() | atom()) -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.
start_link(Supervisor) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Supervisor], []).

%% @doc List keys of servers this manager currently runs.
-spec which_servers() -> [tuple()].
which_servers() ->
    gen_server:call(?SERVER, which_servers).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Supervisor]) ->
    process_flag(trap_exit, true),
    self() ! finish_startup,
    {ok, #state{sup = Supervisor}}.

handle_call(which_servers, _From, State) ->
    {reply, maps:keys(State#state.servers), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(finish_startup, State) ->
    {ok, Ref} = mgmtd:subscribe(?SERVERS, self()),
    {noreply, State#state{servers_cfg_ref = Ref}};
handle_info({config_change, Ref, Ops}, #state{servers_cfg_ref = Ref} = State) ->
    {noreply, lists:foldl(fun apply_op/2, State, Ops)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

apply_op({delete, ?SERVERS, Key}, #state{sup = Sup, servers = Servers} = State) ->
    logger:notice("Stopping server ~p", [Key]),
    example_server_sup:stop_child(Sup, Key),
    State#state{servers = maps:remove(Key, Servers)};
apply_op({add, ?SERVERS, Key}, #state{sup = Sup, servers = Servers} = State) ->
    Conf = server_conf(Key),
    logger:notice("Starting server ~p", [Key]),
    example_server_sup:start_child(Sup, Key, Conf),
    State#state{servers = Servers#{Key => Conf}};
apply_op({set, ["server", "servers", Key, Leaf], Value},
         #state{sup = Sup, servers = Servers} = State)
  when is_tuple(Key) ->
    case maps:find(Key, Servers) of
        error ->
            State;
        {ok, Conf} ->
            Conf1 = lists:keystore(Leaf, 1, Conf, {Leaf, Value}),
            reconfigure_child(Sup, Key, Conf1),
            State#state{servers = Servers#{Key => Conf1}}
    end;
apply_op(_Op, State) ->
    State.

reconfigure_child(Sup, Key, Conf) ->
    case example_server_sup:child_pid(Sup, Key) of
        {ok, Pid} ->
            example_echo_server:reconfigure(Pid, Conf);
        undefined ->
            ok
    end.

server_conf(Key) ->
    Port = case mgmtd:lookup(?SERVERS ++ [Key, "port"]) of
               {ok, P} -> P;
               _ -> undefined
           end,
    Host = case mgmtd:lookup(?SERVERS ++ [Key, "host"]) of
               {ok, H} -> H;
               _ -> undefined
           end,
    [{"name", key_name(Key)}, {"host", Host}, {"port", Port}].

key_name({Name}) ->
    Name;
key_name(Key) ->
    Key.
