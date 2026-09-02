%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Example of subscribing to configuration change events on list
%%% items to start / stop / reconfigure processes based on membership
%%% of the list.
%%%
%%% Expects a pre-existing supervisor it can use to start / stop children
%%% @end
%%% Created :  1 Nov 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(example_manager).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
                sup,
                servers = [],
                servers_cfg_ref
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid()) -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.
start_link(Supervisor) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Supervisor], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: #state{}} |
          {ok, State :: #state{}, Timeout :: timeout()} |
          {ok, State :: #state{}, hibernate} |
          {stop, Reason :: term()} |
          ignore.
init([Supervisor]) ->
    process_flag(trap_exit, true),
    self() ! finish_startup,
    {ok, #state{sup = Supervisor}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: #state{}) ->
          {reply, Reply :: term(), NewState :: #state{}} |
          {reply, Reply :: term(), NewState :: #state{}, Timeout :: timeout()} |
          {reply, Reply :: term(), NewState :: #state{}, hibernate} |
          {noreply, NewState :: #state{}} |
          {noreply, NewState :: #state{}, Timeout :: timeout()} |
          {noreply, NewState :: #state{}, hibernate} |
          {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
          {stop, Reason :: term(), NewState :: #state{}}.
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #state{}) ->
          {noreply, NewState :: #state{}} |
          {noreply, NewState :: #state{}, Timeout :: timeout()} |
          {noreply, NewState :: #state{}, hibernate} |
          {stop, Reason :: term(), NewState :: #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: #state{}) ->
          {noreply, NewState :: #state{}} |
          {noreply, NewState :: #state{}, Timeout :: timeout()} |
          {noreply, NewState :: #state{}, hibernate} |
          {stop, Reason :: normal | term(), NewState :: #state{}}.
handle_info(finish_startup, State) ->
    {ok, Ref} = mgmtd:subscribe(["server", "servers"], self()),
    {noreply, State#state{servers_cfg_ref = Ref}};
handle_info({updated_config, Ref, NewConfig}, #state{servers_cfg_ref = Ref,
                                                     sup = Sup} = State) ->
    {Start, Stop}  = diff(State#state.servers, NewConfig),
    io:format("Start ~p Stop ~p State ~p New ~p~n",[Start, Stop, State#state.servers, NewConfig]),
    stop_children(Sup, Stop),
    start_children(Sup, Start),
    {noreply, State#state{servers = NewConfig}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
                State :: #state{}) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
                  State :: #state{},
                  Extra :: term()) -> {ok, NewState :: #state{}} |
          {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
                    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
diff(Current, Updated) ->
    Stop = Current -- Updated,
    Start = Updated -- Current,
    {Start, Stop}.

stop_children(Sup, Stop) ->
    lists:foreach(fun(C) ->
                          logger:notice("Stopping server ~p~n",[C]),
                          example_server_sup:stop_child(Sup, C)
                  end, Stop).


start_children(Sup, Start) ->
    lists:foreach(fun(C) ->
                          Conf = server_conf(C),
                          logger:notice("Starting server ~p~n",[C]),
                          example_server_sup:start_child(Sup, C, Conf)
                  end, Start).

server_conf(Key) ->
    Port = case mgmtd:lookup(["server", "servers", Key, "port"]) of
               {ok, P} -> P;
               _ -> undefined
           end,
    Host = case mgmtd:lookup(["server", "servers", Key, "host"]) of
               {ok, H} -> H;
               _ -> undefined
           end,
    [{"name", key_name(Key)}, {"host", Host}, {"port", Port}].

key_name({Name}) ->
    Name;
key_name(Key) ->
    Key.
