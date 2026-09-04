%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Simple echo server
%%%
%%% @end
%%% Created :  4 Nov 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(example_echo_server).

-behaviour(gen_server).

%% API
-export([start_link/1, status/1, reconfigure/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).

-record(state, {
                socket,
                port
               }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(inet:port_number()) -> {ok, Pid :: pid()} |
          {error, Error :: {already_started, pid()}} |
          {error, Error :: term()} |
          ignore.
start_link(Config) ->
    gen_server:start_link(?MODULE, [Config], []).

%% @doc Live bind state for operational `status servers`.
%% `port` is the bound UDP port, or `undefined` if not listening.
-spec status(pid()) -> #{port => inet:port_number() | undefined,
                         listening => boolean()}.
status(Pid) ->
    gen_server:call(Pid, status).

%% @doc Apply a new host/port conf. Rebinds the UDP socket if the port
%% changed.
-spec reconfigure(pid(), [{string(), term()}]) -> ok.
reconfigure(Pid, Conf) ->
    gen_server:call(Pid, {reconfigure, Conf}).

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
init([Config]) ->
    process_flag(trap_exit, true),
    io:format("Starting echo server ~p ~p~n", [self(), Config]),
    {ok, apply_conf(Config, #state{})}.

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
handle_call(status, _From, #state{socket = Socket, port = Port} = State) ->
    {reply, #{port => Port, listening => Socket =/= undefined}, State};
handle_call({reconfigure, Conf}, _From, State) ->
    {reply, ok, apply_conf(Conf, State)};
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
terminate(_Reason, #state{socket = Socket}) ->
    close_socket(Socket),
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

apply_conf(Conf, State) ->
    case proplists:get_value("port", Conf) of
        undefined ->
            State;
        Port when Port =:= State#state.port,
                  State#state.socket =/= undefined ->
            State;
        Port ->
            bind_port(Port, State)
    end.

bind_port(Port, State) ->
    close_socket(State#state.socket),
    case gen_udp:open(Port) of
        {ok, Socket} ->
            io:format("opened echo socket ~p~n", [Socket]),
            State#state{socket = Socket, port = Port};
        {error, _Err} ->
            State#state{socket = undefined, port = undefined}
    end.

close_socket(undefined) ->
    ok;
close_socket(Socket) ->
    gen_udp:close(Socket).
