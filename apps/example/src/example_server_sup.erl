%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc
%%%
%%% @end
%%% Created :  4 Nov 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(example_server_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3, stop_child/2, child_pid/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
          {error, {already_started, Pid :: pid()}} |
          {error, {shutdown, term()}} |
          {error, term()} |
          ignore.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(_Sup, Id, Args) ->
    Child = child(Id, Args),
    case supervisor:start_child(?SERVER, Child) of
        {ok, _} = Ok ->
            Ok;
        {ok, _, _} = Ok ->
            Ok;
        {error, {already_started, Pid}} ->
            {ok, Pid};
        {error, already_present} ->
            _ = supervisor:delete_child(?SERVER, Id),
            supervisor:start_child(?SERVER, Child);
        Other ->
            Other
    end.

stop_child(Sup, Id) ->
    _ = supervisor:terminate_child(Sup, Id),
    _ = supervisor:delete_child(Sup, Id),
    ok.

-spec child_pid(pid() | atom(), term()) -> {ok, pid()} | undefined.
child_pid(Sup, Id) ->
    case lists:keyfind(Id, 1, supervisor:which_children(Sup)) of
        {Id, Pid, _, _} when is_pid(Pid) ->
            {ok, Pid};
        _ ->
            undefined
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
          {ok, {SupFlags :: supervisor:sup_flags(),
                [ChildSpec :: supervisor:child_spec()]}} |
          ignore.
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 5},

    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
child(Id, Args) ->
    #{id => Id,
      start => {example_echo_server, start_link, [Args]},
      restart => transient,
      shutdown => 5000,
      type => worker,
      modules => [example_server]}.
