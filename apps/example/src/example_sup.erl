%%%-------------------------------------------------------------------
%% @doc example top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(example_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy  => one_for_one,
                 intensity => 5,
                 period    => 10
                },
    Child = #{id => ?MODULE,
              start => {example_server, start_link, []},
              restart => permanent},
    Sup = #{id => example_server_sup,
            start => {example_server_sup, start_link, []},
            type => supervisor,
            restart => permanent},
    Child2 = #{id => example_manager,
              start => {example_manager, start_link, [example_server_sup]},
              restart => permanent},
    {ok, {SupFlags, [Child, Sup, Child2]} }.

%%====================================================================
%% Internal functions
%%====================================================================
