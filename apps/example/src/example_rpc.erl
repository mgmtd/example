%%%-------------------------------------------------------------------
%%% @doc Host callback for the example-rpc YANG module.
%%%
%%% Load with `#{callback => example_rpc}`. `invoke/2` is the
%%% `mgmtd_rpc` behaviour: input/output maps are keyed by child
%%% names (strings) with already-cast Erlang values.
%%% @end
%%%-------------------------------------------------------------------
-module(example_rpc).

-behaviour(mgmtd_rpc).

-export([invoke/2]).

invoke(["rpc", "echo"], Input) ->
    In = maps:get("in", Input, ""),
    {ok, #{"out" => "echo:" ++ In}};
invoke(_Path, _Input) ->
    {error, unknown_rpc}.
