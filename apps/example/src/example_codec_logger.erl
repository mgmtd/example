%%%-------------------------------------------------------------------
%%% @doc sys.config codec for OTP Kernel `logger` handler entries.
%%%
%%% Default (schema / `#cfg{}`) value is a list of proplists:
%%%
%%%     [[{id, "default"}, {module, "logger_std_h"},
%%%       {level, "error"}, {config, [{file, "log/erlang.log"}]}],
%%%      ...]
%%%
%%% Wire (OTP) value is a list of tagged tuples:
%%%
%%%     [{handler, default, logger_std_h,
%%%       #{level => error, config => #{file => "log/erlang.log"}}},
%%%      ...]
%%%
%%% Atom ↔ string and proplist ↔ map conversions happen here. Unknown
%%% tuple tags (`filters`, `module_level`, `{handler, default, undefined}`)
%%% are rejected until the schema covers them.
%%% @end
%%%-------------------------------------------------------------------
-module(example_codec_logger).

-behaviour(mgmtd_codec).

-export([export/1, import/1]).

-spec export(term()) -> term().
export(Items) when is_list(Items) ->
    [export_handler(Item) || Item <- Items];
export(Other) ->
    throw({export_error, {invalid_logger_list, Other}}).

-spec import(term()) -> term().
import(Items) when is_list(Items) ->
    [import_handler(Item) || Item <- Items];
import(Other) ->
    throw({import_error, {invalid_logger_list, Other}}).

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------
export_handler(Props) when is_list(Props) ->
    Id = to_atom(required(Props, id)),
    %% OTP's tuple always has a callback module; default to the standard
    %% handler so a partial `set` (level / file only) still commits.
    Mod = to_atom(proplists:get_value(module, Props, "logger_std_h")),
    Map = maps:from_list(
            [export_field(K, V) || {K, V} <- Props,
                                   K =/= id, K =/= module]),
    {handler, Id, Mod, Map};
export_handler(Other) ->
    throw({export_error, {invalid_logger_item, Other}}).

export_field(level, V) ->
    {level, to_atom(V)};
export_field(config, V) when is_list(V) ->
    {config, maps:from_list(V)};
export_field(K, V) ->
    {K, V}.

import_handler({handler, Id, Mod, Map})
  when is_map(Map) ->
    [{id, from_atom(Id)},
     {module, from_atom(Mod)}
     | [import_field(K, V) || {K, V} <- maps:to_list(Map)]];
import_handler(Other) ->
    throw({import_error, {unsupported_logger_entry, Other}}).

import_field(level, V) ->
    {level, from_atom(V)};
import_field(config, V) when is_map(V) ->
    {config, maps:to_list(V)};
import_field(K, V) ->
    {K, V}.

required(Props, Key) ->
    case lists:keyfind(Key, 1, Props) of
        {_, V} ->
            V;
        false ->
            throw({export_error, {missing, Key}})
    end.

to_atom(A) when is_atom(A) ->
    A;
to_atom(S) when is_list(S) ->
    list_to_atom(S);
to_atom(B) when is_binary(B) ->
    binary_to_atom(B, utf8);
to_atom(Other) ->
    throw({export_error, {not_atom, Other}}).

from_atom(A) when is_atom(A) ->
    atom_to_list(A);
from_atom(S) when is_list(S) ->
    S;
from_atom(B) when is_binary(B) ->
    unicode:characters_to_list(B);
from_atom(Other) ->
    throw({import_error, {not_name, Other}}).
