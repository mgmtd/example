%%%-------------------------------------------------------------------
%%% @doc OTP kernel.logger codec round-trip via the example schema.
%%% @end
%%%-------------------------------------------------------------------
-module(example_logger_test).

-include_lib("eunit/include/eunit.hrl").

-define(DB_DIR, "test_db_example_logger").

-define(OTP_LOGGER,
        [{kernel,
          [{logger,
            [{handler, default, logger_std_h,
              #{level => error,
                config => #{file => "log/erlang.log"}}},
             {handler, info, logger_std_h,
              #{level => debug,
                config => #{file => "log/debug.log"}}}]}]}]).

%%--------------------------------------------------------------------
%% Pure codec
%%--------------------------------------------------------------------
codec_term_test_() ->
    [fun export_defaults_module_when_missing/0,
     fun export_packs_handler_tuples/0,
     fun import_unpacks_handler_tuples/0,
     fun roundtrip_default_form/0,
     fun import_rejects_filters/0,
     fun import_rejects_disable_default/0].

export_defaults_module_when_missing() ->
    Default =
        [[{id, "def"},
          {level, "error"},
          {config, [{file, "/u01/log"}]}]],
    ?assertEqual(
       [{handler, def, logger_std_h,
         #{level => error, config => #{file => "/u01/log"}}}],
       example_codec_logger:export(Default)).

export_packs_handler_tuples() ->
    Default =
        [[{config, [{file, "log/erlang.log"}]},
          {id, "default"},
          {level, "error"},
          {module, "logger_std_h"}],
         [{config, [{file, "log/debug.log"}]},
          {id, "info"},
          {level, "debug"},
          {module, "logger_std_h"}]],
    Wire = example_codec_logger:export(Default),
    ?assertEqual(
       [{handler, default, logger_std_h,
         #{level => error, config => #{file => "log/erlang.log"}}},
        {handler, info, logger_std_h,
         #{level => debug, config => #{file => "log/debug.log"}}}],
       Wire).

import_unpacks_handler_tuples() ->
    Wire = logger_wire(),
    Default = example_codec_logger:import(Wire),
    [DefaultH, InfoH] = Default,
    ?assertEqual("default", proplists:get_value(id, DefaultH)),
    ?assertEqual("logger_std_h", proplists:get_value(module, DefaultH)),
    ?assertEqual("error", proplists:get_value(level, DefaultH)),
    ?assertEqual([{file, "log/erlang.log"}],
                 proplists:get_value(config, DefaultH)),
    ?assertEqual("info", proplists:get_value(id, InfoH)),
    ?assertEqual("debug", proplists:get_value(level, InfoH)).

roundtrip_default_form() ->
    Default =
        [[{id, "default"},
          {module, "logger_std_h"},
          {level, "error"},
          {config, [{file, "log/erlang.log"}]}]],
    Back = example_codec_logger:import(example_codec_logger:export(Default)),
    ?assertEqual(lists:sort(hd(Default)), lists:sort(hd(Back))).

import_rejects_filters() ->
    ?assertThrow({import_error, {unsupported_logger_entry, {filters, log, []}}},
                 example_codec_logger:import([{filters, log, []}])).

import_rejects_disable_default() ->
    ?assertThrow({import_error, {unsupported_logger_entry, {handler, default, undefined}}},
                 example_codec_logger:import([{handler, default, undefined}])).

logger_wire() ->
    [{handler, default, logger_std_h,
      #{level => error, config => #{file => "log/erlang.log"}}},
     {handler, info, logger_std_h,
      #{level => debug, config => #{file => "log/debug.log"}}}].

%%--------------------------------------------------------------------
%% Schema + sys.config
%%--------------------------------------------------------------------
setup() ->
    start_mgmtd(),
    lists:foreach(fun mgmtd:remove_schema/1, mgmtd:registered_schemas()),
    ok = mgmtd_cfg_db:remove_db(?DB_DIR, [{backend, sys_config}]),
    ok = mgmtd:load_function_schema(fun example:cfg_schema/0,
                                    #{config => true}),
    ok = mgmtd:load_function_schema(fun example:kernel_schema/0,
                                    #{namespace => kernel, config => true}),
    ok = mgmtd_cfg_db:init(?DB_DIR, [{backend, sys_config}]),
    ok.

teardown(_) ->
    lists:foreach(fun mgmtd:remove_schema/1, mgmtd:registered_schemas()),
    ok = mgmtd_cfg_db:remove_db(?DB_DIR, [{backend, sys_config}]),
    ok.

start_mgmtd() ->
    case mgmtd_sup:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

schema_codec_opt_test() ->
    start_mgmtd(),
    lists:foreach(fun mgmtd:remove_schema/1, mgmtd:registered_schemas()),
    ok = mgmtd:load_function_schema(fun example:kernel_schema/0,
                                    #{namespace => kernel, config => true}),
    try
        #{opts := Opts, node_type := list, key_names := ["id"]} =
            mgmtd_schema:lookup(["kernel", "logger"]),
        ?assertEqual(example_codec_logger,
                     mgmtd_schema:codec(["kernel", "logger"])),
        ?assertEqual({codec, example_codec_logger},
                     lists:keyfind(codec, 1, Opts))
    after
        mgmtd:remove_schema(kernel)
    end.

logger_sys_config_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [fun load_otp_snippet/0,
      fun commit_writes_otp_shape/0,
      fun commit_without_module_defaults_std_h/0,
      fun reload_from_otp_file/0,
      fun reject_filters_on_import/0]}.

load_otp_snippet() ->
    ok = write_otp_file(),
    ok = reopen(),
    assert_logger_lookups().

commit_writes_otp_shape() ->
    {ok, Txn} = commit_set(["kernel", "logger", {"default"}, "module", "logger_std_h"]),
    {ok, Txn2} = txn_set_commit(Txn, ["kernel", "logger", {"default"}, "level", "error"]),
    {ok, Txn3} = txn_set_commit(Txn2, ["kernel", "logger", {"default"}, "config", "file", "log/erlang.log"]),
    {ok, Txn4} = txn_set_commit(Txn3, ["kernel", "logger", {"info"}, "module", "logger_std_h"]),
    {ok, Txn5} = txn_set_commit(Txn4, ["kernel", "logger", {"info"}, "level", "debug"]),
    {ok, _} = txn_set_commit(Txn5, ["kernel", "logger", {"info"}, "config", "file", "log/debug.log"]),
    {ok, [Term]} = file:consult(sys_config_file()),
    ?assertEqual(?OTP_LOGGER, [{kernel, proplists:get_value(kernel, Term)}]).

%% Matches `set kernel logger def level error config file /u01/log`
%% with no module leaf — OTP still needs a callback module in the tuple.
commit_without_module_defaults_std_h() ->
    {ok, Txn} = commit_set(["kernel", "logger", {"def"}, "level", "error"]),
    {ok, _} = txn_set_commit(Txn, ["kernel", "logger", {"def"}, "config", "file", "/u01/log"]),
    {ok, [Term]} = file:consult(sys_config_file()),
    Logger = proplists:get_value(logger, proplists:get_value(kernel, Term)),
    ?assertEqual([{handler, def, logger_std_h,
                   #{level => error, config => #{file => "/u01/log"}}}],
                 Logger),
    ok = reopen(),
    ?assertEqual({ok, "logger_std_h"},
                 mgmtd:lookup(["kernel", "logger", {"def"}, "module"])),
    ?assertEqual({ok, "error"},
                 mgmtd:lookup(["kernel", "logger", {"def"}, "level"])),
    ?assertEqual({ok, "/u01/log"},
                 mgmtd:lookup(["kernel", "logger", {"def"}, "config", "file"])).

reload_from_otp_file() ->
    {ok, _} = commit_set(["kernel", "logger", {"default"}, "module", "logger_std_h"]),
    ok = write_otp_file(),
    ok = reopen(),
    assert_logger_lookups(),
    {ok, [Term]} = file:consult(sys_config_file()),
    ?assertEqual(?OTP_LOGGER, [{kernel, proplists:get_value(kernel, Term)}]).

reject_filters_on_import() ->
    Term =
        [{kernel,
          [{logger,
            [{filters, log, []}]}]}],
    ok = mgmtd_cfg_db:remove_db(?DB_DIR, [{backend, sys_config}]),
    ok = filelib:ensure_dir(sys_config_file()),
    ok = file:write_file(sys_config_file(),
                         mgmtd_cfg_db_sys_config:format_consult(Term)),
    ?assertMatch({error, {unsupported_logger_entry, {filters, log, []}}},
                 mgmtd_cfg_db:init(?DB_DIR, [{backend, sys_config}])).

%%--------------------------------------------------------------------
assert_logger_lookups() ->
    Default = ["kernel", "logger", {"default"}],
    Info = ["kernel", "logger", {"info"}],
    ?assertEqual({ok, "default"}, mgmtd:lookup(Default ++ ["id"])),
    ?assertEqual({ok, "logger_std_h"}, mgmtd:lookup(Default ++ ["module"])),
    ?assertEqual({ok, "error"}, mgmtd:lookup(Default ++ ["level"])),
    ?assertEqual({ok, "log/erlang.log"}, mgmtd:lookup(Default ++ ["config", "file"])),
    ?assertEqual({ok, "info"}, mgmtd:lookup(Info ++ ["id"])),
    ?assertEqual({ok, "debug"}, mgmtd:lookup(Info ++ ["level"])),
    ?assertEqual({ok, "log/debug.log"}, mgmtd:lookup(Info ++ ["config", "file"])),
    {ok, Keys} = mgmtd:lookup(["kernel", "logger"]),
    ?assertEqual([{"default"}, {"info"}], lists:sort(Keys)).

write_otp_file() ->
    ok = filelib:ensure_dir(sys_config_file()),
    %% Keep any default-prefix rows already committed; replace the kernel slot.
    Term =
        case file:consult(sys_config_file()) of
            {ok, [Existing]} when is_list(Existing) ->
                lists:keystore(kernel, 1, Existing, hd(?OTP_LOGGER));
            _ ->
                ?OTP_LOGGER
        end,
    ok = file:write_file(sys_config_file(),
                         mgmtd_cfg_db_sys_config:format_consult(Term)).

sys_config_file() ->
    filename:join(?DB_DIR, "sys.config").

reopen() ->
    case ets:info(mgmtd_cfg) of
        undefined -> ok;
        _ -> ets:delete(mgmtd_cfg)
    end,
    mgmtd_cfg_db:init(?DB_DIR, [{backend, sys_config}]).

commit_set(Path) ->
    Txn = mgmtd:txn_new(),
    txn_set_commit(Txn, Path).

txn_set_commit(Txn, Path) ->
    {ok, SchemaPath} = mgmtd_schema:lookup_path(Path),
    {ok, Txn2} = mgmtd:txn_set(Txn, SchemaPath),
    mgmtd:txn_commit(Txn2).
