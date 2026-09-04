%%%-------------------------------------------------------------------
%%% @doc Operational-data provider for the example application.
%%%
%%% Serves the `status` tree: VM identity, running echo servers, and
%%% host network interfaces. Named as `data_callback` on the schema.
%%% Server port/state come from the live process, not from config.
%%% @end
%%%-------------------------------------------------------------------
-module(example_provider).

-behaviour(mgmtd_provider).

-export([schema/0]).
-export([get_value/1, get_first/1, get_next/2, list_keys/3]).

-include_lib("mgmtd/include/mgmtd.hrl").

%%--------------------------------------------------------------------
%% Schema (`config = false`). `data_callback` is inherited by children.
%%--------------------------------------------------------------------
schema() ->
    [#container{name = "status",
                desc = "Operational status",
                config = false,
                data_callback = ?MODULE,
                children = fun status_schema/0}].

status_schema() ->
    [#container{name = "system",
                desc = "Erlang node",
                children = fun system_schema/0},
     #list{name = "servers",
           desc = "Running echo servers",
           key_names = ["name"],
           children = fun server_status_schema/0},
     #list{name = "interfaces",
           desc = "Host network interfaces",
           key_names = ["name"],
           children = fun iface_schema/0}].

system_schema() ->
    [#leaf{name = "node",
           desc = "Erlang node name",
           type = string},
     #leaf{name = "otp-release",
           desc = "OTP release",
           type = string},
     #leaf{name = "uptime",
           desc = "Time since the VM started",
           type = string},
     #leaf{name = "processes",
           desc = "Number of Erlang processes",
           type = uint32},
     #leaf{name = "memory",
           desc = "Total memory in bytes",
           type = uint64}].

server_status_schema() ->
    [#leaf{name = "name",
           desc = "Server name",
           type = string},
     #leaf{name = "pid",
           desc = "Erlang pid",
           type = string},
     #leaf{name = "port",
           desc = "Listen port",
           type = 'inet:port-number'},
     #leaf{name = "state",
           desc = "Process / socket state",
           type = {enum, ["up", "down", "listening"]}}].

iface_schema() ->
    [#leaf{name = "name",
           desc = "Interface name",
           type = string},
     #leaf{name = "address",
           desc = "IPv4 address",
           type = 'inet:ip-address'},
     #leaf{name = "mac",
           desc = "Hardware address",
           type = string},
     #leaf{name = "mtu",
           desc = "MTU",
           type = uint32},
     #leaf_list{name = "flags",
                desc = "Interface flags",
                type = string}].

%%--------------------------------------------------------------------
%% mgmtd_provider
%%--------------------------------------------------------------------
get_value(["status", "system", "node"]) ->
    {ok, atom_to_list(node())};
get_value(["status", "system", "otp-release"]) ->
    {ok, erlang:system_info(otp_release)};
get_value(["status", "system", "uptime"]) ->
    {TotalMs, _} = erlang:statistics(wall_clock),
    {ok, format_uptime(TotalMs)};
get_value(["status", "system", "processes"]) ->
    {ok, erlang:system_info(process_count)};
get_value(["status", "system", "memory"]) ->
    {ok, erlang:memory(total)};
get_value(["status", "servers", Key, "pid"]) ->
    case server_pid(Key) of
        undefined ->
            {ok, not_found};
        Pid ->
            {ok, pid_to_list(Pid)}
    end;
get_value(["status", "servers", Key, "port"]) ->
    case live_status(Key) of
        #{port := Port} when is_integer(Port) ->
            {ok, Port};
        _ ->
            {ok, not_found}
    end;
get_value(["status", "servers", Key, "state"]) ->
    {ok, server_state(Key)};
get_value(["status", "interfaces", {Name}, "address"]) ->
    if_prop(Name, address);
get_value(["status", "interfaces", {Name}, "mac"]) ->
    if_prop(Name, mac);
get_value(["status", "interfaces", {Name}, "mtu"]) ->
    if_prop(Name, mtu);
get_value(["status", "interfaces", {Name}, "flags"]) ->
    if_prop(Name, flags);
get_value(_Path) ->
    {ok, not_found}.

get_first(["status", "servers"]) ->
    first_key(server_keys());
get_first(["status", "interfaces"]) ->
    first_key(iface_keys());
get_first(_Path) ->
    {ok, not_found}.

get_next(["status", "servers"], Prev) ->
    next_key(server_keys(), Prev);
get_next(["status", "interfaces"], Prev) ->
    next_key(iface_keys(), Prev);
get_next(_Path, _Prev) ->
    {ok, not_found}.

list_keys(_Txn, Path, Match) ->
    case mgmtd_provider:list_keys(?MODULE, Path, Match) of
        {ok, Keys} ->
            Keys;
        {error, _} ->
            []
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

first_key([Key | _]) ->
    {ok, Key};
first_key([]) ->
    {ok, not_found}.

next_key([Prev, Next | _], Prev) ->
    {ok, Next};
next_key([_ | Rest], Prev) ->
    next_key(Rest, Prev);
next_key(_, _) ->
    {ok, not_found}.

server_keys() ->
    lists:sort([Id || {Id, Pid, _, _} <- server_children(), is_pid(Pid)]).

server_pid(Key) ->
    case lists:keyfind(Key, 1, server_children()) of
        {_, Pid, _, _} when is_pid(Pid) ->
            Pid;
        _ ->
            undefined
    end.

server_children() ->
    case erlang:whereis(example_server_sup) of
        undefined ->
            [];
        _Pid ->
            supervisor:which_children(example_server_sup)
    end.

live_status(Key) ->
    case server_pid(Key) of
        Pid when is_pid(Pid) ->
            example_echo_server:status(Pid);
        undefined ->
            undefined
    end.

server_state(Key) ->
    case live_status(Key) of
        #{listening := true} ->
            "listening";
        #{} ->
            "up";
        undefined ->
            "down"
    end.

iface_keys() ->
    [{Name} || {Name, _} <- ifs()].

ifs() ->
    case inet:getifaddrs() of
        {ok, Ifs} ->
            lists:keysort(1, Ifs);
        {error, _} ->
            []
    end.

if_prop(Name, Field) ->
    case lists:keyfind(Name, 1, ifs()) of
        {_, Props} ->
            case if_field(Field, Props) of
                undefined ->
                    {ok, not_found};
                Value ->
                    {ok, Value}
            end;
        false ->
            {ok, not_found}
    end.

if_field(address, Props) ->
    case lists:keyfind(addr, 1, Props) of
        {addr, Addr} when tuple_size(Addr) =:= 4 ->
            Addr;
        _ ->
            first_ipv4(Props)
    end;
if_field(mac, Props) ->
    case lists:keyfind(hwaddr, 1, Props) of
        {hwaddr, Bytes} when is_list(Bytes), Bytes =/= [] ->
            format_mac(Bytes);
        _ ->
            undefined
    end;
if_field(mtu, Props) ->
    case lists:keyfind(mtu, 1, Props) of
        {mtu, Mtu} when is_integer(Mtu) ->
            Mtu;
        _ ->
            undefined
    end;
if_field(flags, Props) ->
    case lists:keyfind(flags, 1, Props) of
        {flags, Flags} when is_list(Flags) ->
            [atom_to_list(F) || F <- Flags, is_atom(F)];
        _ ->
            []
    end.

first_ipv4([{addr, Addr} | _]) when tuple_size(Addr) =:= 4 ->
    Addr;
first_ipv4([_ | Rest]) ->
    first_ipv4(Rest);
first_ipv4([]) ->
    undefined.

format_mac(Bytes) ->
    lists:flatten(
      string:join([io_lib:format("~2.16.0b", [B]) || B <- Bytes], ":")).

format_uptime(Ms) ->
    Sec = Ms div 1000,
    {D, {H, M, S}} = calendar:seconds_to_daystime(Sec),
    lists:flatten(io_lib:format("~p days, ~2..0B:~2..0B:~2..0B", [D, H, M, S])).
