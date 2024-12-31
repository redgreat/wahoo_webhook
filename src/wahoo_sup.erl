%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%% @doc
%%
%% wahoo top level supervisor.
%%
%% @end
%%% Created : 2024-01-23 17:30:14
%%%-------------------------------------------------------------------
-module(wahoo_sup).
-author("wangcw").

%%%===================================================================
%%% 函数行为
%%%===================================================================
-behaviour(supervisor).

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([start_link/0, add_pool/3]).
-export([init/1]).

%%%===================================================================
%%% Define
%%%===================================================================
-define(SERVER, ?MODULE).
%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%%====================================================================
%% API 函数
%%====================================================================

%% @private
%% @doc
%% start_link.
%% @end
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @private
%% @doc
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}.
%% @end
init([]) ->
    %% Pg Pool Start.
    Pools = application:get_env(epgsql, pools, []),
    lager:info("PG数据库连接参数：~p~n", [Pools]),
    PoolSpec = lists:map(fun ({PoolName, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, PoolName}},
            {worker_module, eadm_pgpool_worker}] ++ SizeArgs,
        poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)
                         end, Pools),
    {ok, { {one_for_one, 10, 10}, PoolSpec} },

    %% Oracle Connect Start.
    OrclCon = application:get_env(eorcl, []),
    lager:info("Oracle数据库连接参数：~p~n", [OrclCon]),
    {ok, OraConnRef} = jamdb_oracle:start([{role, 1},{prelim, 1}]++OrclCon),

    %% CowBoy Start.
    WebhookToken = application:get_env(wahoo,hook_token, ""),
    HttpPort = application:get_env(wahoo,http_port, 8080),
    lager:info("Http参数：~p~p~n", [WebhookToken, HttpPort]),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/webhook/" ++ WebhookToken, webhook_handler, []}
            ]
        }
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener, 100, [{port, HttpPort}], #{
        env => #{dispatch => Dispatch}
    }),
    io:format("Cowboy started on port 8080~n").

add_pool(Name, PoolArgs, WorkerArgs) ->
    ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
    supervisor:start_child(?MODULE, ChildSpec).
