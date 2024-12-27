%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%% @doc
%%
%% eadm top level supervisor.
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
    %% lager:info("数据库连接参数：~p~n", [Pools]),
    PoolSpec = lists:map(fun ({PoolName, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, PoolName}},
            {worker_module, eadm_pgpool_worker}] ++ SizeArgs,
        poolboy:child_spec(PoolName, PoolArgs, WorkerArgs)
                         end, Pools),
    {ok, { {one_for_one, 10, 10}, PoolSpec} },

    %% Oracle Connect Start.
    ConnOpts = application:get_env(eorcl, []),
    {ok, OraConnRef} = jamdb_oracle:start([{role, 1},{prelim, 1}]++ConnOpts),

    %% CowBoy Start.
    WebhookToken = "3006dd15-2514-4b21-8269-24fa90523786",
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/webhook/" ++ WebhookToken, webhook_handler, []}
            ]
        }
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener, 100, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    io:format("Cowboy started on port 8080~n").

add_pool(Name, PoolArgs, WorkerArgs) ->
    ChildSpec = poolboy:child_spec(Name, PoolArgs, WorkerArgs),
    supervisor:start_child(?MODULE, ChildSpec).
