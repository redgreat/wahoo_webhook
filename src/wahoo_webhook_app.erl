%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%% @doc
%%
%% eadm public API
%%
%% @end
%%% Created : 2024-01-23 17:27:13
%%%-------------------------------------------------------------------
-module(wahoo_webhook_app).
-author("wangcw").

%%%===================================================================
%%% 行为
%%%===================================================================
-behaviour(application).

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([start/2, stop/1]).

%%====================================================================
%% API 函数
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    application:start(lager),
    wahoo_webhook_sup:start_link().

start() ->
    WebhookToken = "3006dd15-2514-4b21-8269-24fa90523786",
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/webhook/" ++ WebhookToken, webhook_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(my_http_listener, 100, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    io:format("Cowboy started on port 8080~n").

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    application:stop(lager),
    ok.
