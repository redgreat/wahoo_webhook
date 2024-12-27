%%%-------------------------------------------------------------------
%%% @author wangcw
%%% @copyright (C) 2024, REDGREAT
%%% @doc
%%%
%%% 
%%%
%%% @end
%%% Created : 2024-12-24 19:39
%%%-------------------------------------------------------------------
-module(wahoo_handler).
-author("wangcw").
-behaviour(cowboy_http_handler).

%%%===================================================================
%%% 函数导出
%%%===================================================================
-export([init/3, handle/2, terminate/3]).

%%====================================================================
%% API 函数
%%====================================================================
init(_, _, _) ->
  {ok, _} = cowboy:static(init:webhook_token()), % 初始化静态变量
  {ok, State}.

handle(Req, State) ->
  try
    case cowboy_req:method(Req) of
      <<"POST">> ->
        {true, Req1, Body} = cowboy_req:read_body(Req),
        Data = jiffy:decode(Body),
        io:format("Received Webhook Data: ~p~n", [Data]),
        % 这里可以添加处理数据的代码
        {ok, Req2} = cowboy_req:reply(200, [], <<"Webhook received">>, Req1),
        {ok, Req2, State};
      _ ->
        {ok, Req2} = cowboy_req:reply(405, Req),
        {ok, Req2, State}
    end
  catch
    _:_ ->
      {ok, Req2} = cowboy_req:reply(500, [], <<"Internal server error">>, Req),
      {ok, Req2, State}
  end.

terminate(_Reason, _Req, _State) ->
  ok.

%%====================================================================
%% 内部函数
%%====================================================================
