-module(vshlr1).
-author("cky").
-compile(export_all).

% 对外的接口
i_am_at(Who,Where) -> server1:rpc(vshlr1, {i_am_at, Who, Where}).
find(Who) -> server1:rpc(vshlr1, {find, Who}).

% vshlr1 进程
% 启动
start() -> server1:start(vshlr1, fun handle_event/2, dict:new()).

% 停止
stop() -> server1:stop(vshlr1).

% 处理不同事件
handle_event({i_am_at, Who, Where}, Dict) ->
  {ok, dict:store(Who, Where, Dict)};
handle_event({find, Who}, Dict) ->
  {dict:find(Who, Dict), Dict}.
