-module(server1).
-author("cky").
-compile(export_all).

start(Name, F, State) ->
  register(Name, spawn(
    fun() ->
      loop(Name, F, State)
    end
  )).

stop(Name) -> Name ! stop.

swap_code(Name, F) -> rpc(Name, {swap_code, F}).

loop(Name, F, State) ->
  receive
    stop -> void;

    % 更换 F 函数
    {From, {swap_code, F1}} ->
      From ! {Name, ok, ack},
      loop(Name, F1, State);

    {From, Query} ->
      % 假如提供 F 内部有问题，进程也不应该崩溃，而是转化为一个消息通知客户端
      case (catch F(Query, State)) of
        {'EXIT', Why} ->
          log_error(Name, Query, Why),
          From ! {Name, crash},
          loop(Name, F, State); % 操作失败后，服务器保持状态不变
        {Reply, State1} ->
          From ! {Name, ok, Reply},
          loop(Name, F, State1) % 操作成功，服务器转为新状态
      end
  end.

rpc(Name, Query) ->
  Name ! {self(), Query},
  receive
    {Name, crash} -> exit(rpc);
    {Name, ok, Reply} -> Reply
  after 10000 ->
    exit(timeout)
  end.

log_error(Name, Query, Why) ->
  io:format("Server ~p query ~p caused exception ~p~n",
    [Name, Query, Why]).