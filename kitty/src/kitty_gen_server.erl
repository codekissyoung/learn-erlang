-module(kitty_gen_server).
-author("cky").
-export([]).
-compile(export_all).
% gen_server 是一种行为
% 本句代码含义是，gen_server 期望本模块实现 一组函数: init/1 handle_call ...
-behavior(gen_server).
-record(cat, {name, color=green, description}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

order_cat(Pid, Name, Color, Desc) ->
  % call 默认超时 5 秒，如果超过，本调用会奔溃
  gen_server:call(Pid, {order, Name, Color, Desc}).

return_cat(Pid, Cat = #cat{}) ->
  gen_server:cast(Pid, {return, Cat}).

close_shop(Pid) ->
  gen_server:call(Pid, terminate).

% 回调函数，负责初始化服务器的状态，可以返回
% {ok, State} {ok, State, TimeOUt} {ok, State, hibernate}
% {stop, Reason} ignore
% State 会传递给进程的主循环
% 理解成construct
init([]) -> {ok, []}.

% 处理同步消息
% 参数是 Request From State
% 有8种返回值可以选择
% {reply, Reply, NewState} {reply, Reply, NewState, TimeOut} {reply, Reply, NewState, hibernate}
% {noreply, NewState} {noreply, NewState, TimeOut} {noreply, NewState, hibernate}
% {stop, Reason, Reply, NewState} {stop, Reason, NewState}
handle_call({order, Name, Color, Desp}, _From, Cats) ->
  if
  Cats =:= [] ->
    {reply, kitty_server:make_cat(Name, Color, Desp),Cats};
  Cats =/= [] ->
    {reply, hd(Cats), tl(Cats)}
  end;
handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.

% 处理异步消息, 可以返回
% 参数是 Message State
% {noreply, NewState} {noreply, NewState, TimeOut}
% {noreply, NewState, hibernate} {stop, Reason, NewState}
handle_cast({return, Cat = #cat{}}, Cats) ->
  {noreply, [Cat | Cats]}.

% 处理和接口不相容的消息，即直接通过 ! 发送的消息
% 比如 init/1 中的 timeout ,监控器通知　EXIT 信号等
handle_info(Msg, Cats) ->
  io:format("Unexcepted Message : ~p~n", [Msg]),
  {noreply, Cats}.

% handel_xxx 系列函数，返回 {stop, xxx} 元组时，会调用此函数
% 当父进程死亡时候，也会发送一个退出信号过来，此时也会调用本函数
% 理解成 destruct,完成所有清理工作
teminate(_Reason, Cats) ->
  [io:format("~p was set free. ~n", [C#cat.name]) || C <- Cats],
  ok.

% 用于代码升级
% 参数 PreviousVersion State Extra
code_change(_OldVsn, State, _Extra) -> {ok, State}.

make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description = Desc}.
