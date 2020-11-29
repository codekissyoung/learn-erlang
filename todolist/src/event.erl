-module(event).
-author("cky").
-compile(export_all).
-record(state, {server, name="", waitSeconds=0}).

cancel(Pid) ->
  Ref = erlang:monitor(process,Pid),
  Pid ! {self(), Ref, cancel},
  receive
    {Ref, ok} ->
      erlang:demonitor(Ref, [flush]), {Ref, ok}; % 取消成功
    {'DOWN',Ref,process,Pid,_Reason} ->
      {Ref, done} %　取消的时候，对方进程已经死亡
  end.

start(EventName, Delay) ->
  spawn(?MODULE, init, [self(),EventName,Delay]).

start_link(EventName, Delay) ->
  spawn(?MODULE, init, [self(),EventName,Delay]).

init(Server, EventName, Delay) ->
  loop(#state{server=Server, name=EventName, waitSeconds = normalize(Delay)}).

loop(S = #state{server=Server}) ->
  receive
    {Server, Ref, cancel } -> Server ! {Ref, ok}
  after S#state.waitSeconds ->
    Server ! {done, S#state.name}
  end.

normalize(Delay) ->
  Delay * 1000.

