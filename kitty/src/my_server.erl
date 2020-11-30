-module(my_server).
-compile(export_all).

start(Module, InitState) ->
  spawn(fun() -> init(Module, InitState) end ).

start_link(Module, InitState) ->
  spawn_link(fun() -> init(Module, InitState) end ).

init(Module, InitState) ->
  loop(Module, Module:init(InitState)).

loop(Module, State) ->
  receive
    {async, Msg} ->
      loop(Module, Module:handle_cast(Msg, State));
    {sync, Pid, Ref, Msg} ->
      loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
  end.

% 同步调用
call(Pid, Msg) ->
  % 监控一个不存在的进程会如何？
  Ref = erlang:monitor(process, Pid),
  Pid ! {sync, self(), Ref, Msg},
  receive
    {Ref, Reply} ->
      erlang:demonitor(Ref, [flush]),
      Reply;
    {'DOWN', Ref, process, Pid, Reason} ->
      erlang:error(Reason)
  after 5000 ->
    erlang:error(timeout)
  end.

% 异步调用
cast(Pid, Msg) ->
  Pid ! {async, Msg},
  ok.

reply({Pid, Ref}, Reply) ->
  Pid ! {Ref, Reply}.