-module(my_server).
-compile(export_all).


% my_server 待解决的问题：
% 进程命名 超时配置　调试信息　非期望消息处理　代码热加载　特殊错误的处理
% 公共回复代码的提炼　服务器关闭的处理
% 保证服务器和监督者的配合

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