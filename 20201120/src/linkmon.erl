-module(linkmon).
-author("cky").
-compile([debug_info, export_all]). % 编译选项: 加入调试信息，导出全部函数

myproc() ->
  timer:sleep(5000),
  exit(no_reason).

chain(0) ->
  receive
    _ -> ok
  after 3000 ->
    exit("chain dies here")
  end;
chain(N) ->
  spawn_link( fun() -> chain(N - 1) end ),
  receive
    _ -> ok
  end.

judge(Pid ,Band) ->
  Pid ! {self(), {Band}},
  receive
    {Pid, Criticism}
      -> Criticism
  after 2000 ->
    timeout
  end.

start_critic() ->
  spawn(?MODULE, critic, []).

start_critic2() ->
  spawn(?MODULE, protect_critic, []).

judge2(X) ->
  critic ! {self(),X},
  Pid = whereis(critic),
  receive
    {Pid, Y} -> Y
  after 2000 ->
    timeout
  end.

protect_critic() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, critic, []),
  register(critic, Pid),                  % 给进程命名, 挂到注册树上
  receive
    {'EXIT', Pid, normal} -> ok;          % exit normal
    {'EXIT', Pid, shutdown} -> ok;        % exit by hand
    {'EXIT', Pid, _} -> protect_critic()  % restart it
  end.

critic() ->
  receive
    {From, {"a"}}           -> From ! {self(),"A"};
    {From, {"b"}}           -> From ! {self(),"B"};
    {From, {X}}             -> From ! {self(),X}, X;
    {From, {_Band, _Album}} -> From ! {self(),_Band, _Album}
  end,
  critic().

