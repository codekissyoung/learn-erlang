-module(linkmon).
-author("cky").
-compile([debug_info, export_all]). % 编译选项: 加入调试信息，导出全部函数

% 给 Client 调用的
send(X) ->
  Ref = make_ref(),
  critic ! {self(), Ref, {X}},
  receive
    {Ref, Y} -> Y
  after 2000 ->
    timeout
  end.

% 启动一个服务进程
start() ->
  spawn(?MODULE, protect_critic, []).

% critic 的保护进程
protect_critic() ->
  process_flag(trap_exit, true),                  % 系统进程
  Pid = spawn_link(?MODULE, critic, []),
  register(critic, Pid),                          % 给进程命名, 挂到注册树上
  receive
    {'EXIT', Pid, normal}     -> ok;              % exit normal
    {'EXIT', Pid, shutdown}   -> ok;              % exit by hand
    {'EXIT', Pid, _}          -> protect_critic() % restart it
  end.

% critic 进程
critic() ->
  receive
    {From, Ref, {"a"}}           -> From ! {Ref, "A"};
    {From, Ref, {"b"}}           -> From ! {Ref, "B"};
    {From, Ref, {X}}             -> From ! {Ref, X};
    {From, Ref, _}               -> From ! {Ref, unkown}
  end,
  critic().
