-module(evserv).
-author("cky").
-compile(export_all).
-record(state, {events, clients}).
-record(event, {name="", description="", pid, timeout=10}).

%% 给客户端调用的

%% 关闭服务
terminate() ->
  ?MODULE ! shutdown.

%% 订阅事件
subscribe(Pid) ->
  Ref = erlang:monitor(process, whereis(?MODULE)),
  ?MODULE ! {self(),Ref,{subscribe, Pid}},
  receive
    {Ref, ok } -> {ok, Ref};
    {'DOWN', Ref, process, _pid, Reason} -> {error, Reason}
  after 5000 ->
    {error, timeout}
  end.

%% 添加事件
add_event(Name, Description, Timeout) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {add, Name, Description, Timeout}},
  receive
    {Ref, {error,Reason}} -> erlang:error(Reason); % 让客户端进程崩溃
    {Ref, Msg} -> Msg
  after 5000 ->
    {error, timeout}
  end.

%% 取消事件
cancel(Name) ->
  Ref = make_ref(),
  ?MODULE ! {self(), Ref, {cancel,Name}},
  receive
    {Ref, ok} -> ok
  after 5000 ->
    {error, timeout}
  end.

start() ->
  register(?MODULE, Pid = spawn(?MODULE, init, [])), Pid. % 将模块名作为进程名

start_link() ->
  register(?MODULE, Pid=spawn_link(?MODULE, init, [])), Pid.

init() ->
  % 这里可以放从静态文件中读取事件的逻辑
  loop(#state{events=orddict:new(), clients = orddict:new()}).

loop(S = #state{}) ->
  receive
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 客户端 => 服务端 %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % 新增提醒事件
    {ClientPid, MsgRef, {add, Name, Des, Timeout}} ->
      EventPid = event:start_link(Name, Timeout),
      % 将要提醒的事件存起来，开一个定时器，通知后，将事件取出，广播给所有的客户端
      NewEvent =  #event{name=Name, description = Des, pid=EventPid,timeout = Timeout},
      NewEvents = orddict:store(Name, NewEvent, S#state.events),
      ClientPid ! {MsgRef, "add event ok"},
      loop(S#state{events=NewEvents});

    % 取消提醒事件
    {ClientPid, MsgRef, {cancel, Name}} ->
      Events = case orddict:find(Name, S#state.events) of
                 {ok,E} ->
                   event:cancel(E#event.pid),
                   orddict:erase(Name, S#state.events);
                 error ->
                   S#state.events
               end,
      ClientPid ! {MsgRef, ok},
      loop(S#state{events = Events});

    % 订阅消息
    {FromPid, MsgRef, {subscribe, ClientPid}} ->
      Ref = erlang:monitor(process, ClientPid),
      %% 每条订阅消息的 Ref 作为 Key
      NewClients = orddict:store(Ref, ClientPid, S#state.clients),
      FromPid ! {MsgRef,ok},
      loop(S#state{clients = NewClients});

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%% server 与 event 进程的交互 %%%%%%%%%%%%%%%%%%%%
    {done, Name} ->
      case orddict:find(Name, S#state.events) of
        {ok, E} ->

          ToClientMsg = {oops, E#event.name, E#event.description},
          format_list(orddict:to_list(S#state.clients)),
          send_to_clients(ToClientMsg, S#state.clients),
          NewEvents = orddict:erase(Name, S#state.events),
          loop(S#state{events = NewEvents});
        error ->
          loop(S)
      end;

    shutdown ->
      % 退出前数据落盘可以在这里实现
      exit(shutdown);

    {'DOWN', Ref, process, _Pid, _Reason} ->
      loop(S#state{clients = orddict:erase(Ref,S#state.clients)});

    code_change ->
      ?MODULE:loop(S); % 这里使用全路径，代码热更新

    Unknown -> io:format("Unknown message : ~p~n", [Unknown]),
      loop(S)
  end.

% 向所有订阅的客户端发消息
send_to_clients(Msg, Clients) ->
  format_list(orddict:to_list(Clients)),
  orddict:map(fun(_Ref, ClientPid) -> ClientPid ! Msg end, Clients).

format_list(L) when is_list(L) ->
  io:format("["),
  fnl(L),
  io:format("]").
fnl([H]) ->
  io:format("~p", [H]);
fnl([H|T]) ->
  io:format("~p,", [H]),
  fnl(T);
fnl([]) ->
  ok.