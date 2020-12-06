-module(code_lock).
-behavior(gen_fsm).
-export([start_link/1, handle_event/3, handle_sync_event/4]).
-export([button/1]).
-export([init/1, locked/2, open/2]).

%% 启动状态机
%% 通过调用 gen_fsm:start_link/4 启动 gen_fsm. 他应该被 supervisor 调用加入监督树。
%% 在此，启动的时候把锁的密码保存起来，也就是Code。
start_link(Code) ->
  %% (进程名，callback module，给init/1的参数，状态机选项)， 注册成功调用init(Code).
  gen_fsm:start_link({local, code_lock}, code_lock, Code, []).

button(Digit) ->
  %% (进程注册名，事件)，事件被收到会调用以当前状态为名的函数,参数是(事件名，状态数据)。
  gen_fsm:send_event(code_lock, {button, Digit}).

%% 返回的第二项是锁的初始状态,同时也是出现该状态要执行的回调函数
%% 这个值应该是一个 atom 类型，因为后面会执行由此命令的函数。
init(Code) -> {ok, locked, {[], Code}}.

locked({button, Digit}, {SoFar, Code}) ->
  case [Digit|SoFar] of
    Code ->
      do_unlock(),
      {next_state, open, {[], Code}, 3000};
    Incomplete when length(Incomplete) < length(Code) ->
      {next_state, locked, {Incomplete, Code}};
    _Wrong ->
      {next_state, locked, {[], Code}}
  end.

open(timeout, State) ->
  do_lock(),
  {next_state, locked, State}.

handle_event(Event, StateName, StateData) ->
  erlang:error(not_implemented).

handle_sync_event(Event, From, StateName, StateData) ->
  erlang:error(not_implemented).