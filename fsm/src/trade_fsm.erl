-module(trade_fsm).
-author("cky").
-behavior(gen_fsm).
-compile(export_all).
-record(state,{name="", other ="", ownitems = [], otheritems = [], monitor, from}).

start(Name) -> gen_fsm:start(?MODULE, [Name], []).
start_link(Name) -> gen_fsm:start_link(?MODULE, [Name], []).
trade(OwnPid, OtherPid) -> gen_fsm:sync_send_event(OwnPid, {negotiate, OtherPid}, 30000).
accept_trade(OwnPid) -> gen_fsm:sync_send_event(OwnPid, accept_negotiate).
make_offer(OwnPid, Item) -> gen_fsm:send_event(OwnPid, {make_offer, Item}).
do_offer(OtherPid, Item) -> gen_fsm:send_event(OtherPid, {do_offer, Item}).
undo_offer(OtherPid, Item) -> gen_fsm:send_event(OtherPid, {undo_offer, Item}).
retract_offer(OwnPid, Item) -> gen_fsm:send_event(OwnPid, {retract_offer, Item}).
ready(OwnPid) -> gen_fsm:sync_send_event(OwnPid, ready, infinity).
cancel(OwnPid) -> gen_fsm:sync_send_all_state_event(OwnPid, cancel).
ask_negotiate(OtherPid, OwnPid) -> gen_fsm:send_event(OtherPid, {ask_negotiate, OwnPid}).
accept_negotiate(OtherPid, OwnPid) -> gen_fsm:send_event(OtherPid, {accept_negotiate, OwnPid}).
are_you_ready(OtherPid) -> gen_fsm:send_event(OtherPid, are_you_ready).
not_yet(OtherPid) -> gen_fsm:send_event(OtherPid, not_yet).
am_ready(OtherPid) -> gen_fsm:send_event(OtherPid, 'ready!').
ack_trans(OtherPid) -> gen_fsm:send_event(OtherPid, ack).
ask_commit(OtherPid) -> gen_fsm:sync_send_event(OtherPid, ask_commit).
do_commit(OtherPid) -> gen_fsm:sync_send_event(OtherPid, do_commit).
notify_cancel(OtherPid) -> gen_fsm:send_all_state_event(OtherPid, cancel).

init(Name) -> {ok, idle, #state{name=Name}}.
notice(#state{name=N}, Str, Args) -> io:format("~s : " ++ Str ++ "~n", [ N | Args]).
unexpected(Msg, State) -> io:format("~p received unknown event ~p while in state ~p~n", [self(), Msg, State]).

% async idle
idle({ask_negotiate, OtherPid}, S=#state{}) ->
  Ref = monitor(process, OtherPid),
  notice(S, "~p asked for a trade negotiation",[OtherPid]),
  {next_state, idle_wait, S#state{other=OtherPid, monitor=Ref}};
idle(Event, Data) ->
  unexpected(Event, idle),
  {next_state, idle, Data}.

% sync idle
idle({ask_negotiate, OtherPid}, From, S=#state{}) ->
  ask_negotiate(OtherPid, self()),
  notice(S, "asking user ~p for a trade", [OtherPid]),
  Ref = monitor(process, OtherPid),
  {next_state, idle_wait, S#state{other=OtherPid, monitor = Ref, from=From}};
idle(Event, _From, Data) ->
  unexpected(Event, idle),
  {next_state,idle,Data}.

idle_wait({ask_negotiate, OtherPid}, S = #state{other=OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiate", []),
  {next_state, negotiate, S};
idle_wait({accept_negotiate, OtherPid}, S = #state{other = OtherPid}) ->
  gen_fsm:reply(S#state.from, ok),
  notice(S, "starting negotiate", []),
  {next_state, negotiate, S};
idle_wait(Event, Data) -> unexpected(Event, idle_wait), {next_state, idle_wait, Data}.

idle_wait(accept_negotiate, _From, S = #state{other=OtherPid}) ->
  accept_negotiate(OtherPid, self()),
  notice(S, "starting negotiate", []),
  {reply, ok, negotiate, S};
idle_wait(Event, _From, Data) -> unexpected(Event, idle_wait), {next_state, idle_wait, Data}.

add(Item, Items) ->
  [Item | Items].

remove( Item, Items ) ->
  Items -- [Item].

negotiate({make_offer, Item}, S = #state{ownitems=OwnItems}) ->
  do_offer(S#state.other, Item),
  notice(S,"offering ~p", [Item]),
  {next_state, negotiate, S#state{ownitems = add(Item, OwnItems)}};
% 撤销一件商品
negotiate({retract_offer, Item}, S = #state{ownitems=OwnItems}) ->
  undo_offer(S#state.other, Item),
  notice(S, "cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{ownitems = remove(Item, OwnItems)}};

% 对方提供一件商品
negotiate({do_offer, Item}, S = #state{otheritems = OtherItems}) ->
  notice(S, "other player offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = add(Item, OtherItems)}};

% 对方撤销一件商品
negotiate({undo_offer, Item}, S = #state{otheritems = OtherItems}) ->
  notice(S, "other player cancelling offer on ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = remove(Item, OtherItems)}};
negotiate(are_you_ready, S = #state{other = OtherPid}) ->
  io:format("other user ready to trade. ~n"),
  notice(S, "transfer goods : ~n You got ~p , The other get ~p", [S#state.otheritems, S#state.ownitems]),
  not_yet(OtherPid),
  {next_state, negotiate, S};
negotiate(Event, Data) ->
  unexpected(Event, negotiate),
  {next_state, negotiate, Data}.
negotiate(ready, From, S = #state{other = OtherPid}) ->
  are_you_ready(OtherPid),
  notice(S, "asking if ready, waiting ", []),
  {next_state, wait, S#state{from=From}};
negotiate(Event, _From, S) ->
  unexpected(Event, negotiate),
  {next_state,negotiate, S}.

wait({do_offer, Item}, S = #state{otheritems = OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "other side offering ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = add(Item, OtherItems)}};
wait({undo_offer, Item}, S=#state{otheritems = OtherItems}) ->
  gen_fsm:reply(S#state.from, offer_changed),
  notice(S, "Other side cancelling offer of ~p", [Item]),
  {next_state, negotiate, S#state{otheritems = remove(Item, OtherItems)}};
wait(are_you_ready, S = #state{}) ->
  am_ready(S#state.other),
  notice(S, "asked if ready , and i am . Waiting for same reply ", []),
  {next_state, wait, S};
wait(not_yet, S = #state{}) ->
  notice(S, "Other not ready yet", []),
  {next_state, wait, S};
wait('ready!', S = #state{} ) ->
  am_ready(S#state.other),
  ack_trans(S#state.other),
  gen_fsm:reply(S#state.from, ok ),
  notice(S, "other side is ready. Moving to ready state", []),
  {next_state, ready, S};
wait(Event, Data) ->
  unexpected(Event, wait),
  {next_state, wait, Data}.

% ready 状态收到 ack
ready(ack, S = #state{}) ->
  case priority(self(), S#state.other) of
    true ->
      try
        notice(S, "asking for commit ", []),
        ready_commit = ask_commit(S#state.other),
        notice(S, "ordering commit", []),
        ok = do_commit(S#state.other),
        notice(S, "committing ...", []),
        commit(S),
        {stop, normal, S}
      catch Class:Reason ->
        notice(S, "commit failed", []),
        {stop, {Class, Reason}, S}
      end;
    false -> {next_state, ready, S}
  end;
ready(Event, Data) ->
  unexpected(Event, ready),
  {next_state, ready, Data}.

priority(OwnPid, OtherPid) when OwnPid > OtherPid -> true;
priority(OwnPid, OtherPid) when OwnPid < OtherPid -> false.

commit(S = #state{}) ->
  io:format(
    "交易完成 ~s ~n"
    "发送: ~n ~p ~n"
    "收到: ~n ~p ~n",
    [S#state.name, S#state.ownitems, S#state.otheritems]
  ).

% 收到对方的消息，停止本进程
handle_event(cancel, _StateName, S = #state{}) ->
  notice(S, "received cancel event ", []),
  {stop, other_cancelled, S};
handle_event(Event, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

% 本方玩家取消了交易，通知对方玩家退出
handle_sync_event(cancel, _From, _StateName, S = #state{} ) ->
  notify_cancel(S#state.other),
  notice(S, "canceling trade, sending cancel event ", []),
  {stop, cancelled, ok, S};
handle_sync_event(Event, _From, StateName, Data) ->
  unexpected(Event, StateName),
  {next_state, StateName, Data}.

% 代码热升级
code_change(_OldVersion, StateName, Data, _Extra) ->
  {ok, StateName, Data}.

% 处理对方 FSM 崩溃问题
handle_info({'DOWN', Ref, process, Pid, Reason}, _, S = #state{other = Pid, monitor = Ref}) ->
  notice(S, "Other side dead", []),
  {stop, {other_down, Reason}, S};
handle_info(Info, StateName, Data) ->
  unexpected(Info, StateName),
  {next_state, StateName, Data}.

terminate(normal, ready, S=#state{}) ->
  notice(S, "FSM leaving.", []);
terminate(_Reason, _StateName, _StateData) -> ok.

