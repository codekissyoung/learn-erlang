-module(kitty_gen_server).
-author("cky").
-export([]).
-compile(export_all).
-behavior(gen_server).
-record(cat, {name, color=green, description}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

order_cat(Pid, Name, Color, Desc) ->
  gen_server:call(Pid, {order, Name, Color, Desc}).

return_cat(Pid, Cat = #cat{}) ->
  gen_server:cast(Pid, {return, Cat}).

close_shop(Pid) ->
  gen_server:call(Pid, terminate).

init([]) -> {ok, []}.

handle_call({order, Name, Color, Desp}, _From, Cats) ->
  if
  Cats =:= [] ->
    {reply, kitty_server:make_cat(Name, Color, Desp),Cats};
  Cats =/= [] ->
    {reply, hd(Cats), tl(Cats)}
  end;
handle_call(terminate, _From, Cats) ->
  {stop, normal, ok, Cats}.

handle_cast({return, Cat = #cat{}}, Cats) ->
  {noreply, [Cat | Cats]}.

handle_info(Msg, Cats) ->
  io:format("Unexcepted Message : ~p~n", [Msg]),
  {noreply, Cats}.

teminate(normal, Cats) ->
  [io:format("~p was set free. ~n", [C#cat.name]) || C <- Cats],
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description = Desc}.
