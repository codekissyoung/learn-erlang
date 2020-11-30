-module(kitty_server).
-compile(export_all).
-record(cat, {name, color=green, description}).

% 同步调用
order_cat(Pid, Name, Color, Desc) -> my_server:call(Pid, {order, Name, Color, Desc}).

% 异步调用，立即返回
return_cat(Pid, Cat = #cat{}) -> my_server:cast(Pid, {return, Cat}).

% 同步调用
close_shop(Pid) -> my_server:call(Pid, terminate).

start_link() -> my_server:start_link(?MODULE, []).

init([]) -> [].

handle_call({order, Name, Color,Desc}, From, Cats) ->
  if
    Cats =:= [] ->
      my_server:reply(From, make_cat(Name, Color, Desc)), Cats;
    Cats =/= [] ->
      my_server:reply(From, hd(Cats)),
      tl(Cats)
  end;

handle_call(terminate, From, Cats) ->
  my_server:reply(From,ok),
  terminate(Cats).

handle_cast({return, Cat = #cat{}}, Cats) ->
  [Cat | Cats].

make_cat(Name, Col, Desc) ->
  #cat{name=Name, color=Col, description = Desc}.

terminate(Cats) ->
  [io:format("~p was set free. ~n", [C#cat.name]) || C <- Cats ],
  exit(normal).
