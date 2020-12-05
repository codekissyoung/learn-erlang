-module(dog_fsm).
-author("cky").
-compile(export_all).

start() -> spawn(fun() -> bark() end).
pet(Pid) -> Pid ! pet.
squirrel(Pid) -> Pid ! squirrel.

bark() ->
  io:format("Dog say: Bark ~n "),
  receive
    pet ->
      wag_tail();
    _ ->
      io:format("Dog is confused !~n"),
      bark()
  after 2000 ->
    bark()
  end.

wag_tail() ->
  io:format("摇尾巴 ~n "),
  receive
    pet ->
      sit();
    _ ->
      io:format("Dog is confused !~n"),
      wag_tail()
  after 3000 ->
    bark()
  end.

sit() ->
  io:format("坐着 ~n "),
  receive
    squirrel ->
      bark();
    _ ->
      io:format("Dog is confused !~n"),
      sit()
  end.
