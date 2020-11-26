-module(linkmon).
-author("cky").
-export([chain/1, myproc/0]).

myproc() ->
  timer:sleep(5000),
  throw({no_reason}).

chain(0) ->
  receive
    _ -> ok
  after 2000 ->
    throw("chain dies here")
  end;
chain(N) ->
  link(spawn( fun() -> chain(N - 1) end )),
  receive
    _ -> ok
  end.