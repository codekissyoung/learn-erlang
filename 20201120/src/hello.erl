-module(hello).
-author("cky").
-export([start/0]).

start() ->
  io:format("hello world ~n").
