-module(concurence).
-author("link").
-export([dolphin/0]).

dolphin() ->
  receive
    {From, do_a_flip} ->
      From ! "Nice to meet you, give me fish ~",
      dolphin();
    {From, fish} ->
      From ! "thanks"; % 没有递归了，进程或直接退出
    _ ->
      io:format("do nothing ~n"),
      dolphin()
  end.

