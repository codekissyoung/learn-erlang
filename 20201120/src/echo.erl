-module(echo).
-author("cky").
-export([start/0, loop/0]).

start() -> spawn(?MODULE , loop, []).

loop() ->
  receive
    {From, Message} -> From ! {Message, "Who are you ?"}
  end,
loop().

%%Eshell V9.2  (abort with ^G)
%%1> c("echo.erl").
%%{ok,echo}
%%2> Pid = echo:start().
%%<0.67.0>
%%3> Pid ! {self(), "hello ?"}.
%%{<0.60.0>,"hello ?"}
%%4> receive X -> X end.
%%{"hello ?","Who are you ?"}

