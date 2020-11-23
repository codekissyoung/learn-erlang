-module(hello).
-author("cky").
-import(io, [fwrite/1]).
-export([start/0]).


start() ->
  begin
    io:format("hello \t"), % 直接调用模块
    fwrite("world \n")     % import 引入别的模块函数
  end.
