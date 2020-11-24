-module(hello).
-author("cky").
-import(io, [fwrite/1]).
-export([start/0,factories/1,classify_day/1,reverse/1,flat_length/1,factories_p/1]).

start() ->
  begin
    129 + 100,
    io:format("hello \t"), % 直接调用模块
    fwrite("world \n")     % import 引入别的模块函数
  end.

% 计算整数的阶乘
factories(0) -> 1;
factories(N) -> N * factories( N - 1 ).

% 使用保护式写法
factories_p(N)    % 子句头部
  when N == 0 ->  % 保护式 (可选)
    1;            % 函数表达式
factories_p(N)
  when N > 0 ->
%% 写成一行的表达式
%% N * factories_p(N - 1).
%% 拆开写成表达式序列，用 , 隔开，最终的值为　最后一个表达式的值
    N1 = N -1,
    F1 = factories_p(N1),
    N * F1.

% 判断是工作日　还是 周末
classify_day(saturday) -> weekEnd;
classify_day(sunday)   -> weekEnd;
classify_day(_)        -> weekDay. % _ 是精髓

reverse(L) ->
  reverse(L, []).
reverse([H|T], L) ->
  reverse(T, [H|L]);
reverse([], L) ->
  L.

flat_length(List) ->
  flat_length(List, 0).
flat_length([H|T], N) when is_list(H) ->
  flat_length(H, flat_length(T, N));
flat_length([H|T], N) ->
  flat_length(T, N + 1);
flat_length([], N) ->
  N.
