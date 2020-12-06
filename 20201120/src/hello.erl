-module(hello).
-author("cky").
-define(HOUR, 3600). % 3600 秒
-import(io, [fwrite/1]).
-compile(export_all).

start() ->
    X = 100 + ?HOUR,
    io:format("hello " ),
    fwrite("world \n"),    % import 引入别的模块函数
    X.

% 实现尾递归的关键在于，始终保持在展开计算时
% 始终只有一个函数　以及　计算好的 Accumulator 变量
% 计算整数的阶乘　　　　　　　
fac(0) -> 1;
fac(N) -> N * fac( N - 1 ).

% 尾递归优化的阶乘 Acc : Accumulator 累加器
tail_fac(N)
  -> tail_fac(N, 1).
tail_fac(0, Acc)
  -> Acc;
tail_fac(N, Acc) when N > 0 ->
  tail_fac(N-1, N*Acc).

% 递归计算长度
len([]) -> 0;
len([_|T]) -> 1 + len(T).

% 尾递归优化后的求长度
tail_len(L) -> tail_len(L, 0).
tail_len([], Acc) -> Acc;
tail_len([_|T], Acc) -> tail_len(T, Acc+1).

old_enough(X) when X >= 16, X =< 80 ->true;
old_enough(_) -> false.

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
flat_length([_|T], N) ->
  flat_length(T, N + 1);
flat_length([], N) ->
  N.
