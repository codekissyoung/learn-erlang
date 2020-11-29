#!/usr/bin/env escript
%% 将参数转换成整数类型后调用阶乘函数, A 是命令行参数
%% ./fac.sh 4
%% factorial 4 = 24
main([A]) ->
    I = list_to_integer(A),
    F = fac(I),
    io:format("factorial ~w = ~w~n", [I, F]),
    init:stop().

fac(0) ->1;
fac(N) ->N*fac(N-1).