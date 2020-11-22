-module(error).
-author("cky").
-export([generate_exception/1,demo1/0]).

generate_exception(1) -> a;

% 抛出一个异常
generate_exception(2) -> throw(a);

% 终止当前进程时使用，如果未捕捉，则会广播　{'EXIT', Pid, Why} 到其他链接到本进程的其他进程
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};

% 崩溃性错误
generate_exception(5) -> error(a).

demo1() ->
  [catcher(I) || I <- [1,2,3,4,5]].

catcher(N) ->
  % N 是入参 Ret 是表达式的返回值
  try generate_exception(N) of Ret ->
    {N, normal, Ret} % 如果表达式执行正常，未抛出异常
  catch
    throw:Ret -> % 抛出 throw 异常
      {N, caught, thrown, Ret};
    exit:Ret -> % 收到 exit 异常
      {N, caught, exited, Ret};
    error:Ret -> % 收到 error 异常
      {N, caught, error, Ret}
  end.

%%1> c("error.erl").
%%{ok,error}
%%2> error:demo1().
%%[{1,normal,a},
%%{2,caught,thrown,a},
%%{3,caught,exited,a},
%%{4,normal,{'EXIT',a}},
%%{5,caught,error,a}]
