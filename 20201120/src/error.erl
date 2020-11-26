-module(error).
-author("cky").
-export([generate_exception/1,demo1/0,demo2/0]).

generate_exception(1) -> a;           % 正常
generate_exception(2) -> {'EXIT', a}; % 正常
generate_exception(3) ->
  throw(a); % 抛出一个异常, 没有让进程崩溃的意思，只是为了改变控制流(非局部返回)，并期望调用方去处理异常
generate_exception(4) ->
  exit(a); % 终止当前进程时使用，如果未捕捉，则会广播　{'EXIT', Pid, Why} 到其他链接到本进程的其他进程，不会返回Stack
generate_exception(5) ->
  error(a). % 崩溃性错误,会结束当前进程，会返回 Stack

% throw exit error 都可以被捕获和处理
catcher(N) ->
  % N 是入参 Ret 是表达式的返回值
  try % 中间可以写多个表达式 , 号隔开
    generate_exception(N)
  of
    Ret -> {N, normal, Ret} % 如果表达式执行正常，未抛出异常
  catch
    throw:Ret -> {N, caught, thrown, Ret}; % 处理 throw 异常
    exit:Ret -> {N, caught, exited, Ret};  % 处理 exit 异常
    error:Ret -> {N, caught, error, Ret}   % 处理 error 异常
  after % 一定会执行的子句，不返回任何值，常用来关闭打开的文件等操作
    {N, always, exec }
  end.

demo1() ->
  [catcher(I) || I <- [1,2,3,4,5]].

% 使用 catch 直接将任何异常转换为 {"EXIT", ...} 元组
demo2() ->
  [{I, catch generate_exception(I) } || I <- [1,2,3,4,5]].

%%[{file,"error.erl"},{line,34}]},
%%{error,'-demo2/0-lc$^0/1-0-',1,
%%[{file,"error.erl"},{line,34}]},
%%{erl_eval,do_apply,6,[{file,"erl_eval.erl"},{line,684}]},
%%{shell,exprs,7,[{file,"shell.erl"},{line,686}]},
%%{shell,eval_exprs,7,[{file,"shell.erl"},{line,642}]},
%%{shell,eval_loop,3,[{file,"shell.erl"},{line,627}]}]}}}]

