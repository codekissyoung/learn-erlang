% records.hrl
-record(todo, {status=reminder,who=joe,text}). % record

% 在函数 records 的模式匹配
clear_status(#todo{status = S, who = W} = R) ->
  R#todo{text="Finished",status=finished}.

% 匹配某个类型的 records
do_something(X) when is_record(X, todo) ->
  X#todo{status = finished}.
