-module(my_behavior).
-author("cky").

% 下面是自定义行为
% 使用 -behavior(my_behavior) 引入行为
% 要求实现这些函数，否则编译器会给出警告
-export([behavior_info/1]).
behavior_info(callbacks) -> [{init,1}, {some_fun,0}, {other,3}];
behavior_info(_) -> undefined.