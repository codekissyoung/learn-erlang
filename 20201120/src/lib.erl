-module(lib).
-export([for/3]).

for(Max, Max, F) -> % 这句一定要在前面，是终止条件
  [ F(Max) ];
for(I, Max, F) ->
  [ F(I) | for(I+1, Max, F) ].

%% 20> c("lib.erl").
%% {ok,lib}
%% 21> lib:for(1, 10, fun(X) -> X end).
%% [1,2,3,4,5,6,7,8,9,10]
%% 22> lib:for(1, 10, fun(X) -> X * 2 end).
%% [2,4,6,8,10,12,14,16,18,20]
