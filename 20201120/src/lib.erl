-module(lib).
-export([for/3, qsort/1, demo/0, pythag/1, perms/1, odds_and_evens/1, max/2, filter/2,count_characters/1]).

for(Max, Max, F) -> % 这句一定要在前面，是终止条件
  [ F(Max) ];
for(I, Max, F) ->
  [ F(I) | for(I+1, Max, F) ].

% 快速排序
qsort([]) -> [];
qsort([Piv | T]) ->
  qsort([ X || X <-T, X < Piv ])
  ++ [Piv] ++
  qsort([ X || X <-T, X >= Piv ]).

% 毕达哥拉斯三元数组　即符合 A*A + B*B = C*C 并且 A + B + C =< N 的 {A, B, C}
pythag(N) ->
  [
    {A,B,C} ||
    A <- lists:seq(1, N),
    B <- lists:seq(1, N),
    C <- lists:seq(1, N),
    A + B + C =< N,
    A*A + B*B =:= C*C
  ].

% 排列组合
% L 里依次将元素取出来作为队头 H, 其余元素全排列后, 跟在后面
perms([]) ->
  [[]];
perms(L) ->
  [
    [H | T] ||
    H <- L,
    T <- perms(L -- [H]) % L -- [H] 从 L 中移除 H
  ].

% 增强函数匹配，而设计的 关卡 功能，通过 when 在函数开头调用 光卡
max(X,Y) when X > Y
  -> X;
max(X,Y)
  -> Y.

% case 表达式实现 filter
filter(F, [H|T] ) ->
  case F(H) of
    true -> [H|filter(F,T)];
    false -> filter(F,T)
  end;
filter(F,[]) -> [].

% 归集器
odds_and_evens(L) ->
  odds_and_evens_acc(L, [], []).

odds_and_evens_acc( [H|T], Odds, Evens ) ->
  case (H rem 2) of
    1 -> odds_and_evens_acc(T, [H|Odds], Evens); % 注意，由于插入队头，元素顺序是相反的
    0 -> odds_and_evens_acc(T, Odds, [H|Evens])
  end;
odds_and_evens_acc([], Odds, Evens) ->
  {lists:reverse(Odds), lists:reverse(Evens)}.

% 返回一个 Map , 内含某个字符串里各个字符的出现次数
count_characters(Str) ->
  count_characters(Str, #{}).
count_characters([H|T], #{ H => N} = X ) ->
  count_characters(T, X#{H := N+1 });
count_characters([H|T], X) ->
  count_characters(T, X#{H=>1});
count_characters([], X) -> X.

demo() ->
  qsort([23,12,34,1,234,45,89,12]).

%% 20> c("lib.erl").
%% {ok,lib}
%% 21> lib:for(1, 10, fun(X) -> X end).
%% [1,2,3,4,5,6,7,8,9,10]
%% 22> lib:for(1, 10, fun(X) -> X * 2 end).
%% [2,4,6,8,10,12,14,16,18,20]
%%　9> c("lib.erl").
%%　{ok,lib}
%%　10> lib:demo().
%%　[1,12,12,23,34,45,89,234]
%% 13> lib:pythag(50).
%% [{3,4,5},
%% {4,3,5},
%% {5,12,13},
%% {6,8,10},
%% {8,6,10},
%% {8,15,17},
%% {9,12,15},
%% {12,5,13},
%% {12,9,15},
%% {12,16,20},
%% {15,8,17},
%% {16,12,20}]
