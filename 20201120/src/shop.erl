-module(shop).
-author("link").
-export([cost/1,beach/1,cost2/1,total/1,sum/1,map/2,better_total/1,list_compre_total/1]).

cost(oranges) -> 5;
cost(newspaper) -> 8;
cost(apples) -> 2;
cost(pears) -> 9;
cost(milk) -> 7.



% 使用 if 需要将所有情况列举清楚，不存在 else 分支
bigger(X, Y) ->
  if
    X > Y -> bigger ;
    X < Y -> smaller;
    X == Y -> equal
  end.

% 使用 if 改写 cost
%%2> shop:cost2(orange).
%%5
%%3> shop:cost2(milk).
%%7
%%4> shop:cost2(aaaa).
%%unkown
%%5>
cost2(X) ->
  if
    X == orange -> 5;
    X == newspaper -> 8;
    X == apples -> 2;
    X == pears -> 9;
    X == milk -> 7;
    true -> unkown
  end.

% case 表达式 根据温度决定去不去海滩玩耍
beach(Temprature) ->
  case Temprature of
    {celsius, N } when N >= 20, N =< 45 -> 'favorable';
    {kelvin, N} when N >= 293, N =< 318 -> 'favorable';
    _ -> 'avoid beach'
  end.

total([]) -> 0;
total([{Good,Num}|T])
  -> cost(Good) * Num + total(T).

better_total(L) ->
  sum( map(
    fun({Good, Num})
      -> cost(Good) * Num
    end, L ) ).

list_compre_total(L) ->
  sum( [ cost(Good) * Num || {Good, Num} <- L ] ).

sum([]) -> 0;
sum([H|T])
  -> H + sum(T).

map(_, []) -> [];
map(F, [H|T]) -> [F(H) | map(F, T)].



