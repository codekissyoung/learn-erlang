-module(shop).
-author("link").
-compile(export_all).

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



