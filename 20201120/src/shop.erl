-module(shop).
-author("link").
-export([cost/1,total/1,sum/1,map/2,better_total/1,list_compre_total/1]).

cost(oranges) -> 5;
cost(newspaper) -> 8;
cost(apples) -> 2;
cost(pears) -> 9;
cost(milk) -> 7.

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