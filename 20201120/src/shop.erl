-module(shop).
-author("link").
-export([cost/1,total/1]).

cost(oranges) -> 5;
cost(newspaper) -> 8;
cost(apples) -> 2;
cost(pears) -> 9;
cost(milk) -> 7.

total([{Good,Num}|T]) -> cost(Good) * Num + total(T);
total([]) -> 0.





