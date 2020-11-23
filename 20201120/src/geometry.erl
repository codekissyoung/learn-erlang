-module(geometry).
-author("link").
-export([area/1,test/0]).

% 传一个元组进来，匹配上了，就执行计算面积的代码
area({rectangle,Width,Height}) -> Width * Height;
area({circle,Radius})-> 3.14159 * Radius * Radius;
area({square,Side}) -> Side * Side.

test() ->
  12 = area({rectangle,3,4}),
  _ = area({circle,10}),
  144 = area({square,12}),
  tests_worked.

%%  2> c(geometry).
%%  {ok,geometry}
%%  3> geometry:area({rectangle,10,2}).
%%  20
%%  4> geometry:area({square,4}).
%%  16
%%  5> geometry:area({aaaa,4,34}).
%%** exception error: no function clause matching geometry:area({aaaa,4,34}) (geometry.erl, line 6)
%%  8> geometry:test().
%%  tests_worked

