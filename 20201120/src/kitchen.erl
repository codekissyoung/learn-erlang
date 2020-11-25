-module(kitchen).
-author("cky").
-export([start/1,fridge/1,store/2,take/2]).

start(FoodList) ->
  spawn(?MODULE, fridge, [FoodList]).

% process with status
fridge(FoodList) ->
  receive
    {From, {store, Food}} ->
      From ! {self(), ok},
      fridge([Food|FoodList]);
    {From, {take, Food}} ->
      case lists:member(Food, FoodList) of
        true ->
          From ! {self(), {ok, Food}},
          fridge(lists:delete(Food, FoodList));
        false ->
          From ! {self(), {ok, not_found}},
          fridge(FoodList)
      end;
    terminate -> ok
  end.

% 提供给外界调用，隐藏发送消息的细节
store(Pid, Food) ->
  Pid ! {self(), {store, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 -> % 等待超时
    timeout
  end.

take(Pid, Food) ->
  Pid ! {self(), {take, Food}},
  receive
    {Pid, Msg} -> Msg
  after 3000 -> % 等待超时
    timeout
  end.

