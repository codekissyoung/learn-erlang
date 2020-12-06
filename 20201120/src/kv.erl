-module(kv).
-compile(export_all).
-behavior(gen_server).

start() -> gen_server:start_link({local, kv}, kv, arg1, []).
stop() -> gen_server:cast(kt,stop).
terminate(Reason, Dict) -> io:format("Key Value Server terminating ~n").

init(arg1) ->
  io:format("Key Value server starting ~n "),
  {ok, dict:new()}.

store(Key, Val) -> gen_server:call(kv, {store, Key, Val}).
lookup(Key) -> gen_server:call(kv, {lookup, Key}).

handle_call({store, Key, Val}, From, Dict) ->
  Dict1 = dict:store(Key, Val, Dict),
  {reply, ack, Dict1};
handle_call({lookup, crash}, From, Dict) ->
  1/0;
handle_call({lookup, Key}, From, Dict) ->
  {reply, dict:find(Key, Dict), Dict}.

handle_cast(stop, Dict) -> {stop, normal, Dict}.

