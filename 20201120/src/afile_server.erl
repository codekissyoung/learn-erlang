-module(afile_server).
-author("cky").
-export([start/1,loop/1]).

start(Dir) ->
  spawn(afile_server, loop, [Dir]).

% 无限循环
loop(Dir) ->
  receive % 等待指令

    {Client,list_dir} ->
      Client ! {self(), file:list_dir(Dir)};

    {Client, {get_file,File}} ->
      Full = filename:join(Dir, File),
      Client ! {self(), file:read_file(Full)}

  end,

loop(Dir).
