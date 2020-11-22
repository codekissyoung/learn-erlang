-module(afile_server).
-author("cky").
-export([start/1, loop/1]).

start(Dir) ->
  spawn(afile_server, loop, [Dir]).

% 无限循环
loop(Dir) ->

  receive % 等待指令

    % 收到 {Client, list_dir} 信息, 就回复一个文件列表
    % Client 在运行时,实际是发消息的客户进程 Pid
    % Self() 在运行时,实际是当前进程的 Pid
    {Client, list_dir} ->
      Client ! {self(), file:list_dir(Dir)};

    % 收到 {Client, {get_file, File}}, 就返回这个文件
    {Client, {get_file, File}} ->
      Full = filename:join(Dir, File),
      Client ! {self(), file:read_file(Full)}

  end,

loop(Dir).

%% erl
%% 编译
%%1> c(afile_server).
%%{ok,afile_server}
%% 启动进程，FileServer 绑定是start返回的运行进程的 Pid
%% 有该进程的 Pid, 就可以向该进程发 Message
%%2> FileServer = afile_server:start(".").
%%<0.85.0>
%% 本 Erlang Shell 向 FileServer 发消息
%%3> FileServer ! {self(),list_dir}.
%%{<0.78.0>,list_dir}
%% 本 Erlang Shell 接收所有消息，绑定到 X.
%%4> receive X -> X end.
%%{<0.85.0>,
%%{ok,["run.sh","afile_client.erl","hello.erl",
%%"afile_server.erl","afile_server.beam"]}}

