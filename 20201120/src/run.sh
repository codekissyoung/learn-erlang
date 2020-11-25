#!/usr/bin/env bash

# 编译 hello.erl
erlc -d hello.erl

# 加载 hello 模块，执行 hello:start() 然后执行 init:stop()
erl -noshell -s hello start -s init stop

#/bin/rm *.beam

#erlc afile_server.erl
#erl -noshell -s afile_server start .
