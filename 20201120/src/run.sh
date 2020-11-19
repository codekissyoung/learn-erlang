#!/usr/bin/env bash


# 编译 hello.erl 成 hello.beam
erlc hello.erl

# 加载 hello 模块，执行 hello:start() 然后执行 init:stop()
erl -noshell -s hello start -s init stop