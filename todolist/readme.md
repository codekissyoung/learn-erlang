# 一个 Erlang 例子

```bash
$ erl -make         # 编译
$ erl -pa ebin/     # 执行
```

在 shell 里面重新编译加载

```erlang
> make:all([load]).
```

执行：

```erlang
1> evserv:start().
<0.83.0>
2> evserv:subscribe(self()).
{ok,#Ref<0.319105841.512753670.160727>}
3>  evserv:add_event("aa","cccc",5).
"add event ok"
[{#Ref<0.319105841.512753670.160728>,<0.81.0>}][{#Ref<0.319105841.512753670.160728>,<0.81.0>}]4> 
4> flush().
Shell got {done,"aa","cccc"}
```
