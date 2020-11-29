.SUFFIXES: .erl .beam .yrl # 声明涉及到的文件类型

all : $(patsubst %.erl, %.beam, $(wildcard *.erl))

%.beam : %.erl
	erlc -d -W $<

.PHONY : clean
clean:
	$(RM) *.beam erl_crash.dump