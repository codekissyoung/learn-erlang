# 声明涉及到的文件类型
.SUFFIXES: .erl .beam .yrl

all : $(patsubst %.erl, %.beam, $(wildcard *.erl))

%.beam : %.erl
	erlc $<
#	erlc -d $<

.PHONY : clean
clean:
	$(RM) *.beam erl_crash.dump