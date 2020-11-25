all : $(patsubst %.erl, %.beam, $(wildcard *.erl))

%.beam : %.erl
	erlc -d $<

.PHONY : clean
clean:
	$(RM) *.beam