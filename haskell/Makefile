HC=ghc
HCF=-Werror -Wall -isrc -outputdir bin -o

all: repl loader

rmobj:
	rm -f bin/*.o
	rm -f bin/*.hi

mkbin:
	mkdir -p bin

repl:
	$(MAKE) rmobj
	$(MAKE) mkbin
	$(HC) $(HCF) bin/repl src/Repl.hs

loader:
	$(MAKE) rmobj
	$(MAKE) mkbin
	$(HC) $(HCF) bin/loader src/Loader.hs

clean:
	rm -rf bin
