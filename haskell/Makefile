HC=ghc
HCF=-Werror -Wall -isrc -outputdir bin -o

all: repl loader

rmobj:
	find -name "*.o" | xargs rm
	find -name "*.hi" | xargs rm

mkbin:
	mkdir -p bin

repl:
	$(MAKE) rmobj
	$(MAKE) mkbin
	$(HC) $(HCF) bin/repl src/Tools/Repl.hs

loader:
	$(MAKE) rmobj
	$(MAKE) mkbin
	$(HC) $(HCF) bin/loader src/Tools/Loader.hs

clean:
	rm -rf bin
