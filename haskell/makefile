HC=ghc

all: repl loader

rmobj:
	rm -f bin/*.o
	rm -f bin/*.hi

mkbin:
	mkdir -p bin

repl:
	$(MAKE) rmobj
	$(MAKE) mkbin
	$(HC) -isrc -outputdir bin -o bin/repl src/Repl

loader:
	$(MAKE) rmobj
	$(MAKE) mkbin
	$(HC) -isrc -outputdir bin -o bin/loader src/Loader

clean:
	rm -rf bin
