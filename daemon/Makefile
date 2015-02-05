all:
	ghc --make -Wall -Werror Main -o ardaemon


.PHONY: test clean

test:
	nc localhost 4040 < test/json_commands.txt

clean:
	rm *hi *o ardaemon
