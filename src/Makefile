# definition ghc compiler
HXX := ghc-9.6.1
# basic flags to compile our program
HFLAGS := -Werror -Wall -Wmissing-local-signatures -o lab2

run: Tests.hs
	$(HXX) $(HFLAGS) Tests.hs
	./lab2

clean:
	rm -f *.hi *.o lab2