.PHONY: distclean clean run

MHSDIR=MicroHs
BIN=MicroHs/bin
LIB=MicroHs/lib
TOOLS=MicroHs/Tools
RUNTIME=MicroHs/src/runtime


run: Hello
	./Hello

Hello: Hello.c
	clang \
		ffi.c \
		Hello.c \
		eval-kos-64.c \
		-static \
		-o Hello



HelloHost: Hello.c
	clang \
		ffi.c \
		Hello.c \
		${RUNTIME}/eval-unix-64.c \
		-lm\
		-static \
		-o HelloHost


Hello.c: Hello-opt.comb Addcombs
	./Addcombs Hello-opt.comb Hello.c

Hello-opt.comb: Hello.comb
	$(BIN)/mhseval +RTS -rHello.comb -oHello-opt.comb

Hello.comb: Hello.hs
	MHSDIR=$(MHSDIR) $(BIN)/gmhs -i$(LIB) Hello -oHello.comb


Addcombs:
	MHSDIR=$(MHSDIR) $(BIN)/gmhs -i$(TOOLS) Addcombs -oAddcombs


out.comb: Hello.hs
	MicroHs/bin/gmhs -iMicroHs/lib Hello

test: MicroHs
	cd MicroHs && make runtest

MicroHs:
	git clone https://github.com/augustss/MicroHs

clean:
	rm -rf out.comb
	rm -rf Hello.c
	rm -rf Hello
	rm -rf Hello.comb
	rm -rf Hello-opt.comb

distclean:
	rm -rf MicroHs
