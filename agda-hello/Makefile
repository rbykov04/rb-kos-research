.PHONY: run distclean

run: Hello
	./Hello

MHSDIR=MicroHs
BIN=MicroHs/bin
LIB=MicroHs/lib
HUGS=MicroHs/hugs
TOOLS=MicroHs/Tools
RUNTIME=MicroHs/src/runtime



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

Hello.comb: Hello.hs MicroHs
	MHSDIR=$(MHSDIR) $(BIN)/gmhs -i$(LIB) Hello -oHello.comb

Addcombs: MicroHs
	MHSDIR=$(MHSDIR) $(BIN)/gmhs -i$(TOOLS) Addcombs -oAddcombs



MicroHs:
	git clone https://github.com/augustss/MicroHs
	cd MicroHs && make bin/mhseval
	cd MicroHs && make bin/gmhs


Hello: MAlonzo
	cp RTE.hs MAlonzo/RTE.hs
	MHSDIR=$(MHSDIR) $(BIN)/gmhs \
		MAlonzo.Code.AgdaHello \
		-i$(LIB)  \
		-oHello




HelloGhc: MAlonzo
	ghc -O -o Hello \
		-Werror -i. \
		-main-is MAlonzo.Code.AgdaHello \
		MAlonzo/Code/AgdaHello.hs \
		--make -fwarn-incomplete-patterns





MAlonzo: AgdaHello.agda
	agda  -i. --ghc-dont-call-ghc --compile --library=standard-library-2.2 AgdaHello.agda

clean:
	rm -rf _build
	rm -rf MAlonzo
	rm -f Hello
	rm -f HelloGhc
	rm -f HelloHost
	rm -f Addcombs

distclean:
	rm -rf MicroHs
