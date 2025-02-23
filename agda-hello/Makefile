.PHONY: run distclean

run: Hello
	./Hello



Hello: MAlonzo
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

distclean:
	rm -rf agda-stdlib
