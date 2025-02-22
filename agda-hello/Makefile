.PHONY: run distclean

run: agda-hello
	./agda-hello


agda-hello: agda-hello.agda
	agda  -i.  --compile --library=standard-library-2.2 agda-hello.agda

clean:
	rm -rf _build
	rm -rf MAlonzo
	rm -f agda-hello

distclean:
	rm -rf agda-stdlib
