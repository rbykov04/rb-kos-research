.PHONY: distclean clean

run: out.comb
	./MicroHs/bin/mhseval

out.comb: Hello.hs
	MicroHs/bin/gmhs -iMicroHs/lib Hello

test: MicroHs
	cd MicroHs && make runtest

MicroHs:
	git clone https://github.com/augustss/MicroHs

clean:
	rm -rf out.comb

distclean:
	rm -rf MicroHs
