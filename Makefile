.PHONY: clean run

run:
	cd hello && ./cross-build.sh

clean:
	rm -rf hello/build
