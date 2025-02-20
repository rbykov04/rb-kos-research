.PHONY: clean run

BUILD=build

TARGET="aarch64-kos"
BOARD="RPI4_BCM2711"
SDK_PREFIX="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"

run: hello/build
	cd hello && \
	cmake --build "${BUILD}" --target sim



hello/build:
	cd hello && \
	export PATH="${SDK_PREFIX}/toolchain/bin:${PATH}" && \
	cmake -B "${BUILD}" \
		-D BOARD="${BOARD}" \
		-D CMAKE_TOOLCHAIN_FILE="${SDK_PREFIX}/toolchain/share/toolchain-${TARGET}-clang.cmake"
	cd hello && cmake --build "${BUILD}" --target kos-qemu-image

clean:
	rm -rf hello/build
