.PHONY: clean run

BUILD=build

TARGET="aarch64-kos"
BOARD="RPI4_BCM2711"
SDK="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"
QEMU_FLAGS =-m 2048 -machine vexpress-a15,secure=on -cpu cortex-a72
QEMU_FLAGS+=--nographic -smp 4 -serial stdio -nographic
QEMU_FLAGS+=-monitor none -nic none



run: hello/build
	cd hello/build && \
    ${SDK}/toolchain/bin/qemu-system-aarch64 ${QEMU_FLAGS} -kernel kos-qemu-image

hello/build:
	cd hello && \
	export PATH="${SDK}/toolchain/bin:${PATH}" && \
	cmake -B "${BUILD}" \
		-D BOARD="${BOARD}" \
		-D CMAKE_TOOLCHAIN_FILE="${SDK}/toolchain/share/toolchain-${TARGET}-clang.cmake"
	cd hello && cmake --build "${BUILD}" --target kos-qemu-image

clean:
	rm -rf hello/build
