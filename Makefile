.PHONY: clean run hello/einit/kos-qemu-image hello/Init

BUILD=build

TARGET     ="aarch64-kos"
BOARD      ="RPI4_BCM2711"
SDK        ="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"
TOOLCHAIN  = "${SDK}/toolchain"
CC         = "${TOOLCHAIN}/bin/aarch64-kos-clang"
QEMU_FLAGS =-m 2048 -machine vexpress-a15,secure=on -cpu cortex-a72
QEMU_FLAGS+=--nographic -smp 4 -serial stdio -nographic
QEMU_FLAGS+=-monitor none -nic none


run: hello/build/kos-qemu-image
	cd hello/build && \
    ${SDK}/toolchain/bin/qemu-system-aarch64 ${QEMU_FLAGS} -kernel kos-qemu-image

hello/Hello: hello/hello.c
	${CC} hello/hello.c -o hello/Hello


C_FLAGS  = -fstack-protector-strong -Wl,-z,relro -Wl,-z,now
C_FLAGS += -O2 -mcpu=cortex-a72 -Wall -Wextra -Wconversion -Wsign-conversion -Wformat=2
C_FLAGS += -Wformat-security -Werror=format-security
C_FLAGS += -Werror=return-type -Werror=implicit-function-declaration
C_FLAGS += -Wno-error=deprecated-declarations -fvisibility=hidden
C_FLAGS += -fcommon
C_FLAGS += -fno-omit-frame-pointer

L_FLAGS  = -fstack-protector-strong -Wl,-z,relro -Wl,-z,now
L_FLAGS += -O2 -mcpu=cortex-a72 -Wl,-z,noexecstack -Wl,-z,now
L_FLAGS += -static -static-libgcc
L_FLAGS += -static -no-pie
L_FLAGS += -Wno-error=unused-command-line-argument -static

INIT_FLAGS = ${C_FLAGS} ${L_FLAGS}

hello/Init: hello/init.c
	${CC} --target=aarch64-kos ${INIT_FLAGS} hello/init.c  -o hello/Init


hello/build/kos-qemu-image: hello/build hello/Hello
	${SDK}/toolchain/bin/kos_make_kimg \
    --target=aarch64-kos \
    --with-extra-ldflags=-no-pie \
    --sys-root=${SDK}/sysroot-aarch64-kos \
    --with-toolchain=${SDK}/toolchain \
    --with-compiler=clang \
    --ldscript=${SDK}/libexec/aarch64-kos/kos-qemu.ld \
    --img-src=${SDK}/libexec/aarch64-kos/kos-qemu \
    --img-dst=hello/build/kos-qemu-image \
    --max-filesize= \
    --with-init=hello/Init \
    hello/Hello \
    hello/build/EinitQemu-kss/ksm.module



hello/build:
	cd hello && \
	export PATH="${SDK}/toolchain/bin:${PATH}" && \
	cmake -B "${BUILD}" \
		-D BOARD="${BOARD}" \
		-D CMAKE_TOOLCHAIN_FILE="${SDK}/toolchain/share/toolchain-${TARGET}-clang.cmake"
	cd hello && cmake --build "${BUILD}" --target kos-qemu-image

clean:
	rm -rf hello/build
