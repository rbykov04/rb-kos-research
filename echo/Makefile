.PHONY: clean run

BUILD=build
TARGET     ="aarch64-kos"
BOARD      ="RPI4_BCM2711"
SDK        ="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"
TOOLCHAIN  = "${SDK}/toolchain"
CC         = "${TOOLCHAIN}/bin/aarch64-kos-clang"

QEMU_FLAGS    =-m 2048 -machine vexpress-a15,secure=on -cpu cortex-a72
QEMU_FLAGS    +=--nographic -smp 4 -serial stdio -nographic
QEMU_FLAGS    +=-monitor none -nic none

run: build/kos-qemu-image
	${SDK}/toolchain/bin/qemu-system-aarch64 ${QEMU_FLAGS} -kernel build/kos-qemu-image

build:
	mkdir build

build/Server: build server.c build/Server.edl.h
	${CC} server.c -o build/Server

build/Client: build client.c
	${CC} client.c -o build/Client

ROOT_DIR            :=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
#NK_GEN_FLAGS        = -P${SDK}/sysroot-aarch64-kos/include/system.platform
NK_GEN_FLAGS       += -I${SDK}/sysroot-aarch64-kos/include
NK_GEN_FLAGS       += -I${SDK}/toolchain/include
NK_GEN_FLAGS       += -I${ROOT_DIR}



build/Server.edl.h: Server.edl Echo.idl
	${SDK}/toolchain/bin/nk-gen-c \
		Server.edl \
		${NK_GEN_FLAGS} \
	-o build

INIT_FLAGS = --target=aarch64-kos -static -no-pie
build/Init: build init.c
	${CC} ${INIT_FLAGS} init.c -o build/Init

ROOT_DIR            :=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
PSL_GEN_FLAGS        = -P${SDK}/sysroot-aarch64-kos/include/system.platform
PSL_GEN_FLAGS       += -I${SDK}/sysroot-aarch64-kos/include
PSL_GEN_FLAGS       += -I${SDK}/toolchain/include
PSL_GEN_FLAGS       += -I${ROOT_DIR}

build/security.psl.c: build security.psl
	${SDK}/toolchain/bin/nk-psl-gen-c \
		security.psl \
		${PSL_GEN_FLAGS} \
		-o build/security.psl.c

SECURITY_MODULE_FLAGS = -fno-pic \
						-D__KOS_KERNEL__ \
						-DQCBOR_DISABLE_FLOAT_HW_USE \
						-DQCBOR_DISABLE_INDEFINITE_LENGTH_ARRAYS \
						-DQCBOR_DISABLE_INDEFINITE_LENGTH_STRINGS \
						-DQCBOR_DISABLE_UNCOMMON_TAGS \
						-DQCBOR_DISABLE_EXP_AND_MANTISSA \
						-DQCBOR_DISABLE_PREFERRED_FLOAT \
						-DUSEFULBUF_DISABLE_ALL_FLOAT \
						-DUSEFULBUF_DISABLE_DEPRECATED \
						-Wall \
						-mcpu=cortex-a57 \
						-ffixed-x18 \
						-fno-omit-frame-pointer \
						-mno-omit-leaf-frame-pointer \
						-O2 \
						-Wextra \
						-Wpointer-arith \
						-ffreestanding \
						-mgeneral-regs-only \
						-fno-pie \
						-no-pie \
						-Wframe-larger-than=12288 \
						-fstack-protector-all \
						-g0 \
						-nostdlib \
						-z separate-code \
						-z max-page-size=4096 \
						-lksm_kss-qemu \
						-lrtl-kernel-qemu  \
						-Wl,--entry=KsmKssEntry \
						-Wl,--image-base=0xFFFF808000000000 \
						-fuse-ld=lld \
						-L/${SDK}/toolchain/lib/clang/17/lib/kos \
						-lclang_rt.builtins-aarch64

build/ksm.module: build/security.psl.c
	${CC} ${SECURITY_MODULE_FLAGS} build/security.psl.c -o build/ksm.module

build/kos-qemu-image: build/Client build/Server build/Init build/ksm.module
	cd build && \
	${SDK}/toolchain/bin/kos_make_kimg \
	--target=aarch64-kos \
	--with-extra-ldflags=-no-pie \
	--sys-root=${SDK}/sysroot-aarch64-kos \
    --with-toolchain=${SDK}/toolchain \
    --with-compiler=clang \
    --ldscript=${SDK}/libexec/aarch64-kos/kos-qemu.ld \
    --img-src=${SDK}/libexec/aarch64-kos/kos-qemu \
    --img-dst=kos-qemu-image \
    --max-filesize= \
    --with-init=Init \
    Client \
    Server \
    ksm.module

clean:
	rm -rf build
