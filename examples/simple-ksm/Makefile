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

run: kos-qemu-image
	${SDK}/toolchain/bin/qemu-system-aarch64 ${QEMU_FLAGS} -kernel kos-qemu-image

Hello: hello.c
	${CC} hello.c -o Hello

INIT_FLAGS = --target=aarch64-kos -static -no-pie
Init: init.c
	${CC} ${INIT_FLAGS} init.c -o Init


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

ksm.module: security.psl.c
	${CC} ${SECURITY_MODULE_FLAGS} security.psl.c -o ksm.module

kos-qemu-image: Hello Init ksm.module
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
    Hello \
    ksm.module

clean:
	rm -f kos-qemu-image*
	rm -f Init
	rm -f Hello
	rm -f ksm.module
