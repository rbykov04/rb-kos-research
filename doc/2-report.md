# 1 Report Example Hello with Makefile

## Start
In our system we will have:
1. Hello process to make smt usefull (print hello world :D).
1. Init process to start hello process.
1. Kernel
1. Security model - ksm.module (Kaspersky Security Module - KSM)


Look at tree:
```
 ~/dev/github/rb-kos-research/hello   main ●✚  tree
├── Einit.edl
├── hello.c 
├── Hello.edl
├── init.c
├── Makefile
└── security.psl
```

## User processes

File hello.c is also simple. It is one process. We just print message to stderr.

```
 ~/dev/github/rb-kos-research/hello   main ●✚  cat hello.c 
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    fprintf(stderr,"Hello world from Kaspersky OS!\n");
    return EXIT_SUCCESS;
}
```


To start hello process we need init process. I copyed that from generated file.
But it also pretty simple.

```
 ~/dev/github/rb-kos-research/hello   main ●✚  cat init.c 
#include <stdlib.h>
#include <stdio.h>

#include <rtl/retcode.h>
#include <coresrv/entity/entity_api.h>
#include <services/handle/if_connection.h>

static const EntityInfo taskInfo_hello_Hello_0 = {
    .eiid = "Hello",
    .max_endpoints = 0,
    .endpoints = NK_NULL,
};

int main(void) {
    Entity * task_hello_Hello_0;

    const char * taskInfo_hello_Hello_0_args[] = {
        "Hello",
        RTL_NULL
    };

    task_hello_Hello_0 = EntityInitEx(
        &taskInfo_hello_Hello_0, "Hello", "Hello"
    );
    if (!task_hello_Hello_0) {
        fprintf(stderr, "Can't initialize task \"Hello\"\n");
        return EXIT_FAILURE;
    }

    if (EntitySetArgs(task_hello_Hello_0, taskInfo_hello_Hello_0_args) != rcOk) {
        fprintf(stderr, "Can't set args for task \"Hello\"\n");
        return EXIT_FAILURE;
    }

    if (EntityRun(task_hello_Hello_0) != rcOk) {
        fprintf(stderr, "Can't run task \"Hello\"\n");
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}
```

## Kaspersky OS specific


### Security policy
PSL - Policy Security Language

Look at security policy our system:

```
 ~/dev/github/rb-kos-research/hello   main ●✚  cat security.psl
use nk.base._

task class Einit        : Einit
task class kl.core.Core : kl.core.Core
task class Hello        : Hello

//Threat model - there are not threats

execute  { grant () }
request  { grant () }
response { grant () }
security { grant () }
error    { grant () }
```

This security file contains a few logical sections:

```

// import psl modules
use nk.base._ 

// Static declaration of system
// nk-psl-gen-c (compiler) will search EDL files for every task class
// EDL (Entity Declaration Language) File
// It is a declaration of "Entity" in our case Entity == Process

task class Einit        : Einit
task class kl.core.Core : kl.core.Core
task class Hello        : Hello

// Section with rules.
// In our small system: There is one rule - all communication are grant.

//Threat model - there are not threats
execute  { grant () }
request  { grant () }
response { grant () }
security { grant () }
error    { grant () }
```



Look also at the EDL file.
Our user proccess will not communicate with anyone.
And EDL files contain only names.

```
 ~/dev/github/rb-kos-research/hello   main ●✚  cat Einit.edl 
entity Einit

 ~/dev/github/rb-kos-research/hello   main ●✚  cat Hello.edl 
task class Hello
```

And that is it.

## How to build

Look at the Makefile

I will give this file first without comments:
```
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

ROOT_DIR            :=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
PSL_GEN_FLAGS        = -P${SDK}/sysroot-aarch64-kos/include/system.platform
PSL_GEN_FLAGS       += -I${SDK}/sysroot-aarch64-kos/include
PSL_GEN_FLAGS       += -I${SDK}/toolchain/include
PSL_GEN_FLAGS       += -I${ROOT_DIR}

security.psl.c: security.psl
	${SDK}/toolchain/bin/nk-psl-gen-c \
		security.psl \
		${PSL_GEN_FLAGS} \
		-o security.psl.c

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
	rm -f security.psl.c
```


Let's leave only declarations of targets

```
.PHONY: clean run

run: kos-qemu-image

Hello: hello.c

Init: init.c

security.psl.c: security.psl

ksm.module: security.psl.c

kos-qemu-image: Hello Init ksm.module

clean:
	rm -f kos-qemu-image*
	rm -f Init
	rm -f Hello
	rm -f ksm.module
	rm -f security.psl.c
```

You see - pretty easy. 

Let's comment them a little bit

There are just two "PHONY (Phony means target wich doest produce real file)" target

```
.PHONY: clean run
```

First we will look at run.

```
SDK        ="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"

QEMU_FLAGS    =-m 2048 -machine vexpress-a15,secure=on -cpu cortex-a72
QEMU_FLAGS    +=--nographic -smp 4 -serial stdio -nographic
QEMU_FLAGS    +=-monitor none -nic none

run: kos-qemu-image
	${SDK}/toolchain/bin/qemu-system-aarch64 ${QEMU_FLAGS} -kernel kos-qemu-image
```

We run system in qemu (from SDK).
I don't really understand all flags and will not comment them. 

For run our system we need image "kos-qemu-image".
Let's see how to build that.



```
SDK        ="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"

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
```

To build we need binaries for user process:
- Hello
- Init

Also we need ksm.module.

And wee need some magic (script "kos_make_kimg") - don't ask me what there.

But look at this:
```
kos-qemu-image: Hello Init ksm.module
	${SDK}/toolchain/bin/kos_make_kimg \
    ....
    --with-init=Init \   ---------- Start Process
    Hello \              ---------- Usefull Procces
    ksm.module           ---------- Security Module
```

This we can build.

```
SDK        ="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"
TOOLCHAIN  = "${SDK}/toolchain"
CC         = "${TOOLCHAIN}/bin/aarch64-kos-clang"

Hello: hello.c
	${CC} hello.c -o Hello

INIT_FLAGS = --target=aarch64-kos -static -no-pie
Init: init.c
	${CC} ${INIT_FLAGS} init.c -o Init

```

Import to make Init workable we need to build this binary with 
- -static - make binary this static 
- -no-pie - disable position inpendent code (without this kernel will not run proccess)

Interesting fact: we use clang.

Ok. We need security.module. Let's build one.

```

SDK        ="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"
TOOLCHAIN  = "${SDK}/toolchain"
CC         = "${TOOLCHAIN}/bin/aarch64-kos-clang"

ksm.module: security.psl.c
	${CC} ${SECURITY_MODULE_FLAGS} security.psl.c -o ksm.module

```

Pretty simple. Doesn't it?

Wait a minute but what ${SECURITY_MODULE_FLAGS} and security.psl.c?

Ok ok ok. 

```
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
```

I have no idea what they all means.

I explain a few of them.

- "-D__KOS_KERNEL__" - We will build ksm.module for kernel mode. (Yes security module will work in kernel)
- "-DQCBOR*" - CBOR used in ksm.module and this list for that.
- "-Wall" - :)
- -fno-pie no-pie (Maybe dublication - but who care)
- -nostdlib -ffreestanding - I guess it to make to kernel mode.
- -Wl,--entry=KsmKssEntry -Wl,--image-base=0xFFFF808000000000  - it is from where kernel will run ksm
- -lksm_kss-qemu -lrtl-kernel-qemu  - libs with kernel api for ksm

+ some hardening featches + some clang magic


Ok. What is about security.psl.c

```
SDK        ="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"

ROOT_DIR            :=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
PSL_GEN_FLAGS        = -P${SDK}/sysroot-aarch64-kos/include/system.platform
PSL_GEN_FLAGS       += -I${SDK}/sysroot-aarch64-kos/include
PSL_GEN_FLAGS       += -I${SDK}/toolchain/include
PSL_GEN_FLAGS       += -I${ROOT_DIR}

security.psl.c: security.psl
	${SDK}/toolchain/bin/nk-psl-gen-c \
		security.psl \
		${PSL_GEN_FLAGS} \
		-o security.psl.c
```

To build security.psl.c we need special compiler from sdk and pass it paths for a few files.
Maybe in future a will aleborate this topic a little bit.

But not today.


And that is it.

Let's run system.

```
~/dev/github/rb-kos-research/hello   main ●✚  make
"""/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain"/bin/aarch64-kos-clang" hello.c -o Hello
"""/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain"/bin/aarch64-kos-clang" --target=aarch64-kos -static -no-pie init.c -o Init
make: Circular security.psl <- security.psl.c dependency dropped.
"/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain/bin/nk-psl-gen-c \
	security.psl \
	-P"/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/sysroot-aarch64-kos/include/system.platform -I"/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/sysroot-aarch64-kos/include -I"/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain/include -I/home/rbykov/dev/github/rb-kos-research/hello \
	-o security.psl.c
"""/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain"/bin/aarch64-kos-clang" -fno-pic -D__KOS_KERNEL__ -DQCBOR_DISABLE_FLOAT_HW_USE -DQCBOR_DISABLE_INDEFINITE_LENGTH_ARRAYS -DQCBOR_DISABLE_INDEFINITE_LENGTH_STRINGS -DQCBOR_DISABLE_UNCOMMON_TAGS -DQCBOR_DISABLE_EXP_AND_MANTISSA -DQCBOR_DISABLE_PREFERRED_FLOAT -DUSEFULBUF_DISABLE_ALL_FLOAT -DUSEFULBUF_DISABLE_DEPRECATED -Wall -mcpu=cortex-a57 -ffixed-x18 -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer -O2 -Wextra -Wpointer-arith -ffreestanding -mgeneral-regs-only -fno-pie -no-pie -Wframe-larger-than=12288 -fstack-protector-all -g0 -nostdlib -z separate-code -z max-page-size=4096 -lksm_kss-qemu -lrtl-kernel-qemu -Wl,--entry=KsmKssEntry -Wl,--image-base=0xFFFF808000000000 -fuse-ld=lld -L/"/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain/lib/clang/17/lib/kos -lclang_rt.builtins-aarch64 security.psl.c -o ksm.module
"/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain/bin/kos_make_kimg \
--target=aarch64-kos \
--with-extra-ldflags=-no-pie \
--sys-root="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/sysroot-aarch64-kos \
    --with-toolchain="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain \
    --with-compiler=clang \
    --ldscript="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/libexec/aarch64-kos/kos-qemu.ld \
    --img-src="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/libexec/aarch64-kos/kos-qemu \
    --img-dst=kos-qemu-image \
    --max-filesize= \
    --with-init=Init \
    Hello \
    ksm.module
[Script kos_make_kimg] Checking: linker (/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/bin/aarch64-kos-ld.bfd).. /opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/bin/aarch64-kos-ld.bfd
[Script kos_make_kimg] Checking: strip (/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/bin/aarch64-kos-strip).. /opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/bin/aarch64-kos-strip
[Script kos_make_kimg] Checking: objcopy (/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/bin/aarch64-kos-objcopy).. /opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/bin/aarch64-kos-objcopy
[Script kos_make_kimg] Checking: objdump (/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/bin/aarch64-kos-objdump).. /opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/bin/aarch64-kos-objdump
[Script kos_make_kimg] Checking: root directory.. /opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/sysroot-aarch64-kos
[Script kos_make_kimg] Checking entity: Hello... 
[Script kos_make_kimg] ok
[Script kos_make_kimg] Checking entity: ksm.module... 
[Script kos_make_kimg] ok
[File #0] einit, path:/home/rbykov/dev/github/rb-kos-research/hello/Init
[File #1] Hello, path:/home/rbykov/dev/github/rb-kos-research/hello/Hello
[File #2] ksm.module, path:/home/rbykov/dev/github/rb-kos-research/hello/ksm.module
[Script kos_make_kimg] Checking: romfs obj.. /tmp/tmp.YkrGuGiN6O
[Script kos_make_kimg] Transforming ROMFS image to object file: /tmp/tmp.YkrGuGiN6O ... /opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/bin/aarch64-kos-objcopy -S --rename-section .data=.romfs --input-target=binary --output-target=elf64-littleaarch64 kos-qemu-image.romfs /tmp/tmp.YkrGuGiN6O\n
[Script kos_make_kimg] ok
[Script kos_make_kimg] Linking image: kos-qemu-image ... /opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/bin/aarch64-kos-ld.bfd -nostdlib -no-pie -no-pie -z max-page-size=4096 -o kos-qemu-image -T /opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/libexec/aarch64-kos/kos-qemu.ld -Map=kos-qemu-image.map /opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/libexec/aarch64-kos/kos-qemu /tmp/tmp.YkrGuGiN6O -L/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/lib/clang/17/lib/kos -lclang_rt.builtins-aarch64\n
[Script kos_make_kimg] ok
[Script kos_make_kimg] Generating binary image...
[Script kos_make_kimg] Generating stripped image...
"/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain/bin/qemu-system-aarch64 -m 2048 -machine vexpress-a15,secure=on -cpu cortex-a72 --nographic -smp 4 -serial stdio -nographic -monitor none -nic none -kernel kos-qemu-image
[BOOT ] Starting...
[BOOT ] Checking FDT...
[BOOT ] Setting up multiprocessing...
[BOOT ] Built CPU Identity table using FDT:
[BOOT ] Hardware ID of CPU #0: 0x0
[BOOT ] Hardware ID of CPU #1: 0x1
[BOOT ] Hardware ID of CPU #2: 0x2
[BOOT ] Hardware ID of CPU #3: 0x3
[BOOT ] Hardware ID of CPU #4: 0x4
[BOOT ] Unexpected AP CPU count 0
[BOOT ] Spin exit won't be waited for
[BOOT ] Setting up architecture...
[BOOT ] Setting up interrupt controller...
[BOOT ] Setting up system timer...
[BOOT ] Preparing memory...
[BOOT ] Preparing loader info...
[BOOT ] Preparing FDT...
[BOOT ] Allocating dynamic reserved memory regions...
[BOOT ] Preparing ROMFS...
[BOOT ] DRNG poorly seeded
[BOOT ] Preparing KasperskyOS kernel...
[BOOT ] Preparing trampoline to kernel...
[BOOT ] Preparing page table...
[BOOT ] Preparing modules...
[BOOT ] Dump physical memory map...
[BOOT ] Bootstrap secondary cores...
[BOOT ] Starting KasperskyOS...
[HAL  ] Exceptions initialized

Loading:
 _  __                             _           ___  ____
| |/ /__ _ ___ _ __   ___ _ __ ___| | ___   _ / _ \/ ___|
| ' // _` / __| '_ \ / _ \ '__/ __| |/ / | | | | | \___ \
| . \ (_| \__ \ |_) |  __/ |  \__ \   <| |_| | |_| |___) |
|_|\_\__,_|___/ .__/ \___|_|  |___/_|\_\\__, |\___/|____/
              |_|                       |___/


Built on:   Jan 29 2025 08:35:32
Board:      vexpress_qemu
SoC:        v2pca15
ARCH:       arm64
Git hash:   toolchain_stage1-v1.0-7470-g7720fb4a876660ee3be4b3317682e6d2bfbe2181


Found CPU: ARMv8
System control register:
	Instruction cache: enabled
	Data cache: enabled
[HAL  ] Internal PFN-allocator initiated.
[HAL  ] cpu #0 - 1025MHz
[HAL  ] Cpu initialized.
[HAL  ] Temporary PTEs area has been initialized.
[HAL  ] Shared area has been initialized.
[HAL  ] Clean area has been initialized.
[HAL  ] MMU initialization - Done.
[HAL  ] WXN protection enabled.
[HAL  ] Zero space initialized.
[HAL  ] Found GICv2 with 160 interrupts.
[HAL  ] Interrupts initiated.
[HAL  ] Bootstrap Processor's ID - 0
CPU0 - boot strap processor
CPU1 - ordinary processor
CPU2 - ordinary processor
CPU3 - ordinary processor
[HAL  ] Hardware Abstraction Layer successfully initialized.
[INIT ] Starting core...
[MM   ] Range mapped begin : 0xffff8080021bc000
[MM   ] Range mapped end : 0xffff808002dbbfff
[MM   ] Range mapped size : 0x0000000000c00000
[MM   ] PFN DB size: 12288 KB
[MM   ] PFN database created.
[MM   ] Atomic buddy object created (8 MB).
[MM   ] Atomic PFN allocator initialized.
[MM   ] 	cache line size 64
[MM   ] 	cache line size 64
[MM   ] 	cache line size 64
[MM   ] 	cache line size 64
[MM   ] kmalloc alloc granularity 64
[MM   ] kalloc allocator created. Frontend enabled.
[VMM  ] #PF handler was installed.
[SMP  ] Initializing per-CPU areas...
[SMP  ] Single per-CPU area size: 320
[SMP  ] Total per-CPU area size: 4096
[UID  ] Uid allocator initialized.
[SID  ] Sid allocator initialized.
[IO   ] Initialize MMIO subsystem.
[IO   ] Initialize DMA subsystem.
[IO   ] Hardware interrupts successfully initialized.
[IO   ] Resources registry initialized.
[TASK ] Task infrastructure initialized.
[AUDIT] Starting core audit...
[VLOG ] Virtual logging subsystem initialized.
[SMP  ] Idle thread for CPU #0 created.
[SMP  ] Idle thread for CPU #1 created.
[SMP  ] Idle thread for CPU #2 created.
[SMP  ] Idle thread for CPU #3 created.
[TASK ] Scheduler interrupts initialized.
[TASK ] GSI page initialized.
[HAL  ] cpu #3 - 1038MHz
[HAL  ] cpu #2 - 1037MHz
[HAL  ] cpu #1 - 1038MHz
[SMP  ] CPU #2 - boostrapped.
[SMP  ] CPU #3 - boostrapped.
[SMP  ] CPU #1 - boostrapped.
[TASK ] Multitasking infrastructure initialized.
[TIME ] Time infrastructure initialized.
[ROFS ] Files: 3, size: 1970176 (0x001e1000).
[ROFS ] File #00: einit            - size:   939664 (0x000e5690)
[ROFS ] File #01: Hello            - size:   938208 (0x000e50e0)
[ROFS ] File #02: ksm.module       - size:    78024 (0x000130c8)
[VMM  ] Virtual Memory Manager service initialized.
[IO   ] I/O subsystem successfully initialized.
[FS   ] File System Manager successfully initialized.
[CM   ] Connection Manager successfully initialized.
[KSM  ] Module: 'ksm.module' loaded.
[KSM  ] Audit log created.
[KSM  ] Module: 'ksm.module' initialized.
[KSM  ] Server: 'kl.core.Core' executed.
[KSM  ] Security system successfully initialized.
[INIT ] Starting 'Einit' ...
[INIT ] Starting system worker.
[2025-02-21T08:04:44.071][Info][Einit][11:11][CRT0] VFS filesystem and network backends initialized with stub (related calls will return EIO)
[2025-02-21T08:04:44.159][Info][Hello][12:12][CRT0] VFS filesystem and network backends initialized with stub (related calls will return EIO)
Hello world from Kaspersky OS!
[INIT ] System worker finished
[INIT ] System halted...
System halted

```

```
 ~/dev/github/rb-kos-research/hello   main ●✚  tree
.
├── Einit.edl
├── Hello                      - bin file
├── hello.c
├── Hello.edl
├── Init                       - bin file
├── init.c
├── kos-qemu-image             - family different images
├── kos-qemu-image.bin
├── kos-qemu-image.dtb
├── kos-qemu-image.map
├── kos-qemu-image.romfs
├── kos-qemu-image.stripped
├── ksm.module                 - security module 
├── Makefile
├── security.psl
└── security.psl.c             - psl.c file

1 directory, 16 files

```

That is it. Let's clean temp files
```
clean:
	rm -f kos-qemu-image*
	rm -f Init
	rm -f Hello
	rm -f ksm.module
	rm -f security.psl.c
 ~/dev/github/rb-kos-research/hello 
```

And that is it. I described all stuff.

Thank you for reading. :) 
