# Report: how to port MicroHs to Kaspersky OS

## Start
What We had:
1. Hello example how to build simple hello.c and run on KasperskyOs [Report about this](2-report.md)
1. desire use haskell for this
1. MicroHs - compiler for haskell (pretty small one) [MicroHs](https://github.com/augustss/MicroHs)

What our plan was:
1. Built MicroHs itself.
1. Built Hello.hs to ubuntu
1. Compile this to kos.
1. Run in KasperskyOs

Let's see result.

## Proect
```
 ~/dev/github/rb-kos-research/microhs   main ●  ls -1
config-kos-64.h
Einit.edl
eval-kos-64.c
ffi.c
Hello.edl
Hello.hs
init.c
Makefile
security.psl
```

That is it.

Let's comment what we had at the begging:

```
config-kos-64.h
Einit.edl         - from hello example
eval-kos-64.c
ffi.c
Hello.edl         - from hello example
Hello.hs
init.c            - from hello example
Makefile          - from hello example but modified
security.psl      - from hello example
```

Let`s clean from old stuff

```
config-kos-64.h
eval-kos-64.c
ffi.c
Hello.hs
Makefile          - modified
```

### Hello.hs
```
 ~/dev/github/rb-kos-research/microhs   main ●  cat Hello.hs
module Hello where
import System.IO

main :: IO ()
main = do
  let a = "Hello form haskell" in hPutStrLn stderr a
```
Just print Text.

### haskell runtime

**ffi.c**
This is table for Foreign Function Interface 
You see - table is empty.
It needs for building binary anyway.
```
 ~/dev/github/rb-kos-research/microhs   main ●  cat ffi.c
#include "MicroHs/src/runtime/mhsffi.h"
static struct ffi_entry table[] = {
{ 0,0 }
};
struct ffi_entry *xffi_table = table;
```

**eval-kos-64.c**
```
 ~/dev/github/rb-kos-research/microhs   main ●  cat eval-kos-64.c 

#include "config-kos-64.h"
#include "MicroHs/src/runtime/eval.c"
```
***config-kos-64.h** - mostly I copied this.
```
 ~/dev/github/rb-kos-research/microhs   main ●  cat config-kos-64.h 
/*
 * Various platform specific configuration.
 */

/*
 * Include stdio functions.
 * Without this none of the file I/O in System.IO is available.
 */
#define WANT_STDIO 1
#if WANT_STDIO
#include <unistd.h>
#endif  /* WANT_STDIO */




/*
 * Include ops for floating point arithmetic.
 * Without this +,-,* etc will not be available for the Double type.
 */
#define WANT_FLOAT 0

/*
 * Include <math.h>
 * Without this, exp,sin, etc are not available.
 */
#define WANT_MATH 0

/*
 * Include MD5 checksumming code
 */
#define WANT_MD5 0

/*
 * Include profiling code
 */
#define WANT_TICK 1

/*
 * Process argc, argv
 */
#define WANT_ARGS 1

/*
 * Number of bits in a word.  Only 32 and 64 are supported.
 */
//#define WORD_SIZE 64

/*
 * Find First Set
 * This macro must be defined.
 * It return the number of the least significant bit that is set.
 * Numberings starts from 1.  If no bit is set, it should return 0.
 */
/* #define FFS ffsl */

/*
 * This is the character used for comma-separation in printf.
 * Defaults to "'".
 */
/* #define PCOMMA "'" */


/*
 * Get a raw input character.
 * If undefined, the default always returns -1
 */
/* #define GETRAW */


/*
 * Get time since some epoch in milliseconds.
 */
/* #define GETTIMEMILLI */


/*
 * The ERR macro should report an error and exit.
 * If not defined, a generic one will be used.
 */
/* #define ERR(s) */
/* #define ERR1(s,a) */

#define GCRED    0              /* do some reductions during GC */
#define FASTTAGS 0              /* compute tag by pointer subtraction */
#define INTTABLE 0              /* use fixed table of small INT nodes */
#define SANITY   0              /* do some sanity checks */
#define STACKOVL 0              /* check for stack overflow */
```

**Makefile** Let's print diff:

```
 ~/dev/github/rb-kos-research/microhs   main ●  diff  Makefile ../hello
1,7c1
< .PHONY: distclean clean run runHost
< 
< MHSDIR=MicroHs
< BIN=MicroHs/bin
< LIB=MicroHs/lib
< TOOLS=MicroHs/Tools
< RUNTIME=MicroHs/src/runtime
---
> .PHONY: clean run
22a17,19
> Hello: hello.c
> 	${CC} hello.c -o Hello
> 
93,136d89
< 
< Hello: Hello.c
< 	${CC} \
< 		ffi.c \
< 		Hello.c \
< 		eval-kos-64.c \
< 		-static \
< 		-o Hello
< 
< runHost: HelloHost
< 	./HelloHost
< 
< 
< 
< HelloHost: Hello.c
< 	clang \
< 		ffi.c \
< 		Hello.c \
< 		${RUNTIME}/eval-unix-64.c \
< 		-lm\
< 		-static \
< 		-o HelloHost
< 
< 
< Hello.c: Hello-opt.comb Addcombs
< 	./Addcombs Hello-opt.comb Hello.c
< 
< Hello-opt.comb: Hello.comb
< 	$(BIN)/mhseval +RTS -rHello.comb -oHello-opt.comb
< 
< Hello.comb: Hello.hs MicroHs
< 	MHSDIR=$(MHSDIR) $(BIN)/gmhs -i$(LIB) Hello -oHello.comb
< 
< Addcombs: MicroHs
< 	MHSDIR=$(MHSDIR) $(BIN)/gmhs -i$(TOOLS) Addcombs -oAddcombs
< 
< test: MicroHs
< 	cd MicroHs && make runtest
< 
< MicroHs:
< 	git clone https://github.com/augustss/MicroHs
< 	cd MicroHs && make bin/mhseval
< 	cd MicroHs && make bin/gmhs
< 
138,141d90
< 	rm -rf Hello.c
< 	rm -rf Hello
< 	rm -rf Hello.comb
< 	rm -rf Hello-opt.comb
145,146d93
< 	rm -f HelloHost
< 	rm -f Addcombs
149,153d95
< 
< 
< 
< distclean:
< 	rm -rf MicroHs

```

## Makefile in details

Let's see target's declarations

```
.PHONY: distclean clean run runHost

run: kos-qemu-image
Init: init.c
security.psl.c: security.psl
ksm.module: security.psl.c
kos-qemu-image: Hello Init ksm.module

Hello: Hello.c
runHost: HelloHost
HelloHost: Hello.c

Hello.c: Hello-opt.comb Addcombs
Hello-opt.comb: Hello.comb
Hello.comb: Hello.hs MicroHs

Addcombs: MicroHs

test: MicroHs
MicroHs:
clean:
distclean:
```

Let's remove targets from hello example and leave here only suffient:

```
# group of target to build Hello.hs
Hello: Hello.c
runHost: HelloHost
HelloHost: Hello.c

Hello.c: Hello-opt.comb Addcombs
Hello-opt.comb: Hello.comb
Hello.comb: Hello.hs MicroHs

# Extra Tool
Addcombs: MicroHs

# targets to build Microhs
test: MicroHs
MicroHs:

# clean section
clean:
distclean:
```

Ok. Let's see how to get, build, clean and test MicroHs 

```
test: MicroHs
	cd MicroHs && make runtest <-- this is really long process

MicroHs:
	git clone https://github.com/augustss/MicroHs
	cd MicroHs && make bin/mhseval
	cd MicroHs && make bin/gmhs

distclean:
	rm -rf MicroHs
```

By default MicroHs build:

- bin/gmhs     - generator "bitecode"
- bin/mhseval  - "virtual machine" 



### How to build haskell

Let's see again Makefile:
```
HelloHost: Hello.c
	clang \
		ffi.c \
		Hello.c \
		${RUNTIME}/eval-unix-64.c \
		-lm\
		-static \
		-o HelloHost


Hello.c: Hello-opt.comb Addcombs
	./Addcombs Hello-opt.comb Hello.c

Hello-opt.comb: Hello.comb
	$(BIN)/mhseval +RTS -rHello.comb -oHello-opt.comb

Hello.comb: Hello.hs MicroHs
	MHSDIR=$(MHSDIR) $(BIN)/gmhs -i$(LIB) Hello -oHello.comb

Addcombs: MicroHs
	MHSDIR=$(MHSDIR) $(BIN)/gmhs -i$(TOOLS) Addcombs -oAddcombs

MicroHs:
	cd MicroHs && make bin/mhseval
	cd MicroHs && make bin/gmhs

```

To build HelloHost we need Hello.c and runtime but how exatly we get Hello.c?

Let's see stages
```
Hello.hs        -> 
Hello.comb      -> | this is combinator's bytecode
Hello-opt.comb  -> | this is optimized bytecode
Hello.c            | this bytecode in  c

```

Anouther look:

```
Source      compilation     optimization       compress to c
Hello.hs -> gmhs         -> mhseval      ->    Addcombs      -> Hello.c

```




We have Hello.hs.

```
 ~/dev/github/rb-kos-research/microhs   main ●  cat Hello.hs 
module Hello where
import System.IO

main :: IO ()
main = do
  let a = "Hello form haskell" in hPutStrLn stderr a
```

```
cat Hello.comb 
v7.0
250
A _237 _245 @_246 _248 I @@fromUTF8 "Hello form haskell" @@@:249 @
<----CUT---->
```

And then we have Hello.c
```
static unsigned char combexprdata[] = {
118,55,46,48,10,55,52,10,83,39,32,67,39,32,85,32,75,50,32,75,
<---------------------------CUT-------------------->
32,102,111,114,109,32,104,97,115,107,101,108,108,34,32,64,64,64,125,
};
const unsigned char *combexpr = combexprdata;
const int combexprlen = 7359;
```


### How to build haskell on KasperskyOS
```
SDK        ="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"
TOOLCHAIN  = "${SDK}/toolchain"
CC         = "${TOOLCHAIN}/bin/aarch64-kos-clang"

Hello: Hello.c
	${CC} \
		ffi.c \
		Hello.c \
		eval-kos-64.c \
		-static \
		-o Hello
```

Let's see how to port haskell runtime.

```
  ~/dev/github/rb-kos-research/microhs   main ●  cat eval-kos-64.c 

#include "config-kos-64.h"
#include "MicroHs/src/runtime/eval.c"

```
I will explain a little bit more config-kos-64.h 

```

#define WANT_STDIO 1            // If we want to print text we need stdio
#if WANT_STDIO
#include <unistd.h>             // this is becouse haskell for STDIO require unlink
#endif  /* WANT_STDIO */

                                // we don't need all stuff below right now
#define WANT_FLOAT 0
#define WANT_MATH 0
#define WANT_MD5 0


#define WANT_TICK 1             // I don't know exatly why I enabled this.

#define WANT_ARGS 1             // Maybe we need this maybe not

                                // I have no idea what stuff below need for
//#define WORD_SIZE 64
/* #define FFS ffsl */
/* #define PCOMMA "'" */
/* #define GETRAW */
/* #define GETTIMEMILLI */
/* #define ERR(s) */
/* #define ERR1(s,a) */

                                // I copied this and I have no idea what it need for too
#define GCRED    0              /* do some reductions during GC */
#define FASTTAGS 0              /* compute tag by pointer subtraction */
#define INTTABLE 0              /* use fixed table of small INT nodes */
#define SANITY   0              /* do some sanity checks */
#define STACKOVL 0              /* check for stack overflow */
```

That is it.
We just replace clang to clang from SDK and it builded and it works.

### Full Makefile 

Ok. Ok. Ok: This full *Makefile*:

```
 ~/dev/github/rb-kos-research/microhs   main ●  cat Makefile
.PHONY: distclean clean run runHost

MHSDIR=MicroHs
BIN=MicroHs/bin
LIB=MicroHs/lib
TOOLS=MicroHs/Tools
RUNTIME=MicroHs/src/runtime

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


Hello: Hello.c
	${CC} \
		ffi.c \
		Hello.c \
		eval-kos-64.c \
		-static \
		-o Hello

runHost: HelloHost
	./HelloHost



HelloHost: Hello.c
	clang \
		ffi.c \
		Hello.c \
		${RUNTIME}/eval-unix-64.c \
		-lm\
		-static \
		-o HelloHost


Hello.c: Hello-opt.comb Addcombs
	./Addcombs Hello-opt.comb Hello.c

Hello-opt.comb: Hello.comb
	$(BIN)/mhseval +RTS -rHello.comb -oHello-opt.comb

Hello.comb: Hello.hs MicroHs
	MHSDIR=$(MHSDIR) $(BIN)/gmhs -i$(LIB) Hello -oHello.comb

Addcombs: MicroHs
	MHSDIR=$(MHSDIR) $(BIN)/gmhs -i$(TOOLS) Addcombs -oAddcombs

test: MicroHs
	cd MicroHs && make runtest

MicroHs:
	git clone https://github.com/augustss/MicroHs
	cd MicroHs && make bin/mhseval
	cd MicroHs && make bin/gmhs

clean:
	rm -rf Hello.c
	rm -rf Hello
	rm -rf Hello.comb
	rm -rf Hello-opt.comb
	rm -f kos-qemu-image*
	rm -f Init
	rm -f Hello
	rm -f HelloHost
	rm -f Addcombs
	rm -f ksm.module
	rm -f security.psl.c



distclean:
	rm -rf MicroHs
```


## Extra details
Let's see Hello.hs again:
```
 ~/dev/github/rb-kos-research/microhs   main ●  cat Hello.hs
module Hello where
import System.IO

main :: IO ()
main = do
  let a = "Hello form haskell" in hPutStrLn stderr a
```

Why do we need hPutStrLn stderr?

This is because in our small system we don't have posix support.
In KasperskyOs stdout implemented with virtual filesystem "vfs" (special process).
Without this stdout doesn't work. So we will use stderr instead (looks like a hack in libc kasperskyOS).

## Let's run

```
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
[HAL  ] cpu #0 - 1013MHz
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
[HAL  ] cpu #1 - 1024MHz
[HAL  ] cpu #3 - 1021MHz
[HAL  ] cpu #2 - 1028MHz
[SMP  ] CPU #3 - boostrapped.
[SMP  ] CPU #1 - boostrapped.
[SMP  ] CPU #2 - boostrapped.
[TASK ] Multitasking infrastructure initialized.
[TIME ] Time infrastructure initialized.
[ROFS ] Files: 3, size: 2195456 (0x00218000).
[ROFS ] File #00: einit            - size:   939664 (0x000e5690)
[ROFS ] File #01: Hello            - size:  1163960 (0x0011c2b8)
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
[2025-02-22T13:42:02.075][Info][Einit][11:11][CRT0] VFS filesystem and network backends initialized with stub (related calls will return EIO)
[2025-02-22T13:42:02.179][Info][Hello][12:12][CRT0] VFS filesystem and network backends initialized with stub (related calls will return EIO)
Hello form haskell
[INIT ] System worker finished
[INIT ] System halted...
System halted

```

**Hello from haskell** !
Thank you for attension.

## Discussion

A list of next steps and questions.

Shuld we make fork MicroHs and push config for KasperskyOs to upstream?

Haskell in Kos work slow. (Visually slow).
- Maybe I should try flags for clang -03 etc.
- Maybe Runtime load relly long progamm (and time to heat Virtual Machine)
- We should research a little bit options for config.h (enable math for example)
- We should research ffi.






