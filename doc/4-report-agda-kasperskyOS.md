# Report: how to run agda on Kaspersky OS

## Start

What Did We Had:
1. We can build and run Haskell on Kaspersky OS [Report about this](3-report-port-microhs.md)
2. We had crazy idea if we can run haskell and we can build haskell to agda - we can (posibly) run agda on KasperskyOs

What was our plan:
1. Run agda example on ubuntu
2. Build agda code to haskell
3. Replace ghc to microhs
4. Build binary and run on KasperskyOs

Let's see result

## Project
```
ls -1            - comments
AgdaHello.agda   - new file
config-kos-64.h  - copied from microhs
Einit.edl        - copied form microhs
eval-kos-64.c    - copied form microhs
ffi.c            - copied form microhs
Hello.edl        - copied form microhs
init.c           - copied form microhs
Makefile         - copied form microhs but modifiend
RTE.hs           - new file
security.psl     - copied form microhs
```
Actually there is not so big difference with microhs example.


## Agda file
```
cat AgdaHello.agda 
module AgdaHello where

open import Agda.Builtin.IO using (IO)
open import Agda.Builtin.Unit using (⊤)
open import Agda.Builtin.String using (String)

postulate putStrLn : String → IO ⊤
{-# FOREIGN GHC import qualified Data.Text as T #-}
{-# FOREIGN GHC import qualified System.IO as SIO #-}
{-# COMPILE GHC putStrLn = (SIO.hPutStrLn SIO.stderr) . T.unpack #-}

main : IO ⊤
main = putStrLn "Hello world from Agda! \n"
```

There is one specific part:
```
{-# FOREIGN GHC import qualified System.IO as SIO #-}
{-# COMPILE GHC putStrLn = (SIO.hPutStrLn SIO.stderr) . T.unpack #-}
                                               |
                                           We need stderr to print with simple KasperskyOs app 
```

## RTE.hs

I had a few problems to port agda. And this file is a "patch" to make it works. 

Thera are 3 problems:
```
--FIXME: Problem 1 microhs cnat find this:
--import qualified GHC.Exts as GHC (Any)
data Any

--FIXME: Problem 2 what is generalCategory
--       Microhs can't compile this
--natToChar :: Integer -> Char
--natToChar n | generalCategory c == Surrogate = '\xFFFD'
--            | otherwise                      = c
--  where c = toEnum $ fromIntegral $ mod n 0x110000

--FIXME: Problem 3 what to do with * vs forall ?
--       Microhs can't compile this
--type Infinity (level :: *) a = Inf a
```


## Makefile
I will explain difference (if compare with microhs/Makefile)

We need just one thing: to build Hello.c

```
Hello: Hello.c
	${CC} \
		ffi.c \
		Hello.c \
		eval-kos-64.c \
		-static \
		-o Hello

Hello.c: Hello-opt.comb Addcombs
	./Addcombs Hello-opt.comb Hello.c

Hello-opt.comb: Hello.comb
	$(BIN)/mhseval +RTS -rHello.comb -oHello-opt.comb

Addcombs: MicroHs
	MHSDIR=$(MHSDIR) $(BIN)/gmhs -i$(TOOLS) Addcombs -oAddcombs

MicroHs:
	git clone https://github.com/augustss/MicroHs
	cd MicroHs && make bin/mhseval
	cd MicroHs && make bin/gmhs

```

Let's look at stage to building Hello.


```
# We build Malonzo 
MAlonzo: AgdaHello.agda
	agda  -i. --ghc-dont-call-ghc --compile --library=standard-library-2.2 AgdaHello.agda
#               |
#               We don't want to compile porject with ghc

# We build Hello.comb with microhs
Hello.comb: MAlonzo
	cp RTE.hs MAlonzo/RTE.hs       # Use patched RTE.hs 
	MHSDIR=$(MHSDIR) $(BIN)/gmhs \ # Use mirhohs to build
		MAlonzo.Code.AgdaHello \
		-i$(LIB)  \
		-oHello.comb

# microhs magic
Hello-opt.comb: Hello.comb
	$(BIN)/mhseval +RTS -rHello.comb -oHello-opt.comb


# microhs magic
Hello.c: Hello-opt.comb Addcombs
	./Addcombs Hello-opt.comb Hello.c


# use clang from SDK.
Hello: Hello.c
	${CC} \
		ffi.c \
		Hello.c \
		eval-kos-64.c \
		-static \
		-o Hello

```

And that is it.


## Let's run

I will cut some logs and comment.

Let's run:
```
make run
```

Let's see logs:


Run agda to make haskell
```
agda  -i. --ghc-dont-call-ghc --compile --library=standard-library-2.2 AgdaHello.agda
Checking AgdaHello (dev/github/rb-kos-research/agda-hello/AgdaHello.agda).
Compiling Agda.Primitive in /usr/share/libghc-agda-dev/lib/prim/Agda/Primitive.agdai to dev/github/rb-kos-research/agda-hello/MAlonzo/Code/Agda/Primitive.hs
Compiling Agda.Builtin.Sigma in /usr/share/libghc-agda-dev/lib/prim/Agda/Builtin/Sigma.agdai to dev/github/rb-kos-research/agda-hello/MAlonzo/Code/Agda/Builtin/Sigma.hs
Compiling Agda.Builtin.Maybe in /usr/share/libghc-agda-dev/lib/prim/Agda/Builtin/Maybe.agdai to dev/github/rb-kos-research/agda-hello/MAlonzo/Code/Agda/Builtin/Maybe.hs
Compiling Agda.Builtin.Bool in /usr/share/libghc-agda-dev/lib/prim/Agda/Builtin/Bool.agdai to dev/github/rb-kos-research/agda-hello/MAlonzo/Code/Agda/Builtin/Bool.hs
Compiling Agda.Builtin.Nat in /usr/share/libghc-agda-dev/lib/prim/Agda/Builtin/Nat.agdai to dev/github/rb-kos-research/agda-hello/MAlonzo/Code/Agda/Builtin/Nat.hs
Compiling Agda.Builtin.Char in /usr/share/libghc-agda-dev/lib/prim/Agda/Builtin/Char.agdai to dev/github/rb-kos-research/agda-hello/MAlonzo/Code/Agda/Builtin/Char.hs
Compiling Agda.Builtin.List in /usr/share/libghc-agda-dev/lib/prim/Agda/Builtin/List.agdai to dev/github/rb-kos-research/agda-hello/MAlonzo/Code/Agda/Builtin/List.hs
Compiling Agda.Builtin.String in /usr/share/libghc-agda-dev/lib/prim/Agda/Builtin/String.agdai to dev/github/rb-kos-research/agda-hello/MAlonzo/Code/Agda/Builtin/String.hs
Compiling Agda.Builtin.Unit in /usr/share/libghc-agda-dev/lib/prim/Agda/Builtin/Unit.agdai to dev/github/rb-kos-research/agda-hello/MAlonzo/Code/Agda/Builtin/Unit.hs
Compiling Agda.Builtin.IO in /usr/share/libghc-agda-dev/lib/prim/Agda/Builtin/IO.agdai to dev/github/rb-kos-research/agda-hello/MAlonzo/Code/Agda/Builtin/IO.hs
Compiling AgdaHello in dev/github/rb-kos-research/agda-hello/AgdaHello.agdai to dev/github/rb-kos-research/agda-hello/MAlonzo/Code/AgdaHello.hs
NOT calling: ghc -O -o dev/github/rb-kos-research/agda-hello/AgdaHello -Werror -idev/github/rb-kos-research/agda-hello -main-is MAlonzo.Code.AgdaHello dev/github/rb-kos-research/agda-hello/MAlonzo/Code/AgdaHello.hs --make -fwarn-incomplete-patterns
```

Clone microhs
```
git clone https://github.com/augustss/MicroHs
Cloning into 'MicroHs'...
remote: Enumerating objects: 18619, done.
remote: Counting objects: 100% (2340/2340), done.
remote: Compressing objects: 100% (322/322), done.
remote: Total 18619 (delta 2147), reused 2058 (delta 2015), pack-reused 16279 (from 3)
Receiving objects: 100% (18619/18619), 58.72 MiB | 28.06 MiB/s, done.
Resolving deltas: 100% (12593/12593), done.
```

Then build microhs (part1) 
```
cd MicroHs && make bin/mhseval
cc -Wall -O3  -Isrc/runtime src/runtime/eval-unix-64.c -lm src/runtime/comb.c -o bin/mhseval
size bin/mhseval
   text	   data	    bss	    dec	    hex	filename
 122996	   5792	   3448	 132236	  2048c	bin/mhseval
make[1]: Leaving directory 'dev/github/rb-kos-research/agda-hello/MicroHs'

```

Then build microhs (part2) 

```
cd MicroHs && make bin/gmhs
ghc -DNOTCABAL -XScopedTypeVariables -XTypeSynonymInstances -XMultiParamTypeClasses -XFlexibleInstances -ighc -isrc -ipaths -Wall -Wno-unrecognised-warning-flags -Wno-x-partial -Wno-deprecations -O  -package mtl -package pretty -package haskeline -package process -package time -package ghc-prim -package containers -package deepseq -package directory -package text -outputdir ghc-out  -main-is MicroHs.Main MicroHs.Main -o bin/gmhs
[ 1 of 45] Compiling Data.Integer     ( ghc/Data/Integer.hs, ghc-out/Data/Integer.o )
[ 2 of 45] Compiling MHSPrelude       ( ghc/MHSPrelude.hs, ghc-out/MHSPrelude.o )
[ 3 of 45] Compiling MicroHs.Flags    ( src/MicroHs/Flags.hs, ghc-out/MicroHs/Flags.o )
[ 4 of 45] Compiling MicroHs.IntMap   ( src/MicroHs/IntMap.hs, ghc-out/MicroHs/IntMap.o )
[ 5 of 45] Compiling MicroHs.IntSet   ( src/MicroHs/IntSet.hs, ghc-out/MicroHs/IntSet.o )
[ 6 of 45] Compiling MicroHs.List     ( src/MicroHs/List.hs, ghc-out/MicroHs/List.o )
[ 7 of 45] Compiling MicroHs.Graph    ( src/MicroHs/Graph.hs, ghc-out/MicroHs/Graph.o )
[ 8 of 45] Compiling MicroHs.State    ( src/MicroHs/State.hs, ghc-out/MicroHs/State.o )
[ 9 of 45] Compiling MicroHs.StateIO  ( src/MicroHs/StateIO.hs, ghc-out/MicroHs/StateIO.o )
[10 of 45] Compiling Paths_MicroHs    ( paths/Paths_MicroHs.hs, ghc-out/Paths_MicroHs.o )
[11 of 45] Compiling System.Compress  ( ghc/System/Compress.hs, ghc-out/System/Compress.o )
[12 of 45] Compiling MicroHs.MakeCArray ( src/MicroHs/MakeCArray.hs, ghc-out/MicroHs/MakeCArray.o )
[13 of 45] Compiling System.Console.SimpleReadline ( ghc/System/Console/SimpleReadline.hs, ghc-out/System/Console/SimpleReadline.o )
[14 of 45] Compiling System.IO.MD5    ( ghc/System/IO/MD5.hs, ghc-out/System/IO/MD5.o )
[15 of 45] Compiling System.IO.Serialize ( ghc/System/IO/Serialize.hs, ghc-out/System/IO/Serialize.o )
[16 of 45] Compiling System.IO.TimeMilli ( ghc/System/IO/TimeMilli.hs, ghc-out/System/IO/TimeMilli.o )
[17 of 45] Compiling PrimTable        ( ghc/PrimTable.hs, ghc-out/PrimTable.o )
[18 of 45] Compiling Text.ParserComb  ( src/Text/ParserComb.hs, ghc-out/Text/ParserComb.o )
[19 of 45] Compiling Text.PrettyPrint.HughesPJLite ( src/Text/PrettyPrint/HughesPJLite.hs, ghc-out/Text/PrettyPrint/HughesPJLite.o )
[20 of 45] Compiling MicroHs.Ident    ( src/MicroHs/Ident.hs, ghc-out/MicroHs/Ident.o )
[21 of 45] Compiling MicroHs.Names    ( src/MicroHs/Names.hs, ghc-out/MicroHs/Names.o )
[22 of 45] Compiling MicroHs.Lex      ( src/MicroHs/Lex.hs, ghc-out/MicroHs/Lex.o )
[23 of 45] Compiling MicroHs.TargetConfig ( src/MicroHs/TargetConfig.hs, ghc-out/MicroHs/TargetConfig.o )
[24 of 45] Compiling MicroHs.IdentMap ( src/MicroHs/IdentMap.hs, ghc-out/MicroHs/IdentMap.o )
[25 of 45] Compiling MicroHs.Builtin  ( src/MicroHs/Builtin.hs, ghc-out/MicroHs/Builtin.o )
[26 of 45] Compiling MicroHs.Expr     ( src/MicroHs/Expr.hs, ghc-out/MicroHs/Expr.o )
[27 of 45] Compiling MicroHs.SymTab   ( src/MicroHs/SymTab.hs, ghc-out/MicroHs/SymTab.o )
[28 of 45] Compiling MicroHs.TCMonad  ( src/MicroHs/TCMonad.hs, ghc-out/MicroHs/TCMonad.o )
[29 of 45] Compiling MicroHs.Parse    ( src/MicroHs/Parse.hs, ghc-out/MicroHs/Parse.o )
[30 of 45] Compiling MicroHs.Fixity   ( src/MicroHs/Fixity.hs, ghc-out/MicroHs/Fixity.o )
[31 of 45] Compiling MicroHs.Deriving ( src/MicroHs/Deriving.hs, ghc-out/MicroHs/Deriving.o )
[32 of 45] Compiling MicroHs.TypeCheck ( src/MicroHs/TypeCheck.hs, ghc-out/MicroHs/TypeCheck.o )
[33 of 45] Compiling MicroHs.Exp      ( src/MicroHs/Exp.hs, ghc-out/MicroHs/Exp.o )
[34 of 45] Compiling MicroHs.EncodeData ( src/MicroHs/EncodeData.hs, ghc-out/MicroHs/EncodeData.o )
[35 of 45] Compiling MicroHs.Desugar  ( src/MicroHs/Desugar.hs, ghc-out/MicroHs/Desugar.o )
[36 of 45] Compiling MicroHs.Package  ( src/MicroHs/Package.hs, ghc-out/MicroHs/Package.o )
[37 of 45] Compiling MicroHs.FFI      ( src/MicroHs/FFI.hs, ghc-out/MicroHs/FFI.o )
[38 of 45] Compiling MicroHs.ExpPrint ( src/MicroHs/ExpPrint.hs, ghc-out/MicroHs/ExpPrint.o )
[39 of 45] Compiling MicroHs.Translate ( src/MicroHs/Translate.hs, ghc-out/MicroHs/Translate.o )
[40 of 45] Compiling MicroHs.CompileCache ( src/MicroHs/CompileCache.hs, ghc-out/MicroHs/CompileCache.o )
[41 of 45] Compiling MicroHs.Abstract ( src/MicroHs/Abstract.hs, ghc-out/MicroHs/Abstract.o )
[42 of 45] Compiling MicroHs.Compile  ( src/MicroHs/Compile.hs, ghc-out/MicroHs/Compile.o )
[43 of 45] Compiling MicroHs.Interactive ( src/MicroHs/Interactive.hs, ghc-out/MicroHs/Interactive.o )
[44 of 45] Compiling MicroHs.Main     ( src/MicroHs/Main.hs, ghc-out/MicroHs/Main.o )
[45 of 45] Linking bin/gmhs

```

Then build microhs (part3) 
```
MHSDIR=MicroHs MicroHs/bin/gmhs -iMicroHs/Tools Addcombs -oAddcombs
```

Path RTE.hs and and compile haskell with microhs

```
cp RTE.hs MAlonzo/RTE.hs
MHSDIR=MicroHs MicroHs/bin/gmhs \
	MAlonzo.Code.AgdaHello \
	-iMicroHs/lib  \
	-oHello.comb
```

Use microhs magic to make Hello.c
```
MHSDIR=MicroHs MicroHs/bin/gmhs \
	MAlonzo.Code.AgdaHello \
	-iMicroHs/lib  \
	-oHello.comb
MicroHs/bin/mhseval +RTS -rHello.comb -oHello-opt.comb
./Addcombs Hello-opt.comb Hello.c
```

And then use magic to build Hello and Kaspersky Os Magic.

Prepare Hello binary
```
"""/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain"/bin/aarch64-kos-clang" \
	ffi.c \
	Hello.c \
	eval-kos-64.c \
	-static \
	-o Hello
```


Prepare Init binary
```
"""/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain"/bin/aarch64-kos-clang" --target=aarch64-kos -static -no-pie init.c -o Init
```


Prepare ksm.module (security modele for kernel)
```
"/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain/bin/nk-psl-gen-c \
	security.psl \
	-P"/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/sysroot-aarch64-kos/include/system.platform -I"/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/sysroot-aarch64-kos/include -I"/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain/include -Idev/github/rb-kos-research/agda-hello \
	-o security.psl.c
"""/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain"/bin/aarch64-kos-clang" -fno-pic -D__KOS_KERNEL__ -DQCBOR_DISABLE_FLOAT_HW_USE -DQCBOR_DISABLE_INDEFINITE_LENGTH_ARRAYS -DQCBOR_DISABLE_INDEFINITE_LENGTH_STRINGS -DQCBOR_DISABLE_UNCOMMON_TAGS -DQCBOR_DISABLE_EXP_AND_MANTISSA -DQCBOR_DISABLE_PREFERRED_FLOAT -DUSEFULBUF_DISABLE_ALL_FLOAT -DUSEFULBUF_DISABLE_DEPRECATED -Wall -mcpu=cortex-a57 -ffixed-x18 -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer -O2 -Wextra -Wpointer-arith -ffreestanding -mgeneral-regs-only -fno-pie -no-pie -Wframe-larger-than=12288 -fstack-protector-all -g0 -nostdlib -z separate-code -z max-page-size=4096 -lksm_kss-qemu -lrtl-kernel-qemu -Wl,--entry=KsmKssEntry -Wl,--image-base=0xFFFF808000000000 -fuse-ld=lld -L/"/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain/lib/clang/17/lib/kos -lclang_rt.builtins-aarch64 security.psl.c -o ksm.module
```


Make kos-qemu-image
```
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
```


Run qemu
```
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
[HAL  ] cpu #0 - 1017MHz
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
[HAL  ] cpu #2 - 1048MHz
[HAL  ] cpu #1 - 1044MHz
[HAL  ] cpu #3 - 1048MHz
[SMP  ] CPU #3 - boostrapped.
[SMP  ] CPU #2 - boostrapped.
[SMP  ] CPU #1 - boostrapped.
[TASK ] Multitasking infrastructure initialized.
[TIME ] Time infrastructure initialized.
[ROFS ] Files: 3, size: 2195456 (0x00218000).
[ROFS ] File #00: einit            - size:   939664 (0x000e5690)
[ROFS ] File #01: Hello            - size:  1163992 (0x0011c2d8)
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
[2025-02-24T14:22:46.076][Info][Einit][11:11][CRT0] VFS filesystem and network backends initialized with stub (related calls will return EIO)
[2025-02-24T14:22:46.193][Info][Hello][12:12][CRT0] VFS filesystem and network backends initialized with stub (related calls will return EIO)
Hello world from Agda! 

[INIT ] System worker finished
[INIT ] System halted...

```

And "Hello world from Agda" printed


## Discussion

What can we do better?
1. We need research perfomance microhs (we need time to wait output)
2. We need custom RTE.hs (What should we do here?)
3. It time to make smt usefull.


## Conclusion
We want to run agda and we did this. 
Thank you!
