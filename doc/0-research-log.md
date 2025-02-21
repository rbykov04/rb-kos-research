# 26
## Progress:
- what is bin/ghms - compiler itself. It compiles file.hs -> out.comb
- what is bin/mhsevel - utility to run out.comb
- maybe run same examples on linux - DONE
- what is in bin folder -DONE

tree MicroHs/bin
MicroHs/bin
├── gmhs
└── mhseval


## Active

To port this to KasperskyOS we need build binary without out.comb
There is example on Stm32 in MicroHs.

Let's read example and build simple binary


Ok we did this

```
MHSDIR=MicroHs MicroHs/bin/gmhs -iMicroHs/lib Hello -oHello.comb
MicroHs/bin/mhseval +RTS -rHello.comb -oHello-opt.comb
./Addcombs Hello-opt.comb Hello.c
clang \
	ffi.c \
	Hello.c \
	MicroHs/src/runtime/eval-unix-64.c \
	-lm\
	-static \
	-o Hello
./Hello
"Hello form haskell"

```


# 25
Progress:
- what is bin/ghms ?
- what is bin/mhsevel ?
- maybe run same examples on linux?
- what is in bin folder?

Let's build create an example on Linux

What I made:
```
 ~/dev/github/rb-kos-research/microhs   main ●  cat Hello.hs 
module Hello where

main :: IO ()
main = do
  let a = "Hello form haskell" in print a
```

```
 ~/dev/github/rb-kos-research/microhs   main ●  cat Makefile      
.PHONY: distclean clean

run: out.comb
	./MicroHs/bin/mhseval

out.comb: Hello.hs
	MicroHs/bin/gmhs -iMicroHs/lib Hello

clean:
	rm -rf out.comb
```

It works:
```
 ~/dev/github/rb-kos-research/microhs   main ●  make clean && make
rm -rf out.comb
MicroHs/bin/gmhs -iMicroHs/lib Hello
./MicroHs/bin/mhseval
"Hello form haskell"
```

It maybe curies what is in out.comb: (first )
```
~/dev/github/rb-kos-research/microhs   main ●  cat out.comb
v7.0
252
A _247 _142 _164 @@_248 _250 I @@fromUTF8 "Hello form haskell" @@@:251 @
A K _249 I @@:250 @
A U :249 @
A U I @:248 @
A B B _246 @@_134 @:247 @
A _237 _245 @:246 @
A _243 _244 @_231 @fromUTF8 "stdout" @@:245 @
A IO.stdout :244 @
<---CUT----->
```

# 24 
I cloned https://github.com/augustss/MicroHs
Let's play on ubuntu with this

There is a Makefile

```
~/dev/github/rb-kos-research/microhs/MicroHs   master  make
echo "[default]"         > targets.conf
echo cc = \"cc\"     >> targets.conf
echo ccflags = \"\" >> targets.conf
echo conf = \"unix-64\" >> targets.conf
echo ''                 >> targets.conf
echo "[emscripten]"     >> targets.conf
echo cc = \"emcc -sALLOW_MEMORY_GROWTH -sTOTAL_STACK=5MB -sNODERAWFS -sSINGLE_FILE -DUSE_SYSTEM_RAW\"   >> targets.conf
echo conf = \"unix-64\" >> targets.conf
cc -Wall -O3  -Isrc/runtime src/runtime/eval-unix-64.c -lm generated/mhs.c -o bin/mhs
cc -Wall -O3  -Isrc/runtime src/runtime/eval-unix-64.c -lm generated/cpphs.c -o bin/cpphs
cc -Wall -O3  -Isrc/runtime src/runtime/eval-unix-64.c -lm generated/mcabal.c -o bin/mcabal
```

Ok

```
~/dev/github/rb-kos-research/microhs/MicroHs   master  make runtest
cc -Wall -O3  -Isrc/runtime src/runtime/eval-unix-64.c -lm src/runtime/comb.c -o bin/mhseval
size bin/mhseval
   text	   data	    bss	    dec	    hex	filename
 122996	   5792	   3448	 132236	  2048c	bin/mhseval
ghc -DNOTCABAL -XScopedTypeVariables -XTypeSynonymInstances -XMultiParamTypeClasses -XFlexibleInstances -ighc -isrc -ipaths -Wall -Wno-unrecognised-warning-flags -Wno-x-partial -Wno-deprecations -O  -package mtl -package pretty -package haskeline -package process -package time -package ghc-prim -package containers -package deepseq -package directory -package text -outputdir ghc-out  -main-is MicroHs.Main MicroHs.Main -o bin/gmhs
[ 1 of 45] Compiling Data.Integer     ( ghc/Data/Integer.hs, ghc-out/Data/Integer.o )

<----------- CAT of output ------------------>

[44 of 45] Compiling MicroHs.Main     ( src/MicroHs/Main.hs, ghc-out/MicroHs/Main.o )
[45 of 45] Linking bin/gmhs


cd tests; make alltest

../bin/gmhs -i../lib Info       && ../bin/mhseval +RTS -H1M -RTS
Running on Unix
64 bit words, little endian
../bin/gmhs -i../lib Hello      && ../bin/mhseval +RTS -H1M -RTS > Hello.out      && diff Hello.ref Hello.out
../bin/gmhs -i../lib Serdes     && ../bin/mhseval +RTS -H1M -RTS > Serdes.out     && diff Serdes.ref Serdes.out

<-------- CAT of output ---------------->
../bin/gmhs -i../lib ByteStringIO && ../bin/mhseval +RTS -H1M -RTS > ByteStringIO.out && diff ByteStringIO.ref ByteStringIO.out
sh errtester.sh ../bin/gmhs < errmsg.test

```

It builded and tested

Next step:
- what is bin/ghms ?
- what is bin/mhsevel ?
- maybe run same examples on linux?
- what is in bin folder?


# 23

To move forward We need write programms on some language.
From Box - threre are 3 of them

- C
- C++
- Rust

But what if I want to use haskell?
Let's port haskell to KasperskyOS.
Let's try with
https://github.com/augustss/MicroHs

# 22
Let's make example hello selfcontainted and move Makefile to example

# 21
get rid of cmake!
```
tree 
.
├── Einit.edl
├── hello.c
├── Hello.edl
├── init.c
├── init.yaml.in
└── security.psl.in
```

# 20

Progress:
1. security.psl   -> "cmake magic" -> security.psl.c
1. security.psl.c -> "cmake magic" -> ksm.module - DONE

Let's build security.psl.c


# 19
Progress:
```
    --with-init=hello/build/EinitQemu  - DONE
    hello/build/Hello  - DONE
    hello/build/EinitQemu-kss/ksm.module - it is security.module
```

But how to build security.module?

1. security.psl   -> "cmake magic" -> security.psl.c
1. security.psl.c -> "cmake magic" -> ksm.module

Can we just use security.psl.c?

Maybe? but

```
wc -l hello/build/EinitQemu-kss/security.psl.c 
34241 hello/build/EinitQemu-kss/security.psl.c
```
32241 line of c code.

Maybe in future we work on this.

Lets reimpliment cmake and start with ksm.module 




# 18
Let's research what is EinitQemu?

1. init.yaml.in -> EinitQemu.c
1. EinitQemu.c -> EinitQemu

init.yaml is toml
```
entities:
- name: hello.Hello
```

Let's eliminate init.yaml and use einit.c directly

``````
hello/Init: hello/init.c
	${CC} hello/init.c -o hello/Init
``

It is not enough

```
[TASK ] Incompatible image for task 'einit'.

[KERNEL PANIC]: Can't load init task.

Check your security configuration in the PSL file.

ESR = 0x0
EC = 0x0
Exception class: Unknown reason
    EIID : kl.core.Core
    Name : kl.core.Core
    Path : 
    TID  : 1
    pc   : 0xffff808001055778
    CPU  : 00

Relocation base: <0000000000000000>
Stack bounds: ffff80800106fff0 ffff808001077ff0
Call Trace (Privileged Mode):
    [<ffff808001055778>] ???
    [<ffff808001055758>] ???
    [<ffff80800103a610>] ???
    [<ffff80800101bbe8>] ???
    [<ffff808001008a38>] ???
    [<ffff80800105010c>] ???

System halted
```

Solution:

```
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
```

# 17 
Can we build binary below without cmake?
```
    --with-init=hello/build/EinitQemu  - it is init binary
    hello/build/Hello  - it is hello binary
    hello/build/EinitQemu-kss/ksm.module - it is security.module
```

Let's try with hello.

Result:

```
TOOLCHAIN  = "${SDK}/toolchain"
CC         = "${TOOLCHAIN}/bin/aarch64-kos-clang"

hello/Hello: hello/hello.c
	${CC} hello/hello.c -o hello/Hello
```


# 16
What is happening here:

```
hello/build:
	cd hello && \
	export PATH="${SDK}/toolchain/bin:${PATH}" && \
	cmake -B "${BUILD}" \
		-D BOARD="${BOARD}" \
		-D CMAKE_TOOLCHAIN_FILE="${SDK}/toolchain/share/toolchain-${TARGET}-clang.cmake"
	cd hello && cmake --build "${BUILD}" --target kos-qemu-image
```

It is so complicated!

I can guess we do here: 
1. prepare build dir
1. do some cmake magic
1. build kos-qemu-image

Lets add extra build kos-qemu-image over cmake to unpack this stuff.

Don't ask me how! But here it is:
```
hello/build/kos-qemu-image: hello/build
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
    --with-init=hello/build/EinitQemu \
    hello/build/Hello \
    hello/build/EinitQemu-kss/ksm.module
```

Pay attention to 

```
    --with-init=hello/build/EinitQemu  - it is init binary
    hello/build/Hello  - it is hello binary
    hello/build/EinitQemu-kss/ksm.module - it is security.module
```


# 15
Why do we need cmake to run qemu??

We need just this:
```
${SDK}/toolchain/bin/qemu-system-aarch64 ${QEMU_FLAGS} -kernel kos-qemu-image
```

# 14
Make policy file self contained.

Threat model - there are not threats:

```
execute: kl.core.Execute

use nk.base._

use EDL Einit
use EDL kl.core.Core
use EDL hello.Hello

execute  { grant () }
request  { grant () }
response { grant () }
security { grant () }
error    { grant () }
```


# 13
Move to Makefile. Get rid of crossbuild.sh

# 12
Let's simplify ./cross-build.sh from hello
Separate to 3 stages.

Why do we need in this crossbuild.sh???


```
# configure
cmake -B "$BUILD" \
      -D BOARD="$BOARD" \
      -D CMAKE_TOOLCHAIN_FILE="$SDK_PREFIX/toolchain/share/toolchain-$TARGET-clang.cmake"

# build
cmake --build "$BUILD" --target kos-qemu-image

# run qemu
cmake --build "$BUILD" --target sim
```



# 11
Let's use cmake from ubuntu.


# 10

Let's simplify and make structure flat
```
 ~/dev/github/rb-kos-research   main  (cd hello && tree)
.
├── CMakeLists.txt
├── cross-build.sh
├── hello.c
├── Hello.edl
├── init.yaml.in
└── security.psl.in

1 directory, 6 files


```


# 9
./cross-build.sh 

[it is works](0-report-run-first-kos.md)

# 8
go to 

/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/examples:

copy hello

 


# 7
sudo dpkg -i KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166_ru.deb
Selecting previously unselected package kasperskyos-community-edition-raspberrypi4b.
(Reading database ... 379787 files and directories currently installed.)
Preparing to unpack KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166_ru.deb ...
Unpacking kasperskyos-community-edition-raspberrypi4b (1.3.0.166) ...
Setting up kasperskyos-community-edition-raspberrypi4b (1.3.0.166) ...
Done

yes!



# 6

sudo dpkg -i libncurses5_6.2-0ubuntu2.1_amd64.deb 
Selecting previously unselected package libncurses5:amd64.
(Reading database ... 379778 files and directories currently installed.)
Preparing to unpack libncurses5_6.2-0ubuntu2.1_amd64.deb ...
Unpacking libncurses5:amd64 (6.2-0ubuntu2.1) ...
dpkg: dependency problems prevent configuration of libncurses5:amd64:
 libncurses5:amd64 depends on libtinfo5 (= 6.2-0ubuntu2.1); however:
  Version of libtinfo5:amd64 on system is 6.3-2ubuntu0.1.
  
---

remove libtinfo5
go
http://archive.ubuntu.com/ubuntu/pool/universe/n/ncurses/
wget proper version
install



# 5
** problem:
lsb_release -a
No LSB modules are available.
Distributor ID:	Ubuntu
Description:	Ubuntu 24.04.1 LTS
Release:	24.04
Codename:	noble

sudo apt install libncurses5

Reading package lists... Done
Building dependency tree... Done
Reading state information... Done
E: Unable to locate package libncurses5

## solution

go
http://archive.ubuntu.com/ubuntu/pool/universe/n/ncurses/
wget
install



# 4
## problem:
lsb_release -a
No LSB modules are available.
Distributor ID:	Ubuntu
Description:	Ubuntu 24.04.1 LTS
Release:	24.04
Codename:	noble

sudo apt install libtinfo5

Reading package lists... Done
Building dependency tree... Done
Reading state information... Done
E: Unable to locate package libtinfo5

## solution
sudo apt update
wget http://security.ubuntu.com/ubuntu/pool/universe/n/ncurses/libtinfo5_6.3-2ubuntu0.1_amd64.deb
sudo apt install ./libtinfo5_6.3-2ubuntu0.1_amd64.deb


# 3

sudo apt install python-is-python3
sudo apt install u-boot-tools 
sudo apt install socat
sudo apt install mtools
sudo apt install gcc-aarch64-linux-gnu
sudo apt install fdisk 

# 2


pkg: dependency problems prevent configuration of kasperskyos-community-edition-raspberrypi4b:
 kasperskyos-community-edition-raspberrypi4b depends on libncurses5 (>= 5.5-5~); however:
  Package libncurses5 is not installed.
 kasperskyos-community-edition-raspberrypi4b depends on libtinfo5; however:
  Package libtinfo5 is not installed.
 kasperskyos-community-edition-raspberrypi4b depends on socat (>= 1.7); however:
  Package socat is not installed.
 kasperskyos-community-edition-raspberrypi4b depends on device-tree-compiler; however:
  Package device-tree-compiler is not installed.
 kasperskyos-community-edition-raspberrypi4b depends on fdisk; however:
  Package fdisk is not installed.
 kasperskyos-community-edition-raspberrypi4b depends on gcc-aarch64-linux-gnu; however:
  Package gcc-aarch64-linux-gnu is not installed.
 kasperskyos-community-edition-raspberrypi4b depends on mtools; however:
  Package mtools is not installed.
 kasperskyos-community-edition-raspberrypi4b depends on python-is-python3; however:
  Package python-is-python3 is not installed.
 kasperskyos-community-edition-raspberrypi4b depends on u-boot-tools; however:
  Package u-boot-tools is not installed.




# 1

dpkg -i KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166_ru.deb



# Start

https://os.kaspersky.ru/download-community-edition/
KasperskyOS Community Edition 1.3.0

