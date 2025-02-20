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

