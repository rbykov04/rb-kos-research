# 41
Plan:
1. Introduce Echo.idl  - DONE
2. Create Echo.idl.c file for use IPC with IDL -DONE
3. Upgrade Server
4. Upgrade Client

Ok. Let's upgrade server

Done:
```
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <coresrv/nk/transport-kos.h>
#include <coresrv/sl/sl_api.h>

#include "build/Server.edl.h"

static nk_err_t echo_impl(struct Echo                 *self,
                          const struct Echo_Echo_req  *req,
                          const struct nk_arena       *req_arena,
                          struct Echo_Echo_res        *res,
                          struct nk_arena             *res_arena)
{
    return NK_EOK;
}

int main(void)
{
    ServiceId iid;
    Handle handle = ServiceLocatorRegister("server_connection", NULL, 0, &iid);

    NkKosTransport transport;
    NkKosTransport_Init(&transport, handle, NK_NULL, 0);


    Server_entity_req req;
    char req_buffer[Server_entity_req_arena_size];
    struct nk_arena req_arena = NK_ARENA_INITIALIZER(req_buffer,
                                        req_buffer + sizeof(req_buffer));

    Server_entity_res res;
    char res_buffer[Server_entity_res_arena_size];
    struct nk_arena res_arena = NK_ARENA_INITIALIZER(res_buffer,
                                        res_buffer + sizeof(res_buffer));


    static const struct Echo_ops ops = {.Echo = echo_impl};
    struct Echo impl = {&ops};

    Server_entity entity;
    Server_entity_init(&entity, &impl);

    fprintf(stderr, "Hello I'm server\n");

    //do
    {
        /* Reset the buffer containing the request and response. */
        nk_req_reset(&req);
        nk_arena_reset(&req_arena);
        nk_arena_reset(&res_arena);

        /* Wait for a request for the server program. */
        if (nk_transport_recv(&transport.base, &req.base_, &req_arena) != NK_EOK) {
            fprintf(stderr, "nk_transport_recv error\n");
        } else {
            Server_entity_dispatch(&entity, &req.base_, &req_arena,
                                        &res.base_, &res_arena);
        }

        /* Send response. */
        if (nk_transport_reply(&transport.base,
                               &res.base_,
                               &res_arena) != NK_EOK) {
            fprintf(stderr, "nk_transport_reply error\n");
        }
    }
    //while (true);

    return EXIT_SUCCESS;
}
```

But:

```
Hello I'm server
nk_transport_recv error
nk_transport_reply error
```

Les's check
```
Handle handle = ServiceLocatorRegister("server_connection", NULL, 0, &iid);
assert(handle != INVALID_HANDLE);
```

And yes:

```
assertion "handle != INVALID_HANDLE" failed: file "server.c", line 24, function: main

==============================================================================

Unhandled Common Exception
ESR = 0xf2000001
EC = 0x3c
Exception class: BRK instruction execution in AArch64 state
    EIID : Server
    Name : Server
    Path : Server
    TID  : 12
    pc   : 0x19d517524
    CPU  : 00


```

It means: We need set up our system a little bit to register server_connections

# 40
Plan:
1. Introduce Echo.idl  - DONE
2. Create Echo.idl.c file for use IPC with IDL
3. Upgrade Server
4. Upgrade Client

We need to create Echo.idl.c. It is a helper for us to use IDL inside out Server.c

Let's do.

We need compiler from idl to c.
Let's watch some in SDK

```

ls -la /opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/bin | grep nk
-rwxr-xr-x  1 root root   200608 янв 29 12:13 clang-linker-wrapper
lrwxrwxrwx  1 root root        3 янв 29 11:20 lld-link -> lld
-rwxr-xr-x  1 root root 29354704 янв 29 11:34 nk-driver
-rwxr-xr-x  1 root root 25725520 янв 29 11:34 nk-gen-c  <---------- This one
-rwxr-xr-x  1 root root 31527888 янв 29 11:34 nk-gen-syzlang
-rwxr-xr-x  1 root root   605632 янв 29 12:13 nkppmeta
-rwxr-xr-x  1 root root 51029392 янв 29 11:34 nk-psl-gen-c
-rwxr-xr-x  1 root root 37141840 янв 29 11:34 nk-psl-gen-fuzzer

```

```
/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/toolchain/bin/nk-gen-c    
Missing: <xdl-file>

Usage: nk-gen-c <xdl-file> [-I <search-dir>] [-o <output-dir>] 
                [--types | --interface | --endpoints | --component-refl | 
                  --client | --server | --tests] 
                [--trace-client-ipc <feature>[,<feature>...]] 
                [--trace-server-ipc <feature>[,<feature>...]] 
                [--ipc-trace-method-filter <method>[,<method>...]] 
                [--deprecated-no-extended-errors] [--extended-errors] [-W FLAGS]
                [--log-level <level>]

  Generate C stubs for a given IDL/CDL/EDL file
```

Eeeee. Let's play
```
ls -la build | grep Server
-rwxrwxr-x 1 rbykov rbykov  938216 мар 13 14:45 Server
-rw-rw-r-- 1 rbykov rbykov   11840 мар 13 15:37 Server.edl.h
-rw-rw-r-- 1 rbykov rbykov     268 мар 13 15:37 Server.edl.nk.d
```



# 39
Let's introduce some method for server.

Client ---(Echo msg)-> Server
Server -> print msg to stderr.

To make this wi will use IPC.

Plan:
1. Introduce Echo.idl 
2. Create Echo.idl.c file for use IPC with IDL
3. Upgrade Server
4. Upgrade Client

Ok
```
cat Echo.idl 
package Echo

interface {
    Echo(in string<256> value);
}
```

Then we add this to Server.edl (To exaplain ksm module we will use this in server)
```
cat Server.edl
task class Server

endpoints {
    main : Echo
}
```

We can even see what new will be in security.psl.c

At least this:
```
      case 2:
        {
            switch (iid) {
                
              case 0:
                {
                    *endpoint_name = "main";
                    *interface_name = "Echo";
                    switch (mid) {
                        
                      case 0:
                        {
                            *method_name = "Echo";
                            break;
                        }
                    }
                    break;
                }
            }
            break;
        }
```


# 38
The more source files the more bin files we create

Le't introduce build dir and will create bin files there

Done:
tree  
.
├── build
│   ├── Client
│   ├── Init
│   ├── kos-qemu-image
│   ├── kos-qemu-image.bin
│   ├── kos-qemu-image.dtb
│   ├── kos-qemu-image.map
│   ├── kos-qemu-image.romfs
│   ├── kos-qemu-image.stripped
│   ├── ksm.module
│   ├── security.psl.c
│   └── Server
├── client.c
├── Client.edl
├── Einit.edl
├── init.c
├── Makefile
├── security.psl
├── server.c
└── Server.edl



# 37
Let's upgrade example hello and add another process.

```
 ~/dev/github/rb-kos-research/echo   main  ls -la
total 40
drwxrwxr-x 2 rbykov rbykov 4096 мар 13 14:13 .
drwxrwxr-x 8 rbykov rbykov 4096 мар 13 13:53 ..
-rw-rw-r-- 1 rbykov rbykov  122 мар 13 13:58 client.c
-rw-rw-r-- 1 rbykov rbykov   18 мар 13 14:01 Client.edl
-rw-rw-r-- 1 rbykov rbykov   13 фев 22 12:16 Einit.edl
-rw-rw-r-- 1 rbykov rbykov 1590 мар 13 14:09 init.c
-rw-rw-r-- 1 rbykov rbykov 2840 мар 13 14:00 Makefile
-rw-rw-r-- 1 rbykov rbykov  303 мар 13 14:02 security.psl
-rw-rw-r-- 1 rbykov rbykov  122 мар 13 13:58 server.c
-rw-rw-r-- 1 rbykov rbykov   18 мар 13 14:02 Server.edl

```

# 36
Progress:
I.   Separate build two step - DONE
II.  Replace ghc to microhs  - DON
III. Build to kos
IV.  Run :)

Let's build to kos

This is not so hard.


```
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
[2025-02-23T15:16:20.069][Info][Einit][11:11][CRT0] VFS filesystem and network backends initialized with stub (related calls will return EIO)
[2025-02-23T15:16:20.166][Info][Hello][12:12][CRT0] VFS filesystem and network backends initialized with stub (related calls will return EIO)
Hello world form Agda! 

[INIT ] System worker finished
[INIT ] System halted...
System halted
```

# 35
Continue to run agda with microhs

## Problem 1
```
module MAlonzo.RTE where
...

-- Support for musical coinduction.

data Inf                   a = Sharp { flat :: a }
type Infinity (level :: *) a = Inf a
```

```
MHSDIR=MicroHs MicroHs/bin/gmhs \
	MAlonzo.Code.AgdaHello \
	-iMicroHs/lib  \
	-oHello
gmhs: "./MAlonzo/RTE.hs": line 113, col 25:
  found:    *
  expected: forall ∀ LQIdent ( UQIdent [ literal
```

I have a few questions:
- What is "musical coinduction?"
- What does (level :: *) mean?
- How does fix this?



## Problem 2
```

module MAlonzo.RTE where
...
import qualified GHC.Exts as GHC (Any)

```

```
gmhs: "./MAlonzo/RTE.hs": line 6, col 18: Module not found: GHC.Exts
search path=[".","MicroHs/lib","MicroHs/lib"]
package path=[]
CallStack (from HasCallStack):
  error, called at src/MicroHs/Expr.hs:738:24 in main:MicroHs.Expr
  errorMessage, called at src/MicroHs/Compile.hs:415:7 in main:MicroHs.Compile
```

Ok. I did this.
I build this with microHs.

Thera are 3 problems:
```
--FIXME: Problem 1
--import qualified GHC.Exts as GHC (Any)
data Any

--FIXME: Problem 2 what is generalCategory
--natToChar :: Integer -> Char
--natToChar n | generalCategory c == Surrogate = '\xFFFD'
--            | otherwise                      = c
--  where c = toEnum $ fromIntegral $ mod n 0x110000

--FIXME: Problem 3 what to do with * vs forall ?
--type Infinity (level :: *) a = Inf a
```

Let's leave this for future


# 34
Progress:
I.   Separate build two step - DONE
II.  Replace ghc to microhs 
III. Build to kos
IV.  Run :)

Let's use mhs.

I have achived this but. But I have error of compiling which requere some effort from me



# 33
OK. 
1. We have agda-hello.agda
2. We can compile to haskell and build to binary with agda --compile 

Now we have a plan A:

I.   Separate build two step -> 
        1.  agda    -> haskell (with agda compiler)
        1.  haskell -> binary  (with ghc compiler)
II.  Replace ghc to microhs 
III. Build to kos
IV.  Run :)

And plan B:
We can haskell to agda project

Let's try plan A

Ok. the firts step it is not so hard:

```
MAlonzo: AgdaHello.agda
	agda  -i. --ghc-dont-call-ghc --compile --library=standard-library-2.2 AgdaHello.agda
```

It prints logs:

```
agda  -i. --ghc-dont-call-ghc --compile --library=standard-library-2.2 AgdaHello.agda
Compiling Agda.Primitive in /usr/share/libghc-agda-dev/lib/prim/Agda/Primitive.agdai to ~/github/rb-kos-research/agda-hello/MAlonzo/Code/Agda/Primitive.hs
<---CUT--->
NOT calling: ghc -O -o ~/dev/github/rb-kos-research/agda-hello/AgdaHello -Werror -i~/github/rb-kos-research/agda-hello -main-is MAlonzo.Code.AgdaHello ~/github/rb-kos-research/agda-hello/MAlonzo/Code/AgdaHello.hs --make -fwarn-incomplete-patterns

```
Just copy last line

```
Hello: MAlonzo
	ghc -O -o Hello \
		-Werror -i. \
		-main-is MAlonzo.Code.AgdaHello \
		MAlonzo/Code/AgdaHello.hs \
		--make -fwarn-incomplete-patterns


```

# 32
Ok. Let's start play with agda.
https://agda.readthedocs.io/en/v2.6.0.1/getting-started/hello-world.html
```
 ~/dev/github/rb-kos-research/agda-hello   main ●  cat Hello.agda
module Hello where

open import IO

main = run (putStrLn "Hello, World from agda!")

```
```
agda --compile Hello.agda
Checking Hello (~/github/rb-kos-research/agda-hello/Hello.agda).
~/github/rb-kos-research/agda-hello/Hello.agda:3,1-15
Failed to find source of module IO in any of the following
locations:
  ~/github/rb-kos-research/agda-hello/IO.agda
  ~github/rb-kos-research/agda-hello/IO.lagda
  /usr/share/libghc-agda-dev/lib/prim/IO.agda
  /usr/share/libghc-agda-dev/lib/prim/IO.lagda
when scope checking the declaration
  open import IO
make: *** [Makefile:4: Hello] Error 42
```


agda can't find its own stdlib. :D
```
 ~/dev/github/rb-kos-research/agda-hello   main ●  agda --print-agda-dir 
/usr/share/libghc-agda-dev
 ~/dev/github/rb-kos-research/agda-hello   main ●  ls -1 /usr/share | grep agda
agda-stdlib
libghc-agda-dev
```

I instaled stdlib in dev dir.
```
agda-hello: agda-hello.agda
	agda  -i.  --compile --library=standard-library-2.2 agda-hello.agda

```

And it works!

But
```
~/dev/github/rb-kos-research/agda-hello   main ●  tree
.
├── agda-hello
├── agda-hello.agda
├── agda-hello.agdai
├── _build
│   └── 2.6.3
│       └── agda
│           └── agda-hello.agdai
├── Makefile
└── MAlonzo
    ├── Code
    │   ├── Agda
    │   │   ├── Builtin
    │   │   │   ├── Bool.hs
    │   │   │   ├── Char.hs
    │   │   │   ├── IO.hi
    │   │   │   ├── IO.hs
    │   │   │   ├── IO.o
    │   │   │   ├── List.hs
    │   │   │   ├── Maybe.hs
    │   │   │   ├── Nat.hs
    │   │   │   ├── Sigma.hs
    │   │   │   ├── String.hi
    │   │   │   ├── String.hs
    │   │   │   ├── String.o
    │   │   │   ├── Unit.hi
    │   │   │   ├── Unit.hs
    │   │   │   └── Unit.o
    │   │   └── Primitive.hs
    │   ├── QagdaZ45Zhello.hi
    │   ├── QagdaZ45Zhello.hs
    │   └── QagdaZ45Zhello.o
    ├── RTE
    │   └── Float.hs
    ├── RTE.hi
    ├── RTE.hs
    └── RTE.o



```

We haven't build even two file haskell for MicroHs

# 31
I have a crazy idea.

1. We can compile haskell to c and run into KasperskyOS
1. We can complile agda to haskell.

What if...
What if we ...
What if we compile agda to haskell to compule to c and run KasperskyOS

Let's run simple research



# 31
We can build Haskell programm for KOS. Let's to run in KOS.

Ok. In KOS we have to print we have either connect with vfs or print stderr. Ok. Let's print to std err 

```

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
<----- CUT ----->
[ROFS ] Files: 3, size: 2195456 (0x00218000).
[ROFS ] File #00: einit            - size:   939664 (0x000e5690)
[ROFS ] File #01: Hello            - size:  1163960 (0x0011c2b8)
[ROFS ] File #02: ksm.module       - size:    78024 (0x000130c8)

<----- CUT ----->

Hello form haskell

[INIT ] System worker finished
[INIT ] System halted...
```

# 30
Progress:
```
RUNTIME=MicroHs/src/runtime
Hello: Hello.c
	clang \                         - TODO: replace with clang from SDK
		ffi.c \ 
		Hello.c \
		eval-kos-64.c \
		-static \
		-o Hello
```

Ok. Let's try use clang from SDK

Ok!
```
MHSDIR=MicroHs MicroHs/bin/gmhs -iMicroHs/lib Hello -oHello.comb
MicroHs/bin/mhseval +RTS -rHello.comb -oHello-opt.comb
./Addcombs Hello-opt.comb Hello.c
"""/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"/toolchain"/bin/aarch64-kos-clang" \
	ffi.c \
	Hello.c \
	eval-kos-64.c \
	-static \
	-o Hello
 ~/dev/github/rb-kos
```




# 29
Progress:
```
RUNTIME=MicroHs/src/runtime
Hello: Hello.c
	clang \                         - TODO: replace with clang from SDK
		ffi.c \ 
		Hello.c \
		eval-kos-64.c \
		-lm\                        - TODO: Use lib from SDK / or drop support of math (temprary)
		-static \
		-o Hello
```

Let's remove -lm.
Ok:)_
It worsk


# 28
Progress:
```
RUNTIME=MicroHs/src/runtime
Hello: Hello.c
	clang \                         - TODO: replace with clang from SDK
		ffi.c \ 
		Hello.c \
		${RUNTIME}/eval-unix-64.c \ - TODO: Research
		-lm\                        - TODO: Use lib from SDK / or drop support of math (temprary)
		-static \
		-o Hello
```

Let's research eval-unix-64.c. wj
```
 ~/dev/github/rb-kos-research/microhs   main ●  cat MicroHs/src/runtime/eval-unix-64.c 
/* Copyright 2023 Lennart Augustsson
 * See LICENSE file for full license.
 */
#include "config-unix-64.h"

#include "eval.c"
```

Research list of platforms

```
 ~/dev/github/rb-kos-research/microhs   main ●  ls -1 MicroHs/src/runtime/eval-*
MicroHs/src/runtime/eval-c64.c
MicroHs/src/runtime/eval-esp32.c
MicroHs/src/runtime/eval-micro-64.c
MicroHs/src/runtime/eval-mingw-64.c
MicroHs/src/runtime/eval-stm32f4.c
MicroHs/src/runtime/eval-unix-32.c
MicroHs/src/runtime/eval-unix-64.c
MicroHs/src/runtime/eval-windows-64.c
```

Ok. I prefer choose the smallest config in the begging.
```
cat MicroHs/src/runtime/eval-micro-64.c 
/* Copyright 2023 Lennart Augustsson
 * See LICENSE file for full license.
 */
#include "config-micro-64.h"

#include "eval.c"
```
Let's see config. (I will cut comment to minimaze content)
```
cat MicroHs/src/runtime/config-micro-64.h 
#define WANT_STDIO 0
#define WANT_FLOAT 0
#define WANT_MATH 0
#define WANT_MD5 0
#define WANT_TICK 0

/* * Process argc, argv */
#define WANT_ARGS 0
/* * Number of bits in a word.  Only 32 and 64 are supported. */
//#define WORD_SIZE 64

/* #define FFS ffsl */
/* #define PCOMMA "'" */
/* #define GETRAW */


/* #define GETTIMEMILLI */
/* #define ERR(s) */
/* #define ERR1(s,a) */

#define GCRED    0              /* do some reductions during GC */
#define FASTTAGS 0              /* compute tag by pointer subtraction */
#define INTTABLE 0              /* use fixed table of small INT nodes */
#define SANITY   0              /* do some sanity checks */
#define STACKOVL 0              /* check for stack overflow */
```

OK. Let's try build and run this config on ubuntu first.

Result
```
 ~/dev/github/rb-kos-research/microhs   main ●  cat config-kos-64.h 
#define WANT_STDIO 1
#if WANT_STDIO
#include <unistd.h>
#endif  /* WANT_STDIO */

#define WANT_FLOAT 0

#define WANT_MATH 0
#define WANT_MD5 0

#define WANT_TICK 1
#define WANT_ARGS 1

//#define WORD_SIZE 64

/* #define FFS ffsl */
/* #define PCOMMA "'" */
/* #define GETRAW */
/* #define GETTIMEMILLI */
/* #define ERR(s) */
/* #define ERR1(s,a) */

#define GCRED    0              /* do some reductions during GC */
#define FASTTAGS 0              /* compute tag by pointer subtraction */
#define INTTABLE 0              /* use fixed table of small INT nodes */
#define SANITY   0              /* do some sanity checks */
#define STACKOVL 0              /* check for stack overflow */
```

I enabled stdio + tick + args
```
In file included from eval-kos-64.c:3:
./MicroHs/src/runtime/eval.c:4608:45: error: call to undeclared function 'unlink'; ISO C99 and later do not support implicit function declarations [-Wimplicit-function-declaration]
 4608 | void mhs_unlink(int s) { mhs_from_Int(s, 1, unlink(mhs_to_Ptr(s, 0))); }
      |                                             ^
1 error generated.


```
I had problem with unlink and add this include
```
#if WANT_STDIO
#include <unistd.h>
#endif  /* WANT_STDIO */
```

# 27
Ok. We can build and run binary of haskell programm.

Let's try to build this to kos.

What do we have?

```
Hello: Hello.c
	clang \
		ffi.c \
		Hello.c \
		${RUNTIME}/eval-unix-64.c \
		-lm\
		-static \
		-o Hello
```

**clang** we can use clang from sdk.

**ffi.c** is simple:
```
 ~/dev/github/rb-kos-research/microhs   main ●  cat ffi.c 
#include "MicroHs/src/runtime/mhsffi.h"
static struct ffi_entry table[] = {
{ 0,0 }
};
struct ffi_entry *xffi_table = table;

```

**Hello.c** - I don't expect problem with this. Maybe we will have problem when we will run this.
(For Exxample: Memory in system is not enought). 
```
~/dev/github/rb-kos-research/microhs   main  cat Hello.c
static unsigned char combexprdata[] = {
32,64,64,64,67,39,32,67,39,32,66,32,85,32,64,67,32,85,32,75,
<--------------------CUT------------------------>
32,34,72,101,108,108,111,32,102,111,114,109,32,104,97,115,107,101,108,108,
34,32,64,64,64,125,
};
const unsigned char *combexpr = combexprdata;
const int combexprlen = 7386;
```



**-lm** - is not a problem
```
~/dev/github/rb-kos-research/microhs   main  ls /opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166/sysroot-aarch64-kos/lib | grep libm
libm.a
```


**${RUNTIME}/eval-unix-64.c** - ? I need research this.

Resume and plan
```
Hello: Hello.c
	clang \                         - TODO: replace with clang from SDK
		ffi.c \ 
		Hello.c \
		${RUNTIME}/eval-unix-64.c \ - TODO: Research
		-lm\                        - TODO: Use lib from SDK / or drop support of math (temprary)
		-static \
		-o Hello
```


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

