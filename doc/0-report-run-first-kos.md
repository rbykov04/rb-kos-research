
# Commands
```
cd hello
./cross-build.sh
```

# Output
```
<-- skip build log-->
[100%] Running QEMU with KasperskyOS image...
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
[HAL  ] cpu #0 - 1035MHz
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
[HAL  ] cpu #3 - 1047MHz
[HAL  ] cpu #1 - 1044MHz
[HAL  ] cpu #2 - 1041MHz
[SMP  ] CPU #3 - boostrapped.
[SMP  ] CPU #2 - boostrapped.
[SMP  ] CPU #1 - boostrapped.
[TASK ] Multitasking infrastructure initialized.
[TIME ] Time infrastructure initialized.
[ROFS ] Files: 3, size: 1974272 (0x001e2000).
[ROFS ] File #00: einit            - size:   943744 (0x000e6680)
[ROFS ] File #01: Hello            - size:   939704 (0x000e56b8)
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
[2025-02-20T07:19:55.072][Info][Einit][11:11][CRT0] VFS filesystem and network backends initialized with stub (related calls will return EIO)
[2025-02-20T07:19:55.163][Info][hello.Hello][12:12][CRT0] VFS filesystem and network backends initialized with stub (related calls will return EIO)
Hello world!
```

Then kill qemu
