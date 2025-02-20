cmake_minimum_required (VERSION 3.25)
project (hello)

include (platform)
initialize_platform (FORCE_STATIC) # -- ???
include (platform/nk)
include (platform/image)

nk_build_edl_files (hello_edl_files NK_MODULE "hello" EDL "Hello.edl")

add_executable (Hello "hello.c")
add_dependencies (Hello hello_edl_files)

set (ENTITIES Hello)

set (QEMU_FLAGS           "-nographic -monitor none -nic none")
build_kos_qemu_image (kos-qemu-image
                      EINIT_ENTITY EinitQemu
                      QEMU_FLAGS ${QEMU_FLAGS}
                      CONNECTIONS_CFG "init.yaml.in"
                      SECURITY_PSL "security.psl.in"
                      ${ENTITIES})
