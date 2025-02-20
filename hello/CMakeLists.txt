# © 2024 AO Kaspersky Lab

cmake_minimum_required (VERSION 3.25)

project (hello)

# Initialize the CMake library for the KasperskyOS SDK.
include (platform)
# Statically link executable files.
initialize_platform (FORCE_STATIC)

# Include the CMake library named nk for working with the NK compiler (nk-gen-c).
include (platform/nk)

# Include the CMake library named doxygen for documentation of source texts.
include (platform/doxygen)
add_project_documentation_main_target ()

set (RESOURCES ${CMAKE_SOURCE_DIR}/resources)
set (NK_RESOURCES ${RESOURCES}/xdl)
nk_include_directories (${NK_RESOURCES})


# Set the linker and compiler flags.
project_header_default ("STANDARD_GNU_11:YES" "STRICT_WARNINGS:NO")

# Generate the *.edl.h file using the NK compiler for the Hello program.
nk_build_edl_files (hello_edl_files NK_MODULE "hello" EDL "Hello.edl")

add_executable (Hello "hello.c")
add_dependencies (Hello hello_edl_files)

# Include the CMake library named nk for working with the NK compiler (nk-gen-c).
include (platform/image)
include (${KL_SDK_ROOT_PATH}/common/build-sd-image.cmake)

# Set the linker and compiler flags.
project_header_default ("STANDARD_GNU_11:YES" "STRICT_WARNINGS:NO")

# Define the SECURITY_PSL_FILE variable.
# Variable value: path to the template file of the security.psl.in solution security policy description.
set (SECURITY_PSL_FILE    "security.psl.in")

# Define the CONNECTIONS_CFG_FILE variable.
# Variable value: path to the template file of the init.yaml.in solution initialization description.
set (CONNECTIONS_CFG_FILE "init.yaml.in")

# Define the QEMU_FLAGS variable.
# Variable value: additional flags for running QEMU.
set (QEMU_FLAGS           "-nographic -monitor none -nic none")

# Define the ENTITIES variable with a list of programs.
set (ENTITIES Hello)

# Create a top-level CMake target named kos-image which
# can be used when building a solution image to be started on the hardware platform.
build_kos_hw_image (kos-image
                    EINIT_ENTITY EinitHw
                    CONNECTIONS_CFG ${CONNECTIONS_CFG_FILE}
                    SECURITY_PSL ${SECURITY_PSL_FILE}
                    ${ENTITIES})

# Create a top-level CMake target named sd-image which
# can be used when building an SD-card image to be started on the hardware platform.
build_sd_image (sd-image
                KOS_IMAGE_TARGET kos-image)

# Create a top-level CMake target named kos-qemu-image which
# can be used when building a solution image to be started in QEMU.
build_kos_qemu_image (kos-qemu-image
                      EINIT_ENTITY EinitQemu
                      QEMU_FLAGS ${QEMU_FLAGS}
                      CONNECTIONS_CFG ${CONNECTIONS_CFG_FILE}
                      SECURITY_PSL ${SECURITY_PSL_FILE}
                      ${ENTITIES})
