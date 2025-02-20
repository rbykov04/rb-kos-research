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

add_subdirectory (hello)
add_subdirectory (einit)
