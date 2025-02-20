# © 2024 AO Kaspersky Lab

project (hello)

# Set the linker and compiler flags.
project_header_default ("STANDARD_GNU_11:YES" "STRICT_WARNINGS:NO")

# Generate the *.edl.h file using the NK compiler for the Hello program.
nk_build_edl_files (hello_edl_files NK_MODULE "hello" EDL "${NK_RESOURCES}/hello/Hello.edl")

add_executable (Hello "src/hello.c")
add_dependencies (Hello hello_edl_files)
