#!/bin/bash

BUILD=build

export LANG=C
export TARGET="aarch64-kos"
export BOARD="RPI4_BCM2711"
export SDK_PREFIX="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"
export PATH="$SDK_PREFIX/toolchain/bin:$PATH"

# configure
cmake -B "$BUILD" \
      -D BOARD="$BOARD" \
      -D CMAKE_TOOLCHAIN_FILE="$SDK_PREFIX/toolchain/share/toolchain-$TARGET-clang.cmake"

# build
cmake --build "$BUILD" --target kos-qemu-image

# run qemu
cmake --build "$BUILD" --target sim
