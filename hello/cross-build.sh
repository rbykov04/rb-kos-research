#!/bin/bash
# © 2024 AO Kaspersky Lab

SCRIPT_DIR="$(dirname "$(realpath "${0}")")"
SCRIPT_NAME="$(basename "$(realpath "${0}")")"
BUILD=${SCRIPT_DIR}/build

export LANG=C
# Target architecture.
# Change this value to the desired architecture if required.
export TARGET="aarch64-kos"
export BOARD="RPI4_BCM2711"
export PKG_CONFIG=""
export SDK_PREFIX="/opt/KasperskyOS-Community-Edition-RaspberryPi4b-1.3.0.166"
export INSTALL_PREFIX="$BUILD/../install"
export PATH="$SDK_PREFIX/toolchain/bin:$PATH"

cmake -G "Unix Makefiles" -B "$BUILD" \
      --no-warn-unused-cli \
      -D BOARD="$BOARD" \
      -D CMAKE_TOOLCHAIN_FILE="$SDK_PREFIX/toolchain/share/toolchain-$TARGET-clang.cmake"

cmake --build "$BUILD" --target sim
