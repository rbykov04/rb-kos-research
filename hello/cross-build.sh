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

CMAKE_TARGETS="sim"

function PrintHelp () {
    cat<<HELP
${SCRIPT_NAME} [-h | --help]
${SCRIPT_NAME} [-l | --list]
${SCRIPT_NAME} [-b | --build <path>] [-s | --sdk-path <path to SDK>] [-t | --target <targets>]
This script builds a KasperskyOS-based solution image for each target defined in the CMAKE_TARGETS variable.
The built image will be run if there is the target named sim in the CMAKE_TARGETS variable.


-h, --help              Help text.
-l, --list              List of all available CMake targets.
-t, --target <targets>  List of CMake targets that you want to build. Enclose list items in quotation marks and separate them with a space character. If not specified, this targets will be built: "${CMAKE_TARGETS}"
-s, --sdk-path <path>   Path to the installed KasperskyOS SDK. If not specified, this value will be used: "${SDK_PREFIX}"
-b, --build <path>      Path to the generated CMake build directory. If not specified, this value will be used: "${BUILD}"
HELP
}

function ParseArguments () {
    while [ -n "${1}" ]; do
        case "${1}" in
        -h | --help)
            PrintHelp
            exit 0;;
        -l | --list)
            _CMAKE_TARGETS="help"
            break;;
        -t | --target)
            _CMAKE_TARGETS="${2}"
            shift;;
        -s | --sdk-path)
            _SDK_PREFIX=$2
            shift;;
        -b | --build)
            _BUILD=$2
            shift;;
        *) echo "Unknown option passed: '${1}'"
            exit 1;;
        esac
        shift
    done

    if [ "${_SDK_PREFIX}" ]; then
        export SDK_PREFIX=$_SDK_PREFIX
    fi
    if [ "${_CMAKE_TARGETS}" ]; then
        CMAKE_TARGETS=$_CMAKE_TARGETS
    fi
    if [ "${_BUILD}" ]; then
        BUILD=$_BUILD
    fi
}

ParseArguments "$@"

"$SDK_PREFIX/toolchain/bin/cmake" -G "Unix Makefiles" -B "$BUILD" \
      --no-warn-unused-cli \
      -D CMAKE_BUILD_TYPE:STRING=Debug \
      -D CMAKE_INSTALL_PREFIX:STRING="$INSTALL_PREFIX" \
      -D CMAKE_FIND_ROOT_PATH="$([[ -f "$SCRIPT_DIR/additional_cmake_find_root.txt" ]] && cat "$SCRIPT_DIR/additional_cmake_find_root.txt")$PREFIX_DIR/sysroot-$TARGET" \
      -D BOARD="$BOARD" \
      -D CMAKE_TOOLCHAIN_FILE="$SDK_PREFIX/toolchain/share/toolchain-$TARGET-clang.cmake" \
      "$SCRIPT_DIR/" && "$SDK_PREFIX/toolchain/bin/cmake" --build "$BUILD" --target $CMAKE_TARGETS
