
if (POLICY CMP0048)
    cmake_policy(SET CMP0048 NEW)
endif (POLICY CMP0048)
cmake_minimum_required(VERSION 3.19)

project(cosmo
        VERSION 0.1.0
        DESCRIPTION "Cosmo Project"
        LANGUAGES C CXX)

# import cmake compilation features I
set(CMAKE_CXX_STANDARD 17 CACHE STRING "C++ standard to conform to")
set(CMAKE_CXX_STANDARD_REQUIRED YES)
set(CMAKE_CXX_EXTENSIONS NO)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# import cmake compilation features II
include(cmake/feat-exception.cmake)
include(cmake/feat-rtti.cmake)
include(cmake/Cosmo.cmake)
include(cmake/CosmoLLVM.cmake)
# include(cmake/feat-use-lld.cmake)

option(COSMO_ENABLE_CLANG_SYS "Build the cosmo-clang-sys C++ namespace query library" OFF)
set(COSMO_LLVM_PATH "$ENV{COSMO_LLVM_PATH}" CACHE PATH "LLVM/Clang install prefix for cosmo-clang-sys")
set(COSMO_LLVM_VERSION "21.1.8" CACHE STRING "LLVM/Clang version used by cosmo-clang-sys")
set(COSMO_LLVM_DOWNLOAD_DIR "${PROJECT_SOURCE_DIR}/target/cosmo/llvm" CACHE PATH "Directory for downloaded LLVM artifacts")
set(COSMO_LLVM_MANIFEST_PATH "${PROJECT_SOURCE_DIR}/config/llvm-manifest.json" CACHE FILEPATH "LLVM artifact manifest for cosmo-clang-sys")
option(COSMO_LLVM_ENABLE_LTO "Use the LTO LLVM artifact from config/llvm-manifest.json" OFF)
option(COSMO_LLVM_OFFLINE "Do not download LLVM; use COSMO_LLVM_PATH or an existing target cache" OFF)
set(COSMO_LLVM_TARGET_PLATFORM "" CACHE STRING "Override LLVM artifact platform: linux, macosx, or windows")
set(COSMO_LLVM_TARGET_ARCH "" CACHE STRING "Override LLVM artifact architecture: x64 or arm64")

if (COSMO_ENABLE_CLANG_SYS)
    cosmo_setup_llvm()
    add_subdirectory(native/cosmo-clang-sys)
endif ()

## ~ project components
cosmo_import_artifact()
add_subdirectory(cmd)
