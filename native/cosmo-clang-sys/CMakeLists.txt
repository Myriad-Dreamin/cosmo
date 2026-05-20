find_package(LLVM CONFIG QUIET)
if (NOT LLVM_FOUND)
    message(FATAL_ERROR
        "cosmo-clang-sys requires LLVM CMake packages. Set COSMO_LLVM_PATH to an LLVM/Clang install prefix or pass -DCOSMO_LLVM_PATH=<prefix>.")
endif ()

find_package(Clang CONFIG QUIET)
if (NOT Clang_FOUND)
    message(FATAL_ERROR
        "cosmo-clang-sys requires Clang CMake packages. Set COSMO_LLVM_PATH to an LLVM/Clang install prefix or pass -DCOSMO_LLVM_PATH=<prefix>.")
endif ()

add_library(cosmoClang STATIC src/cosmo_clang_sys.cc)
add_library(cosmo::clang ALIAS cosmoClang)

set_target_properties(cosmoClang PROPERTIES
    OUTPUT_NAME cosmoClang
    POSITION_INDEPENDENT_CODE ON)

target_include_directories(cosmoClang
    PUBLIC
        ${CMAKE_CURRENT_SOURCE_DIR}/include
    PRIVATE
        ${LLVM_INCLUDE_DIRS}
        ${CLANG_INCLUDE_DIRS})

target_compile_options(cosmoClang PRIVATE ${LLVM_DEFINITIONS})

if (TARGET libclang)
    target_link_libraries(cosmoClang PUBLIC libclang)
elseif (TARGET clang-cpp)
    target_link_libraries(cosmoClang PUBLIC clang-cpp)
else ()
    find_library(COSMO_CLANG_C_LIBRARY
        NAMES clang libclang
        HINTS
            ${COSMO_LLVM_PATH}/lib
            $ENV{COSMO_LLVM_PATH}/lib)
    if (NOT COSMO_CLANG_C_LIBRARY)
        message(FATAL_ERROR
            "cosmo-clang-sys requires libclang. Set COSMO_LLVM_PATH to an LLVM/Clang install prefix or pass -DCOSMO_LLVM_PATH=<prefix>.")
    endif ()
    target_link_libraries(cosmoClang PUBLIC ${COSMO_CLANG_C_LIBRARY})
endif ()
