set(COSMO_LLVM_HAS_CMAKE_PACKAGES OFF)
if (EXISTS "${COSMO_LLVM_PATH}/lib/cmake/llvm/LLVMConfig.cmake"
        AND EXISTS "${COSMO_LLVM_PATH}/lib/cmake/clang/ClangConfig.cmake")
    set(COSMO_LLVM_HAS_CMAKE_PACKAGES ON)
    find_package(LLVM CONFIG REQUIRED
        PATHS "${COSMO_LLVM_PATH}/lib/cmake/llvm"
        NO_DEFAULT_PATH)
    find_package(Clang CONFIG REQUIRED
        PATHS "${COSMO_LLVM_PATH}/lib/cmake/clang"
        NO_DEFAULT_PATH)
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
        ${COSMO_LLVM_PATH}/include
        ${LLVM_INCLUDE_DIRS}
        ${CLANG_INCLUDE_DIRS})

if (LLVM_DEFINITIONS)
    target_compile_options(cosmoClang PRIVATE ${LLVM_DEFINITIONS})
endif ()

if (TARGET libclang)
    target_link_libraries(cosmoClang PUBLIC libclang)
elseif (TARGET clang-cpp)
    target_link_libraries(cosmoClang PUBLIC clang-cpp)
else ()
    unset(COSMO_CLANG_C_LIBRARY CACHE)
    find_library(COSMO_CLANG_C_LIBRARY
        NAMES clang libclang
        PATHS
            ${COSMO_LLVM_PATH}/lib
            $ENV{COSMO_LLVM_PATH}/lib
        NO_DEFAULT_PATH)
    if (COSMO_CLANG_C_LIBRARY)
        target_link_libraries(cosmoClang PUBLIC ${COSMO_CLANG_C_LIBRARY})
    else ()
        file(GLOB COSMO_CLANG_STATIC_LIBRARIES CONFIGURE_DEPENDS
            "${COSMO_LLVM_PATH}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}clang[a-zA-Z]*${CMAKE_STATIC_LIBRARY_SUFFIX}")
        file(GLOB COSMO_LLVM_STATIC_LIBRARIES CONFIGURE_DEPENDS
            "${COSMO_LLVM_PATH}/lib/${CMAKE_STATIC_LIBRARY_PREFIX}LLVM[a-zA-Z]*${CMAKE_STATIC_LIBRARY_SUFFIX}")
        if (NOT COSMO_CLANG_STATIC_LIBRARIES OR NOT COSMO_LLVM_STATIC_LIBRARIES)
            message(FATAL_ERROR
                "cosmo-clang-sys requires Clang/LLVM static libraries under ${COSMO_LLVM_PATH}/lib. "
                "Set COSMO_LLVM_PATH to an LLVM/Clang install prefix, or leave it empty so CMake downloads the manifest LLVM into target/cosmo/llvm.")
        endif ()

        find_package(Threads QUIET)
        set(COSMO_CLANG_SYSTEM_LIBRARIES)
        if (Threads_FOUND)
            list(APPEND COSMO_CLANG_SYSTEM_LIBRARIES Threads::Threads)
        endif ()
        if (CMAKE_DL_LIBS)
            list(APPEND COSMO_CLANG_SYSTEM_LIBRARIES ${CMAKE_DL_LIBS})
        endif ()
        foreach (COSMO_CLANG_SYSTEM_LIBRARY_NAME IN ITEMS z zstd m)
            string(TOUPPER "${COSMO_CLANG_SYSTEM_LIBRARY_NAME}" COSMO_CLANG_SYSTEM_LIBRARY_KEY)
            find_library(COSMO_CLANG_${COSMO_CLANG_SYSTEM_LIBRARY_KEY}_LIBRARY
                NAMES ${COSMO_CLANG_SYSTEM_LIBRARY_NAME})
            if (COSMO_CLANG_${COSMO_CLANG_SYSTEM_LIBRARY_KEY}_LIBRARY)
                list(APPEND COSMO_CLANG_SYSTEM_LIBRARIES
                    ${COSMO_CLANG_${COSMO_CLANG_SYSTEM_LIBRARY_KEY}_LIBRARY})
            endif ()
        endforeach ()

        target_compile_definitions(cosmoClang PUBLIC CLANG_BUILD_STATIC=1)
        if (UNIX AND NOT APPLE)
            target_link_libraries(cosmoClang PUBLIC
                -Wl,--start-group
                ${COSMO_CLANG_STATIC_LIBRARIES}
                ${COSMO_LLVM_STATIC_LIBRARIES}
                -Wl,--end-group
                ${COSMO_CLANG_SYSTEM_LIBRARIES})
        else ()
            target_link_libraries(cosmoClang PUBLIC
                ${COSMO_CLANG_STATIC_LIBRARIES}
                ${COSMO_LLVM_STATIC_LIBRARIES}
                ${COSMO_CLANG_SYSTEM_LIBRARIES})
        endif ()
    endif ()
endif ()
