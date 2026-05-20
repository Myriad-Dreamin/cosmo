
add_executable(cosmoc cosmoc/main.cc)
target_include_directories(cosmoc PRIVATE ${PROJECT_SOURCE_DIR})
target_link_libraries(cosmoc PUBLIC cosmo_std)

if (COSMO_ENABLE_CLANG_SYS)
    target_compile_definitions(cosmoc PUBLIC COSMO_ENABLE_CLANG_SYS=1)
    target_link_libraries(cosmoc PUBLIC cosmoClang)
endif ()
