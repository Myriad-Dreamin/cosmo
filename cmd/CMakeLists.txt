
add_executable(cosmoc_placeholder cosmoc/main.cc)
target_include_directories(cosmoc_placeholder PRIVATE ${PROJECT_SOURCE_DIR})
target_link_libraries(cosmoc_placeholder PUBLIC cosmo_std)
