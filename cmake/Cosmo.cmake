
function(cosmo_import_artifact)
    ## ~ cosmo generated code
    message(STATUS "${CMAKE_CURRENT_SOURCE_DIR}/target/cosmo/release")
    if(EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/target/cosmo/release")
        add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/target/cosmo/release")
    endif()
endfunction(cosmo_import_artifact)
