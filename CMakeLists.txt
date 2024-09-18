
if (POLICY CMP0048)
    cmake_policy(SET CMP0048 NEW)
endif (POLICY CMP0048)
cmake_minimum_required(VERSION 3.16)

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
# include(cmake/feat-use-lld.cmake)

## ~ project components
cosmo_import_artifact()
add_subdirectory(cmd)
