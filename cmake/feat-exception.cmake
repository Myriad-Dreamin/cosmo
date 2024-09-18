
# Disable C++ exceptions.
message(STATUS "Disabling C++ exceptions")
if (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    if ("${CMAKE_CXX_FLAGS} " STREQUAL " ")
    else ()
        string(REGEX REPLACE "/EH[a-z]+" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
    endif ()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /EHsc")
else (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")
    if ("${CMAKE_CXX_FLAGS} " STREQUAL " ")
    else ()
        string(REGEX REPLACE "-fexceptions" "" CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
    endif ()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions")
endif (CMAKE_CXX_COMPILER_ID STREQUAL "MSVC")