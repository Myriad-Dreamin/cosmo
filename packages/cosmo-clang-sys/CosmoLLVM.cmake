include_guard()

function(cosmo_llvm_bool_value VALUE OUTPUT_VARIABLE)
    set(COSMO_LLVM_BOOL_RESULT OFF)
    if ("${VALUE}")
        set(COSMO_LLVM_BOOL_RESULT ON)
    endif ()
    set(${OUTPUT_VARIABLE} "${COSMO_LLVM_BOOL_RESULT}" PARENT_SCOPE)
endfunction()

function(cosmo_llvm_normalize_platform OUTPUT_VARIABLE)
    if (COSMO_LLVM_TARGET_PLATFORM)
        string(TOLOWER "${COSMO_LLVM_TARGET_PLATFORM}" COSMO_LLVM_PLATFORM)
    elseif (CMAKE_SYSTEM_NAME STREQUAL "Linux")
        set(COSMO_LLVM_PLATFORM "linux")
    elseif (CMAKE_SYSTEM_NAME STREQUAL "Darwin")
        set(COSMO_LLVM_PLATFORM "macosx")
    elseif (CMAKE_SYSTEM_NAME STREQUAL "Windows")
        set(COSMO_LLVM_PLATFORM "windows")
    else ()
        message(FATAL_ERROR "Unsupported LLVM artifact platform: ${CMAKE_SYSTEM_NAME}")
    endif ()

    if (COSMO_LLVM_PLATFORM STREQUAL "darwin")
        set(COSMO_LLVM_PLATFORM "macosx")
    endif ()
    if (COSMO_LLVM_PLATFORM STREQUAL "win32")
        set(COSMO_LLVM_PLATFORM "windows")
    endif ()

    if (NOT COSMO_LLVM_PLATFORM MATCHES "^(linux|macosx|windows)$")
        message(FATAL_ERROR
            "Unsupported LLVM artifact platform: ${COSMO_LLVM_PLATFORM}. "
            "Expected linux, macosx, or windows.")
    endif ()

    set(${OUTPUT_VARIABLE} "${COSMO_LLVM_PLATFORM}" PARENT_SCOPE)
endfunction()

function(cosmo_llvm_normalize_arch OUTPUT_VARIABLE)
    if (COSMO_LLVM_TARGET_ARCH)
        set(COSMO_LLVM_ARCH_INPUT "${COSMO_LLVM_TARGET_ARCH}")
    else ()
        set(COSMO_LLVM_ARCH_INPUT "${CMAKE_SYSTEM_PROCESSOR}")
    endif ()

    string(TOLOWER "${COSMO_LLVM_ARCH_INPUT}" COSMO_LLVM_ARCH_INPUT)
    if (COSMO_LLVM_ARCH_INPUT MATCHES "^(x86_64|amd64|x64)$")
        set(COSMO_LLVM_ARCH "x64")
    elseif (COSMO_LLVM_ARCH_INPUT MATCHES "^(aarch64|arm64)$")
        set(COSMO_LLVM_ARCH "arm64")
    else ()
        message(FATAL_ERROR
            "Unsupported LLVM artifact architecture: ${COSMO_LLVM_ARCH_INPUT}. "
            "Expected x64 or arm64.")
    endif ()

    set(${OUTPUT_VARIABLE} "${COSMO_LLVM_ARCH}" PARENT_SCOPE)
endfunction()

function(cosmo_llvm_build_type OUTPUT_VARIABLE)
    set(COSMO_LLVM_SELECTED_BUILD_TYPE "${CMAKE_BUILD_TYPE}")
    if (NOT COSMO_LLVM_SELECTED_BUILD_TYPE)
        set(COSMO_LLVM_SELECTED_BUILD_TYPE "RelWithDebInfo")
    endif ()
    set(${OUTPUT_VARIABLE} "${COSMO_LLVM_SELECTED_BUILD_TYPE}" PARENT_SCOPE)
endfunction()

function(cosmo_llvm_artifact_stem FILENAME OUTPUT_VARIABLE)
    set(COSMO_LLVM_STEM "${FILENAME}")
    if (COSMO_LLVM_STEM MATCHES "\\.tar\\.xz$")
        string(REGEX REPLACE "\\.tar\\.xz$" "" COSMO_LLVM_STEM "${COSMO_LLVM_STEM}")
    elseif (COSMO_LLVM_STEM MATCHES "\\.tar\\.gz$")
        string(REGEX REPLACE "\\.tar\\.gz$" "" COSMO_LLVM_STEM "${COSMO_LLVM_STEM}")
    elseif (COSMO_LLVM_STEM MATCHES "\\.tar$")
        string(REGEX REPLACE "\\.tar$" "" COSMO_LLVM_STEM "${COSMO_LLVM_STEM}")
    endif ()
    set(${OUTPUT_VARIABLE} "${COSMO_LLVM_STEM}" PARENT_SCOPE)
endfunction()

function(cosmo_llvm_install_is_usable PREFIX OUTPUT_VARIABLE)
    set(COSMO_LLVM_USABLE OFF)
    if (IS_DIRECTORY "${PREFIX}"
            AND IS_DIRECTORY "${PREFIX}/include"
            AND IS_DIRECTORY "${PREFIX}/lib")
        set(COSMO_LLVM_USABLE ON)
    endif ()
    set(${OUTPUT_VARIABLE} "${COSMO_LLVM_USABLE}" PARENT_SCOPE)
endfunction()

function(cosmo_llvm_require_usable PREFIX CONTEXT)
    cosmo_llvm_install_is_usable("${PREFIX}" COSMO_LLVM_PREFIX_USABLE)
    if (COSMO_LLVM_PREFIX_USABLE)
        return()
    endif ()

    message(FATAL_ERROR
        "${CONTEXT} LLVM path is not usable: ${PREFIX}\n"
        "Expected include and lib directories.")
endfunction()

function(cosmo_llvm_pick_artifact VERSION BUILD_TYPE PLATFORM ARCH OUTPUT_FILENAME OUTPUT_SHA256)
    if (NOT COSMO_LLVM_MANIFEST_PATH)
        set(COSMO_LLVM_MANIFEST_PATH "${CMAKE_CURRENT_FUNCTION_LIST_DIR}/config/llvm-manifest.json")
    endif ()

    get_filename_component(COSMO_LLVM_MANIFEST_ABS "${COSMO_LLVM_MANIFEST_PATH}" ABSOLUTE)
    if (NOT EXISTS "${COSMO_LLVM_MANIFEST_ABS}")
        message(FATAL_ERROR "LLVM manifest not found: ${COSMO_LLVM_MANIFEST_ABS}")
    endif ()

    file(READ "${COSMO_LLVM_MANIFEST_ABS}" COSMO_LLVM_MANIFEST_JSON)
    string(JSON COSMO_LLVM_ARTIFACT_COUNT LENGTH "${COSMO_LLVM_MANIFEST_JSON}")
    if (COSMO_LLVM_ARTIFACT_COUNT LESS_EQUAL 0)
        message(FATAL_ERROR "LLVM manifest must be a non-empty array: ${COSMO_LLVM_MANIFEST_ABS}")
    endif ()

    cosmo_llvm_bool_value("${COSMO_LLVM_ENABLE_LTO}" COSMO_LLVM_WANT_LTO)
    math(EXPR COSMO_LLVM_LAST_INDEX "${COSMO_LLVM_ARTIFACT_COUNT} - 1")
    foreach (COSMO_LLVM_INDEX RANGE 0 ${COSMO_LLVM_LAST_INDEX})
        string(JSON COSMO_LLVM_ENTRY_VERSION GET "${COSMO_LLVM_MANIFEST_JSON}" ${COSMO_LLVM_INDEX} version)
        if (NOT "${COSMO_LLVM_ENTRY_VERSION}" STREQUAL "${VERSION}")
            continue()
        endif ()

        string(JSON COSMO_LLVM_ENTRY_PLATFORM GET "${COSMO_LLVM_MANIFEST_JSON}" ${COSMO_LLVM_INDEX} platform)
        string(TOLOWER "${COSMO_LLVM_ENTRY_PLATFORM}" COSMO_LLVM_ENTRY_PLATFORM)
        if (NOT "${COSMO_LLVM_ENTRY_PLATFORM}" STREQUAL "${PLATFORM}")
            continue()
        endif ()

        string(JSON COSMO_LLVM_ENTRY_ARCH GET "${COSMO_LLVM_MANIFEST_JSON}" ${COSMO_LLVM_INDEX} arch)
        string(TOLOWER "${COSMO_LLVM_ENTRY_ARCH}" COSMO_LLVM_ENTRY_ARCH)
        if (NOT "${COSMO_LLVM_ENTRY_ARCH}" STREQUAL "${ARCH}")
            continue()
        endif ()

        string(JSON COSMO_LLVM_ENTRY_BUILD_TYPE GET "${COSMO_LLVM_MANIFEST_JSON}" ${COSMO_LLVM_INDEX} build_type)
        if (NOT "${COSMO_LLVM_ENTRY_BUILD_TYPE}" STREQUAL "${BUILD_TYPE}")
            continue()
        endif ()

        string(JSON COSMO_LLVM_ENTRY_LTO GET "${COSMO_LLVM_MANIFEST_JSON}" ${COSMO_LLVM_INDEX} lto)
        cosmo_llvm_bool_value("${COSMO_LLVM_ENTRY_LTO}" COSMO_LLVM_ENTRY_WANTS_LTO)
        if (NOT "${COSMO_LLVM_ENTRY_WANTS_LTO}" STREQUAL "${COSMO_LLVM_WANT_LTO}")
            continue()
        endif ()

        string(JSON COSMO_LLVM_ENTRY_FILENAME GET "${COSMO_LLVM_MANIFEST_JSON}" ${COSMO_LLVM_INDEX} filename)
        string(JSON COSMO_LLVM_ENTRY_SHA256 GET "${COSMO_LLVM_MANIFEST_JSON}" ${COSMO_LLVM_INDEX} sha256)
        set(${OUTPUT_FILENAME} "${COSMO_LLVM_ENTRY_FILENAME}" PARENT_SCOPE)
        set(${OUTPUT_SHA256} "${COSMO_LLVM_ENTRY_SHA256}" PARENT_SCOPE)
        return()
    endforeach ()

    message(FATAL_ERROR
        "No matching LLVM artifact in ${COSMO_LLVM_MANIFEST_ABS} for "
        "version=${VERSION}, platform=${PLATFORM}, arch=${ARCH}, "
        "build_type=${BUILD_TYPE}, lto=${COSMO_LLVM_WANT_LTO}.")
endfunction()

function(cosmo_llvm_archive_url VERSION FILENAME OUTPUT_VARIABLE)
    string(REPLACE "+" "%2B" COSMO_LLVM_URL_VERSION "${VERSION}")
    set(${OUTPUT_VARIABLE}
        "https://github.com/clice-io/clice-llvm/releases/download/${COSMO_LLVM_URL_VERSION}/${FILENAME}"
        PARENT_SCOPE)
endfunction()

function(cosmo_llvm_ensure_archive URL ARCHIVE EXPECTED_SHA256)
    if (EXISTS "${ARCHIVE}")
        file(SHA256 "${ARCHIVE}" COSMO_LLVM_CURRENT_SHA256)
        if ("${COSMO_LLVM_CURRENT_SHA256}" STREQUAL "${EXPECTED_SHA256}")
            message(STATUS "Using cached LLVM archive: ${ARCHIVE}")
            return()
        endif ()

        if (COSMO_LLVM_OFFLINE)
            message(FATAL_ERROR
                "Cached LLVM archive hash mismatch in offline mode: ${ARCHIVE}\n"
                "Expected ${EXPECTED_SHA256}, got ${COSMO_LLVM_CURRENT_SHA256}.")
        endif ()

        message(STATUS "Removing LLVM archive with SHA-256 mismatch: ${ARCHIVE}")
        file(REMOVE "${ARCHIVE}")
    endif ()

    if (COSMO_LLVM_OFFLINE)
        message(FATAL_ERROR
            "LLVM archive is not cached and COSMO_LLVM_OFFLINE=ON: ${ARCHIVE}\n"
            "Set COSMO_LLVM_PATH to a local LLVM/Clang install prefix or populate the target cache.")
    endif ()

    get_filename_component(COSMO_LLVM_ARCHIVE_DIR "${ARCHIVE}" DIRECTORY)
    file(MAKE_DIRECTORY "${COSMO_LLVM_ARCHIVE_DIR}")

    set(COSMO_LLVM_DOWNLOAD_OPTIONS)
    set(COSMO_LLVM_GITHUB_TOKEN "$ENV{GH_TOKEN}")
    if (NOT COSMO_LLVM_GITHUB_TOKEN)
        set(COSMO_LLVM_GITHUB_TOKEN "$ENV{GITHUB_TOKEN}")
    endif ()
    if (COSMO_LLVM_GITHUB_TOKEN)
        list(APPEND COSMO_LLVM_DOWNLOAD_OPTIONS HTTPHEADER "Authorization: Bearer ${COSMO_LLVM_GITHUB_TOKEN}")
    endif ()

    message(STATUS "Downloading LLVM artifact: ${URL}")
    file(DOWNLOAD "${URL}" "${ARCHIVE}"
        EXPECTED_HASH "SHA256=${EXPECTED_SHA256}"
        STATUS COSMO_LLVM_DOWNLOAD_STATUS
        SHOW_PROGRESS
        TLS_VERIFY ON
        ${COSMO_LLVM_DOWNLOAD_OPTIONS})

    list(GET COSMO_LLVM_DOWNLOAD_STATUS 0 COSMO_LLVM_DOWNLOAD_CODE)
    list(GET COSMO_LLVM_DOWNLOAD_STATUS 1 COSMO_LLVM_DOWNLOAD_MESSAGE)
    if (NOT COSMO_LLVM_DOWNLOAD_CODE EQUAL 0)
        file(REMOVE "${ARCHIVE}")
        message(FATAL_ERROR
            "Failed to download LLVM artifact.\n"
            "URL: ${URL}\n"
            "Path: ${ARCHIVE}\n"
            "Error: ${COSMO_LLVM_DOWNLOAD_MESSAGE}")
    endif ()
endfunction()

function(cosmo_llvm_flatten_install_dir PREFIX)
    cosmo_llvm_install_is_usable("${PREFIX}" COSMO_LLVM_PREFIX_USABLE)
    if (COSMO_LLVM_PREFIX_USABLE)
        return()
    endif ()

    file(GLOB COSMO_LLVM_CHILDREN LIST_DIRECTORIES true "${PREFIX}/*")
    set(COSMO_LLVM_CHILD_DIRS)
    foreach (COSMO_LLVM_CHILD IN LISTS COSMO_LLVM_CHILDREN)
        if (IS_DIRECTORY "${COSMO_LLVM_CHILD}")
            list(APPEND COSMO_LLVM_CHILD_DIRS "${COSMO_LLVM_CHILD}")
        endif ()
    endforeach ()

    list(LENGTH COSMO_LLVM_CHILD_DIRS COSMO_LLVM_CHILD_DIR_COUNT)
    if (NOT COSMO_LLVM_CHILD_DIR_COUNT EQUAL 1)
        return()
    endif ()

    list(GET COSMO_LLVM_CHILD_DIRS 0 COSMO_LLVM_NESTED_DIR)
    cosmo_llvm_install_is_usable("${COSMO_LLVM_NESTED_DIR}" COSMO_LLVM_NESTED_USABLE)
    if (NOT COSMO_LLVM_NESTED_USABLE)
        return()
    endif ()

    message(STATUS "Flattening LLVM install directory: ${COSMO_LLVM_NESTED_DIR}")
    file(GLOB COSMO_LLVM_NESTED_ENTRIES LIST_DIRECTORIES true "${COSMO_LLVM_NESTED_DIR}/*")
    foreach (COSMO_LLVM_NESTED_ENTRY IN LISTS COSMO_LLVM_NESTED_ENTRIES)
        get_filename_component(COSMO_LLVM_ENTRY_NAME "${COSMO_LLVM_NESTED_ENTRY}" NAME)
        set(COSMO_LLVM_TARGET_ENTRY "${PREFIX}/${COSMO_LLVM_ENTRY_NAME}")
        if (EXISTS "${COSMO_LLVM_TARGET_ENTRY}")
            message(FATAL_ERROR
                "Cannot flatten LLVM install directory; target already exists: "
                "${COSMO_LLVM_TARGET_ENTRY}")
        endif ()
        file(RENAME "${COSMO_LLVM_NESTED_ENTRY}" "${COSMO_LLVM_TARGET_ENTRY}")
    endforeach ()
    file(REMOVE_RECURSE "${COSMO_LLVM_NESTED_DIR}")
endfunction()

function(cosmo_llvm_extract_archive ARCHIVE PREFIX)
    if (EXISTS "${PREFIX}")
        file(REMOVE_RECURSE "${PREFIX}")
    endif ()
    file(MAKE_DIRECTORY "${PREFIX}")

    message(STATUS "Extracting LLVM artifact to: ${PREFIX}")
    file(ARCHIVE_EXTRACT INPUT "${ARCHIVE}" DESTINATION "${PREFIX}")
    cosmo_llvm_flatten_install_dir("${PREFIX}")
    cosmo_llvm_require_usable("${PREFIX}" "Downloaded")
endfunction()

function(cosmo_setup_llvm)
    get_filename_component(COSMO_LLVM_DOWNLOAD_ROOT "${COSMO_LLVM_DOWNLOAD_DIR}" ABSOLUTE)
    set(COSMO_LLVM_CONFIGURED_PATH "${COSMO_LLVM_PATH}")
    set(COSMO_LLVM_RESOLVED_PATH "")

    if (COSMO_LLVM_CONFIGURED_PATH)
        get_filename_component(COSMO_LLVM_CONFIGURED_ABS "${COSMO_LLVM_CONFIGURED_PATH}" ABSOLUTE)
        cosmo_llvm_install_is_usable("${COSMO_LLVM_CONFIGURED_ABS}" COSMO_LLVM_CONFIGURED_USABLE)

        set(COSMO_LLVM_CONFIGURED_IS_AUTO OFF)
        if ("${COSMO_LLVM_CONFIGURED_ABS}" STREQUAL "${COSMO_LLVM_DOWNLOAD_ROOT}")
            set(COSMO_LLVM_CONFIGURED_IS_AUTO ON)
        else ()
            string(FIND "${COSMO_LLVM_CONFIGURED_ABS}" "${COSMO_LLVM_DOWNLOAD_ROOT}/" COSMO_LLVM_AUTO_PATH_INDEX)
            if (COSMO_LLVM_AUTO_PATH_INDEX EQUAL 0)
                set(COSMO_LLVM_CONFIGURED_IS_AUTO ON)
            endif ()
        endif ()

        if (COSMO_LLVM_CONFIGURED_USABLE)
            set(COSMO_LLVM_RESOLVED_PATH "${COSMO_LLVM_CONFIGURED_ABS}")
            message(STATUS "Using configured LLVM install: ${COSMO_LLVM_RESOLVED_PATH}")
        elseif (NOT COSMO_LLVM_CONFIGURED_IS_AUTO)
            cosmo_llvm_require_usable("${COSMO_LLVM_CONFIGURED_ABS}" "Configured")
        endif ()
    endif ()

    if (NOT COSMO_LLVM_RESOLVED_PATH)
        cosmo_llvm_build_type(COSMO_LLVM_BUILD_TYPE)
        cosmo_llvm_normalize_platform(COSMO_LLVM_PLATFORM)
        cosmo_llvm_normalize_arch(COSMO_LLVM_ARCH)
        cosmo_llvm_pick_artifact(
            "${COSMO_LLVM_VERSION}"
            "${COSMO_LLVM_BUILD_TYPE}"
            "${COSMO_LLVM_PLATFORM}"
            "${COSMO_LLVM_ARCH}"
            COSMO_LLVM_ARTIFACT_FILENAME
            COSMO_LLVM_ARTIFACT_SHA256)
        cosmo_llvm_artifact_stem("${COSMO_LLVM_ARTIFACT_FILENAME}" COSMO_LLVM_ARTIFACT_STEM)

        set(COSMO_LLVM_RESOLVED_PATH
            "${COSMO_LLVM_DOWNLOAD_ROOT}/${COSMO_LLVM_VERSION}/${COSMO_LLVM_ARTIFACT_STEM}")
        cosmo_llvm_install_is_usable("${COSMO_LLVM_RESOLVED_PATH}" COSMO_LLVM_CACHE_USABLE)

        if (COSMO_LLVM_CACHE_USABLE)
            message(STATUS "Using cached LLVM install: ${COSMO_LLVM_RESOLVED_PATH}")
        else ()
            set(COSMO_LLVM_ARCHIVE
                "${COSMO_LLVM_DOWNLOAD_ROOT}/cache/${COSMO_LLVM_VERSION}/${COSMO_LLVM_ARTIFACT_FILENAME}")
            cosmo_llvm_archive_url(
                "${COSMO_LLVM_VERSION}"
                "${COSMO_LLVM_ARTIFACT_FILENAME}"
                COSMO_LLVM_DOWNLOAD_URL)
            cosmo_llvm_ensure_archive(
                "${COSMO_LLVM_DOWNLOAD_URL}"
                "${COSMO_LLVM_ARCHIVE}"
                "${COSMO_LLVM_ARTIFACT_SHA256}")
            cosmo_llvm_extract_archive("${COSMO_LLVM_ARCHIVE}" "${COSMO_LLVM_RESOLVED_PATH}")
            message(STATUS "Using downloaded LLVM install: ${COSMO_LLVM_RESOLVED_PATH}")
        endif ()
    endif ()

    set(COSMO_LLVM_PATH "${COSMO_LLVM_RESOLVED_PATH}" CACHE PATH
        "LLVM/Clang install prefix for cosmo-clang-sys" FORCE)
    set(COSMO_LLVM_CMAKE_DIR "${COSMO_LLVM_PATH}/lib/cmake/llvm" CACHE PATH
        "LLVM CMake package directory" FORCE)
    set(COSMO_CLANG_CMAKE_DIR "${COSMO_LLVM_PATH}/lib/cmake/clang" CACHE PATH
        "Clang CMake package directory" FORCE)

    list(PREPEND CMAKE_PREFIX_PATH
        "${COSMO_LLVM_PATH}"
        "${COSMO_LLVM_CMAKE_DIR}"
        "${COSMO_CLANG_CMAKE_DIR}")
    set(CMAKE_PREFIX_PATH "${CMAKE_PREFIX_PATH}" PARENT_SCOPE)
endfunction()
