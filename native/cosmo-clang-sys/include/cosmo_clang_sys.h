#pragma once

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum cosmo_clang_sys_status {
  COSMO_CLANG_SYS_OK = 0,
  COSMO_CLANG_SYS_INVALID_ARGUMENT = 1,
  COSMO_CLANG_SYS_PARSE_FAILED = 2,
  COSMO_CLANG_SYS_NOT_FOUND = 3
} cosmo_clang_sys_status;

typedef struct cosmo_clang_sys_string_view {
  const char *data;
  size_t size;
} cosmo_clang_sys_string_view;

typedef struct cosmo_clang_sys_header_set {
  const cosmo_clang_sys_string_view *items;
  size_t size;
} cosmo_clang_sys_header_set;

typedef struct cosmo_clang_sys_symbol_query {
  cosmo_clang_sys_string_view namespace_name;
  cosmo_clang_sys_string_view suffix;
  cosmo_clang_sys_header_set headers;
  size_t max_header_count;
  size_t max_header_bytes;
  size_t max_suffix_segments;
} cosmo_clang_sys_symbol_query;

typedef struct cosmo_clang_sys_symbol_result {
  cosmo_clang_sys_status status;
  char *qualified_name;
  char *diagnostic;
} cosmo_clang_sys_symbol_result;

cosmo_clang_sys_status cosmo_clang_sys_query_symbol(
    const cosmo_clang_sys_symbol_query *query,
    cosmo_clang_sys_symbol_result *result);

void cosmo_clang_sys_dispose_result(cosmo_clang_sys_symbol_result *result);

#ifdef __cplusplus
}
#endif
