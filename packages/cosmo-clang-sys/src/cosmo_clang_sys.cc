#include "cosmo_clang_sys.h"

#include <clang/Frontend/FrontendActions.h>
#include <clang/Tooling/Tooling.h>

#include <algorithm>
#include <cstddef>
#include <cstring>
#include <memory>
#include <sstream>
#include <string>
#include <vector>

#ifndef COSMO_CLANG_RESOURCE_DIR
#define COSMO_CLANG_RESOURCE_DIR ""
#endif

namespace {

constexpr std::size_t kDefaultMaxHeaderCount = 32;
constexpr std::size_t kDefaultMaxHeaderBytes = 256;
constexpr std::size_t kDefaultMaxSuffixSegments = 16;

std::string view_to_string(cosmo_clang_sys_string_view view) {
  if (view.data == nullptr || view.size == 0) {
    return std::string();
  }
  return std::string(view.data, view.size);
}

char *copy_c_string(const std::string &value) {
  char *copy = new char[value.size() + 1];
  std::memcpy(copy, value.data(), value.size());
  copy[value.size()] = '\0';
  return copy;
}

void set_result(
    cosmo_clang_sys_symbol_result *result,
    cosmo_clang_sys_status status,
    const std::string &qualified_name,
    const std::string &diagnostic) {
  if (result == nullptr) {
    return;
  }

  result->status = status;
  result->qualified_name = qualified_name.empty() ? nullptr : copy_c_string(qualified_name);
  result->diagnostic = diagnostic.empty() ? nullptr : copy_c_string(diagnostic);
}

bool is_identifier_char(char value) {
  return (value >= 'A' && value <= 'Z') ||
         (value >= 'a' && value <= 'z') ||
         (value >= '0' && value <= '9') ||
         value == '_';
}

bool is_identifier_start(char value) {
  return (value >= 'A' && value <= 'Z') ||
         (value >= 'a' && value <= 'z') ||
         value == '_';
}

bool valid_qualified_name(const std::string &value, std::size_t max_segments) {
  std::size_t offset = 0;
  std::size_t segments = 0;
  if (value.rfind("::", 0) == 0) {
    offset = 2;
  }

  while (offset < value.size()) {
    if (!is_identifier_start(value[offset])) {
      return false;
    }

    while (offset < value.size() && is_identifier_char(value[offset])) {
      offset += 1;
    }
    segments += 1;

    if (segments > max_segments) {
      return false;
    }

    if (offset == value.size()) {
      return true;
    }
    if (offset + 2 > value.size() || value.substr(offset, 2) != "::") {
      return false;
    }
    offset += 2;
  }

  return segments > 0;
}

bool valid_header_name(const std::string &value, std::size_t max_bytes) {
  if (value.empty() || value.size() > max_bytes) {
    return false;
  }

  return std::all_of(value.begin(), value.end(), [](char ch) {
    return (ch >= 'A' && ch <= 'Z') ||
           (ch >= 'a' && ch <= 'z') ||
           (ch >= '0' && ch <= '9') ||
           ch == '_' ||
           ch == '-' ||
           ch == '.' ||
           ch == '/';
  });
}

std::string normalize_header_name(const std::string &value) {
  constexpr const char *prefix = "c++/";
  constexpr std::size_t prefix_size = 4;
  if (value.rfind(prefix, 0) == 0) {
    return value.substr(prefix_size);
  }
  return value;
}

std::string build_probe_source(
    const std::vector<std::string> &headers,
    const std::string &namespace_name,
    const std::string &suffix) {
  std::ostringstream source;
  for (const std::string &header : headers) {
    source << "#include <" << header << ">\n";
  }
  source << "namespace cosmo_clang_sys_probe {\n";
  source << "using __cosmo_probe_symbol = " << namespace_name << "::" << suffix << ";\n";
  source << "}\n";
  return source.str();
}

bool parse_probe_source(const std::string &source, std::string *diagnostic) {
  std::vector<std::string> args = {"-std=c++17"};
  if (std::strlen(COSMO_CLANG_RESOURCE_DIR) > 0) {
    args.push_back("-resource-dir");
    args.push_back(COSMO_CLANG_RESOURCE_DIR);
  }

  bool parsed = clang::tooling::runToolOnCodeWithArgs(
      std::make_unique<clang::SyntaxOnlyAction>(),
      source,
      args,
      "cosmo_clang_sys_probe.cc",
      "cosmo-clang-sys");

  if (!parsed && diagnostic != nullptr) {
    *diagnostic = "clang failed to parse probe translation unit";
  }
  return parsed;
}

} // namespace

cosmo_clang_sys_status cosmo_clang_sys_query_symbol(
    const cosmo_clang_sys_symbol_query *query,
    cosmo_clang_sys_symbol_result *result) {
  if (result != nullptr) {
    result->status = COSMO_CLANG_SYS_INVALID_ARGUMENT;
    result->qualified_name = nullptr;
    result->diagnostic = nullptr;
  }

  if (query == nullptr || result == nullptr) {
    return COSMO_CLANG_SYS_INVALID_ARGUMENT;
  }

  const std::size_t max_header_count =
      query->max_header_count == 0 ? kDefaultMaxHeaderCount : query->max_header_count;
  const std::size_t max_header_bytes =
      query->max_header_bytes == 0 ? kDefaultMaxHeaderBytes : query->max_header_bytes;
  const std::size_t max_suffix_segments =
      query->max_suffix_segments == 0 ? kDefaultMaxSuffixSegments : query->max_suffix_segments;

  const std::string namespace_name = view_to_string(query->namespace_name);
  const std::string suffix = view_to_string(query->suffix);

  if (!valid_qualified_name(namespace_name, max_suffix_segments) ||
      !valid_qualified_name(suffix, max_suffix_segments)) {
    set_result(result, COSMO_CLANG_SYS_INVALID_ARGUMENT, "", "invalid namespace or suffix");
    return result->status;
  }

  if (query->headers.items == nullptr || query->headers.size == 0 ||
      query->headers.size > max_header_count) {
    set_result(result, COSMO_CLANG_SYS_INVALID_ARGUMENT, "", "invalid header set");
    return result->status;
  }

  std::vector<std::string> headers;
  headers.reserve(query->headers.size);
  for (std::size_t index = 0; index < query->headers.size; index += 1) {
    std::string header = normalize_header_name(view_to_string(query->headers.items[index]));
    if (!valid_header_name(header, max_header_bytes)) {
      set_result(result, COSMO_CLANG_SYS_INVALID_ARGUMENT, "", "invalid header name");
      return result->status;
    }
    headers.push_back(header);
  }

  const std::string source = build_probe_source(headers, namespace_name, suffix);
  std::string diagnostic;
  if (!parse_probe_source(source, &diagnostic)) {
    set_result(result, COSMO_CLANG_SYS_NOT_FOUND, "", diagnostic);
    return result->status;
  }

  set_result(result, COSMO_CLANG_SYS_OK, namespace_name + "::" + suffix, "");
  return result->status;
}

void cosmo_clang_sys_dispose_result(cosmo_clang_sys_symbol_result *result) {
  if (result == nullptr) {
    return;
  }

  delete[] result->qualified_name;
  delete[] result->diagnostic;
  result->qualified_name = nullptr;
  result->diagnostic = nullptr;
  result->status = COSMO_CLANG_SYS_INVALID_ARGUMENT;
}
