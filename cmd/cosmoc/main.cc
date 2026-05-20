#include <cstdio>

#ifdef COSMO_ENABLE_CLANG_SYS
#include <cosmo_clang_sys.h>
#endif

int main() {
#ifdef COSMO_ENABLE_CLANG_SYS
  cosmo_clang_sys_symbol_result result{};
  cosmo_clang_sys_dispose_result(&result);
#endif
  printf("cosmoc bootstrap placeholder\n");
  return 0;
}
