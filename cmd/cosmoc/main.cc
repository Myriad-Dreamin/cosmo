#include <cstdio>

#include <cosmo/std/src/fs.h>

#include "package-less/samples/Pattern/natAdd.cc"

using cosmo_std::str::String;

int main() {
  auto file_content = cosmo_std::fs::readFile(String("cmd/CMakeLists.txt"));
  printf("Read with cosmo function: Content of `cmd/CMakeLists.txt`\n");
  printf("----------------\n");
  printf("%s\n", file_content.internal.c_str());
  printf("----------------\n");
  Nat n = Nat::Zero_cons();
  Nat one = Nat::Succ_cons(Nat::Zero_cons());
  Nat x = n.clone();
  for (int i = 0; i < 10; i++) {
    x = Nat::Succ_cons(std::move(x));
  }
  printf("Nat(x).to_int() = %d\n", x.to_int());
  return 0;
}
