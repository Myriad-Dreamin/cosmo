# Cosmo

Cosmo is a language for writing compile-time programs that generate ordinary
C++ artifacts, while still being able to use existing C++ libraries explicitly.

The current compiler package, `cosmoc`, is written in Cosmo and compiled by
`cosmo0`. Building `cosmoc` from source requires a C++17 compiler, CMake, and
Rust/Cargo. Scala is not required for normal source builds; only developers who
work on `cosmo0` itself need the Scala/sbt toolchain.

## C++ Import Syntax

C++ headers are imported through an explicit namespace alias. Header-only C++
imports are intentionally unsupported.

```cos
import std as cstd from "c++/vector"

type CppVector[T] = cstd::vector[T]
```

The alias `cstd` is the only name introduced into Cosmo scope. The original C++
namespace name `std` is not implicitly visible unless it is imported as the
local alias.

Multiple imports merge when they use the same alias and C++ namespace:

```cos
import std as cstd from "c++/vector"
import std as cstd from "c++/string"

type CppVector[T] = cstd::vector[T]
```

## Build and Run

Requirements for building and running `cosmoc` from source:

- C++17 compiler: MSVC, Clang, or GCC
- CMake 3.19 or newer
- Rust/Cargo, for native support libraries used by Cosmo packages

Build with the shell wrapper:

```sh
./scripts/build.sh
```

On Windows PowerShell:

```powershell
./scripts/build.ps1
```

Both wrappers configure CMake under `target/cosmo/cmake` by default and build
the `cosmoc` target. Extra CMake configure arguments can be passed through:

```sh
./scripts/build.sh -DCOSMO_ENABLE_CLANG_SYS=ON
```

## Minimal Commands

The development driver commands below use Node.js.

Run a minimal checked-in package sample:

```sh
node cmd/cosmo/main.js -p samples/Packages/basic run
```

Run a package entrypoint:

```sh
node cmd/cosmo/main.js -p fixtures/cosmo0/package/run-smoke run -- --example-arg
```

Build the Cosmo-written compiler package to an executable path:

```sh
node cmd/cosmo/main.js -p packages/cosmoc build -o target/cosmoc
```

The package executable is built under the selected package's `target/`
directory unless an explicit `-o` path is provided. It executes with the package
root as its working directory and receives all arguments after `run` or after
`--` as process arguments.

## More Documentation

The older README notes were moved to [docs/introduction.md](docs/introduction.md)
and will be reorganized with the rest of the documentation later.
