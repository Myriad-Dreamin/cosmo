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

def main(): Unit = {
  val vec = CppVector[u32]()
  vec.push_back(1)
  vec.push_back(2)
  println(vec.size())
}
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

Both wrappers build the Cosmo compiler package with `cosmo0` and write the
executable to `target/cosmoc` by default. To choose another output path, set
`COSMO_OUTPUT`:

```sh
COSMO_OUTPUT=target/cosmoc-dev ./scripts/build.sh
```

The driver reads `.env` by default before running commands. Pass `-f <file>` to
select another file, for example:

```sh
node cmd/cosmo/main.js -f .env.prod -p packages/cosmoc build -o target/cosmoc
```

`packages/cosmoc` requests the native `cosmo-clang-sys` support library. Set
`COSMO_LLVM_PATH` to a local LLVM/Clang install prefix when one is available; if
it is unset, the CMake link step uses
`packages/cosmo-clang-sys/config/llvm-manifest.json` and downloads the matching
`llvm-dist` component archives into `target/cosmo/llvm`. The default manifest
combines `llvm-core`, `clang-sdk`, `clang-tooling`, and `clang-repl` for the
selected platform where `llvm-dist` publishes that target, and keeps legacy
single-archive `clice-llvm` fallbacks for release targets that are not published
there yet.

On Linux, the downloaded LLVM/Clang static libraries may require a newer or
matching GNU toolchain. If the native link reports missing symbols such as
`__isoc23_*`, `arc4random`, or newer `GLIBC_2.xx` versions, set
`COSMO_GCC_TOOLCHAIN` or `GCC_TOOLCHAIN` to the matching GCC root, for example
`COSMO_GCC_TOOLCHAIN=/usr/local/gcc-14.3.0`. The CMake target passes that value
to Clang as `--gcc-toolchain` and links `libgcc`/`libstdc++` statically by
default; for a local LLVM build, also set `COSMO_LLVM_PATH` and `CC`/`CXX` to
the matching Clang install before the generated CMake build directory is first
configured. Use `COSMO_CMAKE_C_COMPILER` and `COSMO_CMAKE_CXX_COMPILER` when the
driver should pass compiler paths as explicit CMake cache entries.

## Minimal Commands

The development driver commands below use Node.js.

Run a minimal checked-in package sample:

```sh
node cmd/cosmo/main.js -p samples/Packages/basic run
```

After building `target/cosmoc`, run a source file through the Cosmo-written
driver. `cosmoc` walks upward from the file to find a package `cosmo.json`;
when no package manifest is found, it compiles and runs the file as a
standalone source:

```sh
target/cosmoc run samples/workspaces/single-file/main.cos
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
