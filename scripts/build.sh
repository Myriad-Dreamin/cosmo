#!/usr/bin/env sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
build_dir=${COSMO_BUILD_DIR:-"$repo_root/target/cosmo/cmake"}

cmake -S "$repo_root" -B "$build_dir" "$@"
cmake --build "$build_dir" --target cosmoc

case "$(uname -s 2>/dev/null || printf unknown)" in
  MINGW*|MSYS*|CYGWIN*) printf '%s\n' "$build_dir/cmd/cosmoc.exe" ;;
  *) printf '%s\n' "$build_dir/cmd/cosmoc" ;;
esac
