#!/usr/bin/env sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
output=${COSMO_OUTPUT:-"target/cosmoc"}

cd "$repo_root"
sbt fullLinkJS
node cmd/cosmo/main.js -p packages/cosmoc build -o "$output"
