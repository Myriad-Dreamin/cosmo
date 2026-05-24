#!/usr/bin/env sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
scala_root=${SCALA_STYLE_ROOT:-packages/cosmo0/src/main/scala/cosmo0}

cd "$repo_root"

if command -v scalafmt >/dev/null 2>&1; then
  scalafmt --test "$scala_root"
elif command -v cs >/dev/null 2>&1; then
  scalafmt_version=${SCALAFMT_CLI_VERSION:-}
  if [ -z "$scalafmt_version" ]; then
    scalafmt_version=$(sed -n 's/^version *= *"\([^"]*\)".*/\1/p' .scalafmt.conf | head -n 1)
  fi
  if [ -z "$scalafmt_version" ]; then
    echo "Could not determine scalafmt version from .scalafmt.conf" >&2
    exit 1
  fi
  cs launch "org.scalameta:scalafmt-cli_2.13:$scalafmt_version" -- --test "$scala_root"
else
  echo "scalafmt is not installed and coursier (cs) is unavailable" >&2
  exit 1
fi

find "$scala_root" -name '*.scala' -exec perl -0ne '
  while (/^(?:(?:private(?:\[[^\]]+\])?\s+)?final\s+(?:class|case class)\s+\w+)\(\n[ \t]+[^\n]*,\n\):/mg) {
    my $line = substr($_, 0, $-[0]) =~ tr/\n//;
    print "$ARGV:" . ($line + 1) . ": single-parameter constructor should be written on one line\n";
    $bad = 1;
  }
  END {
    exit($bad ? 1 : 0);
  }
' {} +
