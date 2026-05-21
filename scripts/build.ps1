param(
  [Parameter(ValueFromRemainingArguments = $true)]
  [string[]] $CMakeArgs
)

$ErrorActionPreference = "Stop"

$RepoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
$BuildDir = if ($env:COSMO_BUILD_DIR) {
  $env:COSMO_BUILD_DIR
} else {
  Join-Path $RepoRoot "target/cosmo/cmake"
}

cmake -S $RepoRoot -B $BuildDir @CMakeArgs
if ($LASTEXITCODE -ne 0) {
  exit $LASTEXITCODE
}

cmake --build $BuildDir --target cosmoc
if ($LASTEXITCODE -ne 0) {
  exit $LASTEXITCODE
}

$Executable = if ($env:OS -eq "Windows_NT") {
  Join-Path $BuildDir "cmd/cosmoc.exe"
} else {
  Join-Path $BuildDir "cmd/cosmoc"
}

Write-Output $Executable
