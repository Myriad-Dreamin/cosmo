$ErrorActionPreference = "Stop"

$RepoRoot = Resolve-Path (Join-Path $PSScriptRoot "..")
$Output = if ($env:COSMO_OUTPUT) {
  $env:COSMO_OUTPUT
} else {
  "target/cosmoc"
}

Push-Location $RepoRoot
try {
  node cmd/cosmo/main.js -p packages/cosmoc build -o $Output
  if ($LASTEXITCODE -ne 0) {
    exit $LASTEXITCODE
  }
} finally {
  Pop-Location
}
