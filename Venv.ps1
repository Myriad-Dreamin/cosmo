function RunCosmo {
    node  '--enable-source-maps' ./cmd/cosmo/main.js $args
}

Set-Alias cosmo RunCosmo