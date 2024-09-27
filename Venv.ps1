function RunCosmo {
    node '--enable-source-maps' ./cmd/cosmo/main.js $args
}

Set-Alias cosmo RunCosmo

function RunCosmoPerf {
    npx "0x" "--" node '--enable-source-maps' ./cmd/cosmo/main.js $args
}

Set-Alias cosmo-perf RunCosmoPerf