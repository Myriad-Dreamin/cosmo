# probe-cosmo0-cte-with-cosmo-eval

Probe minimal cosmo0 compile-time evaluation through cosmo0 eval mode. The
smoke keeps `type x = 1 + 1` gated, routes it through a `CosmoEvalRequest`, and
lets cosmo0 eval compile and invoke a small provider entry against cached
PCH/precompiled context state.
