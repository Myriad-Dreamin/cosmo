## Why

Name resolution, package graphs, scopes, and output-affecting compiler state need deterministic maps and sets. The deterministic behavior should be specified in core0/std so generated output and diagnostics remain stable.

## What Changes

- Add deterministic `Map<K, V>` and `Set<T>` or explicit `OrderedMap<K, V>` and `OrderedSet<T>` APIs.
- Define supported key types and iteration order expectations.
- Add cosmo1 package graph, symbol, scope, or resolve components that use the maps.
- Add deterministic iteration and unsupported key tests.

## Capabilities

### New Capabilities

- `core0-deterministic-map-set`: Provides deterministic keyed collections for later compiler stages.

### Modified Capabilities

- `core0-stage-capability-registry`: Adds the later-stage `core0.map-set` capability.

## Impact

- Unblocks package/module graph and name resolution data structures.
- Makes deterministic output behavior testable.
- Keeps map/set policy in std rather than ad hoc compiler behavior.
