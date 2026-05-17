# Build Cosmo Compiler

Lists Presets:

```shell
cmake --list-presets=all .
```

Generate Preset:

```shell
cmake --preset=relwithdebinfo-ninja-clang .
```

Build with Preset:

```shell
cmake --build cmake-build-relwithdebinfo --config RelWithDebInfo --target xxx
```

Use `xxx` to specify the target you want to build, such as `all`.

# Build Cosmo Editor Support

```shell
pnpm run local:vscode
```

VSCode debugging uses the `Run Cosmo Extension` launch configuration, which runs
the `VS Code Extension Prelaunch` task. That task builds the extension and stages
the `packages/cosmos` server payload under `editors/vscode/out/server-root`.
