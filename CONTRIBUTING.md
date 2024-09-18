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
yarn build:vscode && code --install-extension editors/vscode/cosmo-0.1.0.vsix
```
