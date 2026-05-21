# Change Log

All notable changes to "cosmo" will be documented in this file.

The changelog lines unspecified with authors are all written by the @Myriad-Dreamin.

## v0.1.0-rc1 - [2026-05-21]

### Compiler

* Started type inference and select dispatching in https://github.com/Myriad-Dreamin/cosmo/pull/1
* Recursive pattern match in https://github.com/Myriad-Dreamin/cosmo/pull/5
* Added compile-time integral arithmetics in https://github.com/Myriad-Dreamin/cosmo/pull/6
* Added compile-time if and recursive function in https://github.com/Myriad-Dreamin/cosmo/pull/11
* Added cosmo1 core acceptance corpus in https://github.com/Myriad-Dreamin/cosmo/pull/16
* Added shared parser fixtures and parser_test in https://github.com/Myriad-Dreamin/cosmo/pull/25
* (Test) Validate cosmo1 stage1 capability profile in https://github.com/Myriad-Dreamin/cosmo/pull/38
* Added cosmo1 parser self-compile proposal series in https://github.com/Myriad-Dreamin/cosmo/pull/44
* Added cosmo1 native parser AST output in https://github.com/Myriad-Dreamin/cosmo/pull/45
* Completed cosmo1 syntax arenas and spans in https://github.com/Myriad-Dreamin/cosmo/pull/46
* Added cosmo1 package module graph in https://github.com/Myriad-Dreamin/cosmo/pull/47
* Added name resolution in https://github.com/Myriad-Dreamin/cosmo/pull/48
* Added cosmo1 type model in https://github.com/Myriad-Dreamin/cosmo/pull/49
* Resolved cosmo1 declaration types in https://github.com/Myriad-Dreamin/cosmo/pull/50
* Typechecked basic expressions in https://github.com/Myriad-Dreamin/cosmo/pull/51
* Type-checked member calls and self receivers in https://github.com/Myriad-Dreamin/cosmo/pull/52
* Typechecked parser control flow in https://github.com/Myriad-Dreamin/cosmo/pull/53
* Added cosmo1 IR model verifier in https://github.com/Myriad-Dreamin/cosmo/pull/54
* Lowered cosmo1 declarations to ir in https://github.com/Myriad-Dreamin/cosmo/pull/55
* Lowered cosmo1 basic expressions to ir in https://github.com/Myriad-Dreamin/cosmo/pull/56
* Lowered cosmo1 members and intrinsics to ir in https://github.com/Myriad-Dreamin/cosmo/pull/57
* Lowered parser control flow to IR in https://github.com/Myriad-Dreamin/cosmo/pull/58
* (Test) Validate deterministic parser C++ emission in https://github.com/Myriad-Dreamin/cosmo/pull/59
* Merged cosmo1 parser self compile commits in https://github.com/Myriad-Dreamin/cosmo/pull/60
* (Docs) Archive cosmo1 self-compile proposal set in https://github.com/Myriad-Dreamin/cosmo/pull/61
* Pass fixture tests in https://github.com/Myriad-Dreamin/cosmo/pull/83
* Accepted mutable refs for readonly parameters (#75) in https://github.com/Myriad-Dreamin/cosmo/pull/84
* Added cosmoc bootstrap fixed-point test in https://github.com/Myriad-Dreamin/cosmo/pull/85
* Formalize C++ namespace name resolution in https://github.com/Myriad-Dreamin/cosmo/pull/86

### Bootstrap Runtime

* Added cosmo0 scala package in https://github.com/Myriad-Dreamin/cosmo/pull/17
* Added cosmo0 untyped elaborator in https://github.com/Myriad-Dreamin/cosmo/pull/18
* Added cosmo0 typed expressions in https://github.com/Myriad-Dreamin/cosmo/pull/19
* Added cosmo0 lir model in https://github.com/Myriad-Dreamin/cosmo/pull/20
* Added cosmo0 lir type checker in https://github.com/Myriad-Dreamin/cosmo/pull/21
* Lowered cosmo0 declarations to LIR in https://github.com/Myriad-Dreamin/cosmo/pull/22
* Lowered cosmo0 control flow and descriptors to lir in https://github.com/Myriad-Dreamin/cosmo/pull/23
* Added cosmo0 cpp backend in https://github.com/Myriad-Dreamin/cosmo/pull/24
* Added cosmo0 package pipeline in https://github.com/Myriad-Dreamin/cosmo/pull/26
* (Docs) Plan staged cosmo0 runtime capabilities in https://github.com/Myriad-Dreamin/cosmo/pull/27
* Added cosmo0 spec docs skeleton in https://github.com/Myriad-Dreamin/cosmo/pull/28
* Archive model-cosmo0-spec-docs in https://github.com/Myriad-Dreamin/cosmo/pull/29
* Narrow cosmo0 primitive descriptors in https://github.com/Myriad-Dreamin/cosmo/pull/30
* Added cosmo0 extern abi hooks in https://github.com/Myriad-Dreamin/cosmo/pull/31
* Added core0 stage capability registry in https://github.com/Myriad-Dreamin/cosmo/pull/32
* Added core0 text interfaces in https://github.com/Myriad-Dreamin/cosmo/pull/33
* Added core0 option result vec minimal in https://github.com/Myriad-Dreamin/cosmo/pull/34
* Added core0 path fs in https://github.com/Myriad-Dreamin/cosmo/pull/35
* Added core0 text output in https://github.com/Myriad-Dreamin/cosmo/pull/36
* Added core0 char class in https://github.com/Myriad-Dreamin/cosmo/pull/37
* Added core0 arena id in https://github.com/Myriad-Dreamin/cosmo/pull/39
* Added core0 json bridge in https://github.com/Myriad-Dreamin/cosmo/pull/40
* Implement deterministic core0 map set in https://github.com/Myriad-Dreamin/cosmo/pull/41
* Added core0 lossless numeric literals in https://github.com/Myriad-Dreamin/cosmo/pull/42
* Added core0 command capability in https://github.com/Myriad-Dreamin/cosmo/pull/43
* Added uri-sys support library in https://github.com/Myriad-Dreamin/cosmo/pull/70
* Added cosmo0 corpus coverage and fixture matrix in https://github.com/Myriad-Dreamin/cosmo/pull/82

### Language Server and Editor

* Started cosmoc syntax definition in https://github.com/Myriad-Dreamin/cosmo/pull/2
* Added full lsp types generator in https://github.com/Myriad-Dreamin/cosmo/pull/67
* Added ls-base jsonrpc core in https://github.com/Myriad-Dreamin/cosmo/pull/68
* Added ls-base lsp lifecycle in https://github.com/Myriad-Dreamin/cosmo/pull/69
* Added cosmos workspace documents in https://github.com/Myriad-Dreamin/cosmo/pull/71
* Added cosmos diagnostics pipeline in https://github.com/Myriad-Dreamin/cosmo/pull/72
* Added cosmos hover in https://github.com/Myriad-Dreamin/cosmo/pull/73
* Integrate cosmos with vscode in https://github.com/Myriad-Dreamin/cosmo/pull/74
* Added VSCode LSP follow-up proposals in https://github.com/Myriad-Dreamin/cosmo/pull/75
* Added cosmos definition references in https://github.com/Myriad-Dreamin/cosmo/pull/80
* Added vscode language server output channel in https://github.com/Myriad-Dreamin/cosmo/pull/79
* (Fix) Refreshed diagnostics on active document switch in https://github.com/Myriad-Dreamin/cosmo/pull/78
* (Fix) Corrected helloworld diagnostics false positives in https://github.com/Myriad-Dreamin/cosmo/pull/77
* Cosmo0 sample diagnostics in https://github.com/Myriad-Dreamin/cosmo/pull/81

### CLI and Tooling

* (Fix) CMake linker path with `path` from NodeJS world by @seven-mile in https://github.com/Myriad-Dreamin/cosmo/pull/3
* Fixed GitHub Pages docs build in https://github.com/Myriad-Dreamin/cosmo/pull/88
* Installed pnpm for GitHub Pages syntax build in https://github.com/Myriad-Dreamin/cosmo/pull/89

### Testing and Documentation

* Added sample project workspaces in https://github.com/Myriad-Dreamin/cosmo/pull/76

### Misc

* Split expr/term decls in https://github.com/Myriad-Dreamin/cosmo/pull/8
* Split item/term decls and remove Item class in https://github.com/Myriad-Dreamin/cosmo/pull/9
* Evaluate const func call in https://github.com/Myriad-Dreamin/cosmo/pull/10
* Refactor a bit in https://github.com/Myriad-Dreamin/cosmo/pull/12
* Prepare v0.1.0-rc1 release in https://github.com/Myriad-Dreamin/cosmo/pull/90

**Full Changelog**: https://github.com/Myriad-Dreamin/cosmo/commits/v0.1.0-rc1
