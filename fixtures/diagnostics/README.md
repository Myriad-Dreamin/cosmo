# cosmo0 Diagnostic Fixtures

Diagnostic fixtures are ordinary `.cos` files that use a multi-file convention
so one fixture can model a whole package without creating a directory tree per
case. Fixtures are discovered by scanning `.cos` files with `/// diag(...)`
directives.

```cos
/// path: main.cos
/// diag(error, 2:3): cosmo0.type.unresolved-name
def main(): i32 = {
  missing
}
/// end path: main.cos
```

Rules:

- `/// path: <relative-file>` starts one embedded source file.
- `/// end path: <relative-file>` ends the current embedded source file.
- `/// diag(<severity>, <line>:<column>): <text>` declares an expected diagnostic
  for the current embedded source file.
- Diagnostic line and column numbers are relative to the embedded source body;
  directive lines are not counted.
- `<text>` is matched against `diagnostic.code: diagnostic.message`, so fixtures
  can name a stable diagnostic code and optionally include message text.
