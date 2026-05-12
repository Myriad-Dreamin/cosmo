= cosmo0 Types

== Status

This file is the owner for cosmo0 type syntax and type validity. Section headings are normative ownership markers. Detailed rules are placeholders unless marked otherwise by later OpenSpec changes.

== Primitive Types

cosmo0 accepts the following primitive value types without requiring a standard-library declaration:

- `Unit`
- `Bool`
- signed integer widths `i8`, `i16`, `i32`, and `i64`
- unsigned integer widths `u8`, `u16`, `u32`, `u64`, and `usize`
- scalar aliases `Byte` for `u8`
- `Char`
- `String`
- `f32` and `f64`

Integer literals default to `i32` unless an expected integer type is known from an annotation, parameter, return type, field, or comparison context. Floating literals default to `f64` unless an expected floating type is known. Boolean literals have type `Bool`, string literals have type `String`, and an empty block or unit literal has type `Unit`.

Primitive type names are not an open descriptor namespace. Adding a new primitive type or scalar literal backing requires an explicit spec update in this file and a matching runtime descriptor boundary update in `runtime.typ`.

== Stage 1 Text Scalar Assumptions

`String` is the owned text scalar for Stage 1 source handling. Stage 1 string offsets and lengths are measured in bytes and use `usize`; the `Byte` alias denotes the `u8` value returned by byte-oriented access.

Stage 1 lexing is ASCII-oriented. `Char` exists as the scalar result for character access, but `core0.text` only commits to returning an ASCII-compatible scalar for source bytes in the initial compiler slice. Full Unicode scalar iteration, normalization, grapheme clusters, locale-sensitive case behavior, and encoding repair are outside Stage 1.

`String` values are immutable source values. Slicing creates a new owned `String`, while text views such as `TextView` or source wrappers such as `SourceText` are standard API types owned by `std.typ`; they are not new primitive types and do not extend the descriptor namespace.

== Examples

Accepted type shapes:

```cos
type TokenId = Id<Token>

class TokenBuffer {
  val source: String
  var tokens: Vec<Token>
}

def token_text(token: &Token): &String = &token.text
```

Rejected type shapes until later specs admit them:

```cos
class Box[T] {
  val value: T
}

def IdOf(T: Type) = Id<T>
```

The accepted example uses a simple alias, concrete standard type application, and an immutable reference. The rejected example uses a user-defined generic class and a type-level function.

== Reference and Mutability Types

cosmo0 supports immutable references `&T` and mutable references `&mut T`. A reference type may target a primitive, user class, simple alias, or sealed standard type application accepted by this file.

Methods may receive `&self` or `&mut self`. Mutation through a reference is only valid when the reference is mutable and the selected field or standard operation is mutable. Taking an immutable reference does not make the referenced value mutable.

== Type Aliases

Placeholder for simple type aliases and the limits on alias expansion. User-defined type-level functions and dependent type computation remain outside the subset until explicitly specified.

== Standard Type Application

Placeholder for sealed standard type constructors such as collections, results, arena identifiers, paths, text helpers, and other core0 APIs named by `std.typ`.

Standard type application is source-facing behavior. Temporary descriptor-backed lowering may implement it, but descriptor names do not become public type syntax unless this file or `std.typ` says so.

== Rejected Type Forms

Placeholder for diagnostics covering user-defined generics, trait constraints, higher-kinded types, compile-time type parameters, reflection types, and other full-language type forms that cosmo0 does not accept.
