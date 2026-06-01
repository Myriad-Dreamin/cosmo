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

cosmo0 accepts sealed standard type application for registered core0 standard constructors named by `std.typ`. Stage 1 includes `Option[T]`, `Result[T, E]`, and `Vec[T]` for optional values, recoverable results, and ordered mutable buffers. Later-stage source may use `Arena[T]` and `Id[T]` when the `core0.arena-id` capability is available.

Standard type application is source-facing behavior. Temporary descriptor-backed lowering may implement it, but descriptor names do not become public type syntax unless this file or `std.typ` says so.

`Option[T]` has exactly one type argument. `Result[T, E]` has exactly two type arguments. `Vec[T]` has exactly one type argument. `Arena[T]` has exactly one type argument and stores values of that item type. `Id[T]` has exactly one type argument and is a phantom-typed handle for values allocated in `Arena[T]`. Later-stage `Map[K, V]` has exactly two type arguments and `Set[T]` has exactly one type argument. Each argument must itself be a valid cosmo0 type, including primitives, user classes, simple aliases, references, and nested sealed standard applications, subject to collection-specific restrictions.

The Stage 1 spelling for standard type application in source is the parser-supported compile-time application form, for example `Option[Token]`, `Result[ExprId, Diagnostic]`, and `Vec[Token]`. The same spelling is used for later arena and deterministic collection types, for example `Arena[Expr]`, `Id[Expr]`, `Map[Symbol, DefId]`, and `Set[Symbol]`. Standard type constructors are not first-class runtime values.

User declarations cannot add new type parameters to classes or functions and cannot define new generic type constructors. A user-defined declaration named `Option`, `Result`, `Vec`, `Arena`, or `Id` does not replace the registered core0 standard constructor.

`Id[T]` is source-level distinct for each `T`. An `Id[Expr]` is not assignable to `Id[Ty]`, cannot be passed to `Arena[Ty].get`, and cannot be used with any arena whose item type is not `Expr`. The initial source API keeps the numeric representation opaque; serialization helpers can be added by a later capability.

`Map[K, V]` and `Set[T]` restrict their key position until trait-based ordering or hashing exists. `K` for `Map[K, V]` and `T` for `Set[T]` must resolve to `String`, a primitive integer type, or `Id[U]`. Aliases are checked after expansion, so a symbol alias such as `type Symbol = String` is valid as a key. User classes, references, floating-point values, booleans, and nested collections are not valid keys for the initial deterministic collection capability and must produce `cosmo0.type.unsupported-map-key`.

== Rejected Type Forms

Placeholder for diagnostics covering user-defined generics, trait constraints, higher-kinded types, compile-time type parameters, reflection types, and other full-language type forms that cosmo0 does not accept.
