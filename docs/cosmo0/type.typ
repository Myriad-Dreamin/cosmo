= cosmo0 Types

== Status

This file is the owner for cosmo0 type syntax and type validity. Section headings are normative ownership markers. Detailed rules are placeholders unless marked otherwise by later OpenSpec changes.

== Primitive Types

Placeholder for scalar and built-in value types accepted by cosmo0, including unit, booleans, integer widths, sizes, bytes, chars, strings, and any other primitive forms admitted by future capability work.

== Reference and Mutability Types

Placeholder for immutable references, mutable references, receiver forms, mutable locals, mutable fields, and the type rules that govern mutation through references.

== Type Aliases

Placeholder for simple type aliases and the limits on alias expansion. User-defined type-level functions and dependent type computation remain outside the subset until explicitly specified.

== Standard Type Application

Placeholder for sealed standard type constructors such as collections, results, arena identifiers, paths, text helpers, and other core0 APIs named by `std.typ`.

Standard type application is source-facing behavior. Temporary descriptor-backed lowering may implement it, but descriptor names do not become public type syntax unless this file or `std.typ` says so.

== Rejected Type Forms

Placeholder for diagnostics covering user-defined generics, trait constraints, higher-kinded types, compile-time type parameters, reflection types, and other full-language type forms that cosmo0 does not accept.
