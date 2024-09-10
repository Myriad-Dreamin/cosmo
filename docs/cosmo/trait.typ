#import "mod.typ": *

#show: book-page.with(title: "Trait")

#show raw.where(lang: "cpp", block: true): set block(above: 0.5em)

== Traits

Traits are classes containing unimplemented methods, while you can provide default impls:

```cos
trait Unsigned(T: Type) {
  assert(IsUnsigned(T.value));
  def asUint64(self): u64 = staticCast(u64, self.value);
}
```

```cpp
template <typename T>
struct UnsignedConcept {
  virtual uint64_t asUint64() = 0;
  virtual ~UnsignedConcept() = default;
};

template <typename T>
struct UnsignedModel: public UnsignedConcept<T> {
  T& self;
  UnsignedModel(T& self) : self(self) { static_assert(IsUnsigned<T::ValueT>::value); }
  uint64_t asUint64() override {
    return static_cast<uint64_t>(self.value);
  }
};
```

== Trait Implementations

As you see, traits are dispatched in some C++ magic concepts.

```cos
impl(T <: U32 | U16 | U8) Unsigned(T) for T {
  def asUint64(self): u64 = staticCast(u64, self);
}
```

```cpp
template <typename T, typename Cond>
struct TModel;

template <typename T>
struct TModel<T, std::enable_if<std::is_same<T, uint32_t | uint16_t | uint8_t>>>: public UnsignedConcept<T> {
  T& self;
  UnsignedModel(T& self) : self(self) { }
  uint64_t asUint64() override {
    return static_cast<uint64_t>(self);
  }
};
```

== Discussion

Templated ```cos impl``` introduces challenge on trait method resolving. Users who gets interested in this feature can check #link("https://rustc-dev-guide.rust-lang.org/solve/trait-solving.html")[_Rust: Trait solving_].
