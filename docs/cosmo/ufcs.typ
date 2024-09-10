#import "mod.typ": *

#show: book-page.with(title: "UFCS")

#show raw.where(lang: "cpp", block: true): set block(above: 0.5em)

== Restricted UFCS (Uniform Function Call Syntax)

Cosmo interprets dots in restricted UFCS style. UFCS is a feature that allows you to call a method on a type that is not defined in the type itself.

```cos
class Cat {
  def meow1() {}
  def meow2(self) {}
  def meow3(this: Cat) {}
}

def meow4(cat: Cat) {}

def main() = {
  // Legal call in Cosmo
  Cat.meow1()
  Cat().meow1()
  // error, require an instance: Cat.meow2()
  Cat().meow2()
  Cat.meow2(Cat())
  // error, require an instance: Cat.meow3()
  Cat().meow3()
  Cat.meow3(Cat())
  // error, not in namespace of the class: Cat().meow4()
  meow4(Cat())
}
```

```cpp
struct Cat {
  static void meow1() {}
  void meow2() const {}
  static void meow3(Cat this) {}
}
void meow4(Cat cat) {}

int main() {
  Cat::meow1();
  Cat::meow1();
  Cat().meow2();
  Cat().meow2();
  Cat::meow3(Cat());
  Cat::meow3(Cat());
  meow4(Cat());
}
```

=== UFCS Case 1

For ```cos meow1```, both ```cos Cat.meow1()``` and ```cos Cat().meow1()``` are legal. They will be translated to ```cpp Cat::meow1()``` in C++.

=== UFCS Case 2/3

For ```cos meow2``` and ```cos meow3```, both ```cos Cat().meow2()``` and ```cos Cat.meow2(Cat())``` are legal. They will be translated to ```cos Cat().meow2()``` or ```cpp Cat::meow3(Cat())``` in C++.

=== Forbidded UFCS Case 4

Note for ```cos meow4```, it is a function outside the class. It is not legal to call ```cos Cat().meow4()``` in Cosmo. This is because we forbid #link("https://en.wikipedia.org/wiki/Function_overloading")[_Function Overloading_] in Cosmo. Therefore, we have to call ```cos meow4(Cat())``` in Cosmo.

== Self Sugar

The only difference between `meow2` and `meow3` is the translated C++ code. In C++, `meow2` (self) is translated at member function in best efforts, while `meow3` is translated at static function.

Note, you can also annotate type on `self`:

```cos
class Cat {
  def meow5(self: Ref(Self)) {}
  def meow6(self: RefMut(Self)) {}
  def meow7(self: Self) {}
  def meow8(self: Box(Self)) {}
}
```

```cpp
struct Cat {
  void meow5() const {}
  void meow6() {}
  static void meow7(Cat self) {}
  static void meow8(std::unique_ptr<Cat> self) {}
}
```

== Discussion

It is still unclear on how to simplify following methods:

```cos
class Cat {
  def meow5(self: Ref(Self)) {}
  def meow6(self: RefMut(Self)) {}
  def meow7(self: Self) {}
}
```

In Rust, they are:

```rs
impl Cat {
  fn meow5(&self) {}
  fn meow6(&mut self) {}
  fn meow7(self) {}
}
```

