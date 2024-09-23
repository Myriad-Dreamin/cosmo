package cosmo
import scala.scalajs.js

class LibraryTest extends TestBase:
  test("playground") {
    compilePath("samples/Syntax/playground.cos")
  }
  test("std.prelude") {
    compilePath("library/std/src/prelude.cos")
  }
  test("std.prelude.core") {
    compilePath("library/std/src/prelude/core.cos")
  }
  test("std.result") {
    compilePath("library/std/src/result.cos")
  }
  test("std.io") {
    compilePath("library/std/src/io.cos")
  }
  test("std.str") {
    compilePath("library/std/src/str.cos")
  }
  // test("std.memory") {
  //   compilePath("library/std/src/memory.cos")
  // }
  test("std.collections.vec") {
    compilePath("library/std/src/collections/vec.cos")
  }
  test("std.collections.set") {
    compilePath("library/std/src/collections/set.cos")
  }
  test("std.collections.map") {
    compilePath("library/std/src/collections/map.cos")
  }
  test("std.json".only) {
    compilePath("library/std/src/json.cos")
  }
  test("std.fs") {
    compilePath("library/std/src/fs.cos")
  }
end LibraryTest
