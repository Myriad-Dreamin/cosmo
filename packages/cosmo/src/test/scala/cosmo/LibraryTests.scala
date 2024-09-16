package cosmo
import scala.scalajs.js

class LibraryTest extends munit.FunSuite:
  def runTestOnFile(path: String) = {
    // read the file
    var src =
      cosmo.NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
    var compiler = new Cosmo();
    compiler.loadPackage(PackageMetaSource.ProjectPath("library/std"));
    var result = compiler.transpile(src)
    println(result)
  }

  test("std.prelude") {
    runTestOnFile("library/std/src/prelude.cos")
  }
  test("std.prelude.core") {
    runTestOnFile("library/std/src/prelude/core.cos")
  }
  test("std.result") {
    runTestOnFile("library/std/src/result.cos")
  }
  test("std.io") {
    runTestOnFile("library/std/src/io.cos")
  }
  test("std.str") {
    runTestOnFile("library/std/src/str.cos")
  }
  // test("std.memory") {
  //   runTestOnFile("library/std/src/memory.cos")
  // }
  test("std.collections.vec") {
    runTestOnFile("library/std/src/collections/vec.cos")
  }
  test("std.collections.set") {
    runTestOnFile("library/std/src/collections/set.cos")
  }
  test("std.collections.map") {
    runTestOnFile("library/std/src/collections/map.cos")
  }
  test("std.json".only) {
    runTestOnFile("library/std/src/json.cos")
  }
  test("std.fs") {
    runTestOnFile("library/std/src/fs.cos")
  }
end LibraryTest
