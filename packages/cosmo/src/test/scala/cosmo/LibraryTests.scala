package cosmo
import scala.scalajs.js

class LibraryTest extends munit.FunSuite:
  def runTestOnFile(path: String) = {
    // read the file
    var src =
      cosmo.NodeFs.readFileSync(path, "utf8").asInstanceOf[String]
    var compiler = new Cosmo();
    var result = compiler.mayConvert(src)
    println(result)
  }

  test("std.str") {
    runTestOnFile("library/std/src/str.cos")
  }
  test("std.collections.vec") {
    runTestOnFile("library/std/src/collections/vec.cos")
  }
  test("std.collections.set") {
    runTestOnFile("library/std/src/collections/set.cos")
  }
  test("std.collections.map") {
    runTestOnFile("library/std/src/collections/map.cos")
  }
  test("std.json") {
    runTestOnFile("library/std/src/json.cos")
  }
  test("std.fs".only) {
    runTestOnFile("library/std/src/fs.cos")
  }
end LibraryTest
