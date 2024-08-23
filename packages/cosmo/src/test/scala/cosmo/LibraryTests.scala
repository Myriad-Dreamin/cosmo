package cosmo
import scala.scalajs.js

class LibraryTest extends munit.FunSuite:
  def runTestOnFile(path: String) = {
    // read the file
    var fs = js.Dynamic.global.require("fs")
    var src =
      fs.readFileSync(path, "utf8").asInstanceOf[String]
    var result = Cosmo.mayCompile(src)
    println(result)
  }

  test("std.str") {
    runTestOnFile("library/std/str.cos")
  }
  test("std.collections.vec") {
    runTestOnFile("library/std/collections/vec.cos")
  }
  test("std.collections.set") {
    runTestOnFile("library/std/collections/set.cos")
  }
  test("std.collections.map") {
    runTestOnFile("library/std/collections/map.cos")
  }
end LibraryTest
