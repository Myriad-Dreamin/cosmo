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

  test("std/str".only) {
    runTestOnFile("library/std/str.cos")
  }
end LibraryTest
