package cosmo
import scala.scalajs.js

class SampleTest extends munit.FunSuite:
  def runTestOnFile(path: String) = {
    // read the file
    var fs = js.Dynamic.global.require("fs")
    var src =
      fs.readFileSync(path, "utf8").asInstanceOf[String]
    // mayCompile the source code
    var result = Cosmo.mayCompile(src)
    // check the result
    println(result)
  }

  test("HelloWorld") {
    runTestOnFile("samples/HelloWorld/main.cos")
  }
  test("TypeAnnotation/add".only) {
    runTestOnFile("samples/TypeAnnotation/add.cos")
  }
  test("VecPush") {
    runTestOnFile("samples/Vec/push.cos")
  }
end SampleTest
