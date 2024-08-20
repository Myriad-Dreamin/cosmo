package cosmo
import scala.scalajs.js

class MonoidTest extends munit.FunSuite:
  test("HelloWorld") {
    // read samples/HelloWorld/main.cos
    var fs = js.Dynamic.global.require("fs")
    var src = fs
      .readFileSync("samples/HelloWorld/main.cos", "utf8")
      .asInstanceOf[String]
    // mayCompile the source code
    var result = Cosmo.mayCompile(src)
    // check the result
    println(result)
  }
end MonoidTest
