package cosmo
import scala.scalajs.js

class SampleTest extends TestBase:
  test("Syntax/playground") {
    compilePath("samples/Syntax/playground.cos")
  }
  test("HelloWorld") {
    compilePath("samples/HelloWorld/main.cos")
  }
  test("Syntax/class") {
    compilePath("samples/Syntax/class.cos")
  }
  test("Syntax/literal") {
    compilePath("samples/Syntax/literal.cos")
  }
  test("Syntax/cf.syntax") {
    compilePath("samples/Syntax/cf.syntax.cos")
  }
  test("Syntax/decl.syntax") {
    compilePath("samples/Syntax/decl.syntax.cos")
  }
  test("Syntax/callExpr.syntax") {
    compilePath("samples/Syntax/callExpr.syntax.cos")
  }
  test("Syntax/expr.syntax") {
    compilePath("samples/Syntax/expr.syntax.cos")
  }
  test("Syntax/lambda.syntax") {
    compilePath("samples/Syntax/lambda.syntax.cos")
  }
  test("Syntax/try-catch.syntax") {
    compilePath("samples/Syntax/try-catch.syntax.cos")
  }
  test("Syntax/errs/tmplLit01") {
    compilePath("samples/Syntax/errs/tmplLit01.cos-ast")
  }
  test("Syntax/tmplLit.syntax") {
    compilePath("samples/Syntax/tmplLit.syntax.cos")
  }
  test("TypeAnnotation/add") {
    compilePath("samples/TypeAnnotation/add.cos")
  }
  test("Class/basic") {
    compilePath("samples/Class/basic.cos")
  }
  test("Class/jsonValue") {
    compilePath("samples/Class/jsonValue.cos")
  }
  test("Class/nat") { // broken by self ref
    compilePath("samples/Class/nat.cos")
  }
  test("Class/natCons") { // broken by self ref
    compilePath("samples/Class/natCons.cos")
  }
  test("Class/method") {
    compilePath("samples/Class/method.cos")
  }
  test("Class/staticMethod") {
    compilePath("samples/Class/staticMethod.cos")
  }
  test("Trait/empty") {
    compilePath("samples/Trait/empty.cos")
  }
  test("Trait/resultProblem") {
    compilePath("samples/Trait/resultProblem.cos")
  }
  test("Trait/formatter") {
    compilePath("samples/Trait/formatter.cos")
  }
  test("Trait/formatter_t") {
    compilePath("samples/Trait/formatter_t.cos")
  }
  test("Trait/display") {
    compilePath("samples/Trait/display.cos")
  }
  test("Trait/mutSelf") {
    compilePath("samples/Trait/mutSelf.cos")
  }
  test("Trait/constraint") {
    compilePath("samples/Trait/constraint.cos")
  }
  test("Trait/iter".only) {
    compilePath("samples/Trait/iter.cos")
  }
  test("ControlFlow/loop") {
    compilePath("samples/ControlFlow/loop.cos")
  }
  test("ControlFlow/forIn") {
    compilePath("samples/ControlFlow/forIn.cos")
  }
  test("ControlFlow/forInI") {
    compilePath("samples/ControlFlow/forInI.cos")
  }
  test("ControlFlow/mainIf") {
    compilePath("samples/ControlFlow/mainIf.cos")
  }
  test("Format/templateLit") {
    compilePath("samples/Format/templateLit.cos")
  }
  test("Pattern/natAdd") {
    compilePath("samples/Pattern/natAdd.cos")
  }
  test("Pattern/option") {
    compilePath("samples/Pattern/option.cos")
  }
  test("Pattern/result") {
    compilePath("samples/Pattern/result.cos")
  }
  test("Pattern/byStr") {
    compilePath("samples/Pattern/byStr.cos")
  }
  test("Io/readFile") {
    compilePath("samples/Io/readFile.cos")
  }
  test("Vec/push") {
    compilePath("samples/Vec/push.cos")
  }
  test("PythonTutorial/a_calc") {
    compilePath("samples/PythonTutorial/a_calc.cos")
  }
end SampleTest
