package cosmo
import scala.scalajs.js

class SampleTest extends TestBase:
  test("Syntax/playground".ignore) {
    compilePath("samples/legacy/Syntax/playground.cos")
  }
  test("HelloWorld".ignore) {
    compilePath("samples/legacy/HelloWorld/main.cos")
  }
  test("Syntax/class".ignore) {
    compilePath("samples/legacy/Syntax/class.cos")
  }
  test("Syntax/literal".ignore) {
    compilePath("samples/legacy/Syntax/literal.cos")
  }
  test("Syntax/cf.syntax".ignore) {
    compilePath("samples/legacy/Syntax/cf.syntax.cos")
  }
  test("Syntax/decl.syntax".ignore) {
    compilePath("samples/legacy/Syntax/decl.syntax.cos")
  }
  test("Syntax/callExpr.syntax".ignore) {
    compilePath("samples/legacy/Syntax/callExpr.syntax.cos")
  }
  test("Syntax/expr.syntax".ignore) {
    compilePath("samples/legacy/Syntax/expr.syntax.cos")
  }
  test("Syntax/lambda.syntax".ignore) {
    compilePath("samples/legacy/Syntax/lambda.syntax.cos")
  }
  test("Syntax/try-catch.syntax".ignore) {
    compilePath("samples/legacy/Syntax/try-catch.syntax.cos")
  }
  test("Syntax/errs/tmplLit01".ignore) {
    compilePath("samples/legacy/Syntax/errs/tmplLit01.cos")
  }
  test("Syntax/tmplLit.syntax".ignore) {
    compilePath("samples/legacy/Syntax/tmplLit.syntax.cos")
  }
  test("TypeAnnotation/add".ignore) {
    compilePath("samples/legacy/TypeAnnotation/add.cos")
  }
  test("Class/basic".ignore) {
    compilePath("samples/legacy/Class/basic.cos")
  }
  test("Class/jsonValue".ignore) {
    compilePath("samples/legacy/Class/jsonValue.cos")
  }
  test("Class/nat".ignore) { // broken by self ref
    compilePath("samples/legacy/Class/nat.cos")
  }
  test("Class/natCons".ignore) { // broken by self ref
    compilePath("samples/legacy/Class/natCons.cos")
  }
  test("Class/method".ignore) {
    compilePath("samples/legacy/Class/method.cos")
  }
  test("Class/staticMethod".ignore) {
    compilePath("samples/legacy/Class/staticMethod.cos")
  }
  test("CompileTime/arith".ignore) {
    compilePath("samples/legacy/CompileTime/arith.cos")
  }
  test("CompileTime/addFn".ignore) {
    compilePath("samples/legacy/CompileTime/addFn.cos")
  }
  test("CompileTime/recursive".ignore) {
    compilePath("samples/legacy/CompileTime/recursive.cos")
  }
  test("Reflect/name".ignore) {
    compilePath("samples/legacy/Reflect/name.cos")
  }
  test("Trait/empty".ignore) {
    compilePath("samples/legacy/Trait/empty.cos")
  }
  test("Trait/resultProblem".ignore) {
    compilePath("samples/legacy/Trait/resultProblem.cos")
  }
  test("Trait/formatter".ignore) {
    compilePath("samples/legacy/Trait/formatter.cos")
  }
  test("Trait/formatter_t".ignore) {
    compilePath("samples/legacy/Trait/formatter_t.cos")
  }
  test("Trait/display".ignore) {
    compilePath("samples/legacy/Trait/display.cos")
  }
  test("Trait/mutSelf".ignore) {
    compilePath("samples/legacy/Trait/mutSelf.cos")
  }
  test("Trait/constraint".ignore) {
    compilePath("samples/legacy/Trait/constraint.cos")
  }
  test("Trait/iter".ignore) {
    compilePath("samples/legacy/Trait/iter.cos")
  }
  test("ControlFlow/loop".ignore) {
    compilePath("samples/legacy/ControlFlow/loop.cos")
  }
  test("ControlFlow/forIn".ignore) {
    compilePath("samples/legacy/ControlFlow/forIn.cos")
  }
  test("ControlFlow/forInI".ignore) {
    compilePath("samples/legacy/ControlFlow/forInI.cos")
  }
  test("ControlFlow/mainIf".ignore) {
    compilePath("samples/legacy/ControlFlow/mainIf.cos")
  }
  test("Format/templateLit".ignore) {
    compilePath("samples/legacy/Format/templateLit.cos")
  }
  test("Pattern/natAdd".ignore) { // broken by self ref
    compilePath("samples/legacy/Pattern/natAdd.cos")
  }
  test("Pattern/option".ignore) {
    compilePath("samples/legacy/Pattern/option.cos")
  }
  test("Pattern/result".ignore) {
    compilePath("samples/legacy/Pattern/result.cos")
  }
  test("Pattern/byStr".ignore) {
    compilePath("samples/legacy/Pattern/byStr.cos")
  }
  test("Reflect/name".ignore) {
    compilePath("samples/legacy/Reflect/name.cos")
  }
  test("Io/readFile".ignore) {
    compilePath("samples/legacy/Io/readFile.cos")
  }
  test("Vec/push".ignore) {
    compilePath("samples/legacy/Vec/push.cos")
  }
  test("PythonTutorial/a_calc".ignore) {
    compilePath("samples/legacy/PythonTutorial/a_calc.cos")
  }
end SampleTest
