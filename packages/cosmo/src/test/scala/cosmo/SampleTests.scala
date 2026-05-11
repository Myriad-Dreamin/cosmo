package cosmo
import scala.scalajs.js

class SampleTest extends TestBase:
  test("Syntax/playground".ignore) {
    compilePath("samples/Syntax/playground.cos")
  }
  test("HelloWorld".ignore) {
    compilePath("samples/HelloWorld/main.cos")
  }
  test("Syntax/class".ignore) {
    compilePath("samples/Syntax/class.cos")
  }
  test("Syntax/literal".ignore) {
    compilePath("samples/Syntax/literal.cos")
  }
  test("Syntax/cf.syntax".ignore) {
    compilePath("samples/Syntax/cf.syntax.cos")
  }
  test("Syntax/decl.syntax".ignore) {
    compilePath("samples/Syntax/decl.syntax.cos")
  }
  test("Syntax/callExpr.syntax".ignore) {
    compilePath("samples/Syntax/callExpr.syntax.cos")
  }
  test("Syntax/expr.syntax".ignore) {
    compilePath("samples/Syntax/expr.syntax.cos")
  }
  test("Syntax/lambda.syntax".ignore) {
    compilePath("samples/Syntax/lambda.syntax.cos")
  }
  test("Syntax/try-catch.syntax".ignore) {
    compilePath("samples/Syntax/try-catch.syntax.cos")
  }
  test("Syntax/errs/tmplLit01".ignore) {
    compilePath("samples/Syntax/errs/tmplLit01.cos-ast")
  }
  test("Syntax/tmplLit.syntax".ignore) {
    compilePath("samples/Syntax/tmplLit.syntax.cos")
  }
  test("TypeAnnotation/add".ignore) {
    compilePath("samples/TypeAnnotation/add.cos")
  }
  test("Class/basic".ignore) {
    compilePath("samples/Class/basic.cos")
  }
  test("Class/jsonValue".ignore) {
    compilePath("samples/Class/jsonValue.cos")
  }
  test("Class/nat".ignore) { // broken by self ref
    compilePath("samples/Class/nat.cos")
  }
  test("Class/natCons".ignore) { // broken by self ref
    compilePath("samples/Class/natCons.cos")
  }
  test("Class/method".ignore) {
    compilePath("samples/Class/method.cos")
  }
  test("Class/staticMethod".ignore) {
    compilePath("samples/Class/staticMethod.cos")
  }
  test("CompileTime/arith".ignore) {
    compilePath("samples/CompileTime/arith.cos")
  }
  test("CompileTime/addFn".ignore) {
    compilePath("samples/CompileTime/addFn.cos")
  }
  test("CompileTime/recursive".ignore) {
    compilePath("samples/CompileTime/recursive.cos")
  }
  test("Reflect/name".ignore) {
    compilePath("samples/Reflect/name.cos")
  }
  test("Trait/empty".ignore) {
    compilePath("samples/Trait/empty.cos")
  }
  test("Trait/resultProblem".ignore) {
    compilePath("samples/Trait/resultProblem.cos")
  }
  test("Trait/formatter".ignore) {
    compilePath("samples/Trait/formatter.cos")
  }
  test("Trait/formatter_t".ignore) {
    compilePath("samples/Trait/formatter_t.cos")
  }
  test("Trait/display".ignore) {
    compilePath("samples/Trait/display.cos")
  }
  test("Trait/mutSelf".ignore) {
    compilePath("samples/Trait/mutSelf.cos")
  }
  test("Trait/constraint".ignore) {
    compilePath("samples/Trait/constraint.cos")
  }
  test("Trait/iter".ignore) {
    compilePath("samples/Trait/iter.cos")
  }
  test("ControlFlow/loop".ignore) {
    compilePath("samples/ControlFlow/loop.cos")
  }
  test("ControlFlow/forIn".ignore) {
    compilePath("samples/ControlFlow/forIn.cos")
  }
  test("ControlFlow/forInI".ignore) {
    compilePath("samples/ControlFlow/forInI.cos")
  }
  test("ControlFlow/mainIf".ignore) {
    compilePath("samples/ControlFlow/mainIf.cos")
  }
  test("Format/templateLit".ignore) {
    compilePath("samples/Format/templateLit.cos")
  }
  test("Pattern/natAdd".ignore) { // broken by self ref
    compilePath("samples/Pattern/natAdd.cos")
  }
  test("Pattern/option".ignore) {
    compilePath("samples/Pattern/option.cos")
  }
  test("Pattern/result".ignore) {
    compilePath("samples/Pattern/result.cos")
  }
  test("Pattern/byStr".ignore) {
    compilePath("samples/Pattern/byStr.cos")
  }
  test("Reflect/name".ignore) {
    compilePath("samples/Reflect/name.cos")
  }
  test("Io/readFile".ignore) {
    compilePath("samples/Io/readFile.cos")
  }
  test("Vec/push".ignore) {
    compilePath("samples/Vec/push.cos")
  }
  test("PythonTutorial/a_calc".ignore) {
    compilePath("samples/PythonTutorial/a_calc.cos")
  }
end SampleTest
