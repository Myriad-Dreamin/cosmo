package cosmo

import org.scalajs.dom
import scala.scalajs.js.annotation._
import fastparse._
import fastparse.Parsed

import Parser.root

@main
def CosmoMain() = {}

@JSExportTopLevel("Cosmo")
object Cosmo {
  @JSExport
  def compile(src: String): String = {
    mayCompile(src).getOrElse("#if 0\nCompilation failed\n#endif")
  }
  @JSExport
  def mayCompile(src: String): Option[String] = {
    val parsed = parse(src, root(_), verboseFailures = true)
    val ast = parsed match {
      case Parsed.Success(ast, _) => ast
      case Parsed.Failure(_, index, extra) =>
        println(extra.trace().longAggregateMsg)
        println(src.slice(index, index + 40))
        return None
    }
    val artifact = new Eval().eval(ast)
    for (error <- artifact.errors) {
      println(error)
    }
    val output = new CodeGen(artifact).gen()
    Some(output)
  }
}
