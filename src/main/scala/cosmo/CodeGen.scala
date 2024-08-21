package cosmo

class CodeGen {
  val prelude = """
#include "cosmo-rt.h"
    """

  // Generate Cxx code from the artifact
  def gen(artifact: Eval): String = {
    val inits = artifact.inits.reverse
    val funcs = artifact.funcs.iterator
    val errors = artifact.errors
    val initCode = inits
      .map {
        case ir.Lit(value) => s"int _${value};"
        case _             => ""
      }
      .mkString("\n")
    val funcCode = funcs
      .map { (item) =>
        var (name, ir.Fn(params, body)) = item
        val paramCode =
          params.map(param => s"${param.ty} ${param.name}").mkString(", ")
        val bodyCode = body match
          case Some(body) => s"{${expr(body)}}"
          case None       => ";"
        s"int $name($paramCode) $bodyCode"
      }
      .mkString("\n")
    val errorCode = errors.mkString("\n")
    s"$prelude$initCode\n$funcCode\n#if 0\n$errorCode\n#endif"
  }

  def expr(ast: ir.Item): String = {
    ast match {
      case ir.Lit(value)    => value.toString
      case ir.Opaque(value) => value
      case ir.Region(stmts) =>
        stmts.map(expr).mkString("\n")
      case _ => ""
    }
  }
}
