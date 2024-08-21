package cosmo

class CodeGen(val artifact: Eval) {
  val prelude = """
#include "cosmo-rt.h"
"""

  // Generate Cxx code from the artifact
  def gen(): String = {
    val itemCode = artifact.module.stmts.map(genDef(_, true)).mkString("\n")
    val errorCode = artifact.errors.mkString("\n")
    s"$prelude$itemCode\n#if 0\n$errorCode\n#endif"
  }

  def genDef(ast: ir.Item, topLevel: Boolean): String = {
    ast match {
      case ir.Def(id) => {
        // s"int _${id.id};"
        val defInfo = artifact.defs(id)
        val item = artifact.items(id)
        val name =
          if ((topLevel && defInfo.name == "main") || defInfo.noMangle)
            defInfo.name
          else s"${defInfo.name}${id.id}"
        item match {
          case ir.Fn(params, body) =>
            val paramCode =
              params
                .map(param => s"${ty_(param.ty)} ${param.name}${param.id.id}")
                .mkString(", ")
            val bodyCode = body match {
              case Some(body) => s"{${expr(body)}}"
              case None       => ";"
            }
            s"int $name($paramCode) $bodyCode"
          case _ => s"unknown $name;"
        }
      }
      case ir.Var(id, init, isContant) => {
        val defInfo = artifact.defs(id)
        val name =
          if (topLevel && defInfo.name == "main") "main"
          else s"${defInfo.name}${id.id}"
        var constantStr = if isContant then "const " else ""
        s"${constantStr}int $name = ${expr(init)}"
      }
      case a => expr(a)
    }
  }

  def ty_(ty: Type): String = {
    ty match {
      case IntegerTy(size, isUnsigned) =>
        s"${if (isUnsigned) "u" else ""}int${size}_t"
      case ty => ty.toString
    }
  }

  def expr(ast: ir.Item): String = {
    ast match {
      case ir.Lit(value)    => value.toString
      case ir.Opaque(value) => value
      case ir.Region(stmts) =>
        stmts.map(genDef(_, false)).mkString(";\n") + ";"
      case ir.Variable(id) => {
        val defInfo = artifact.defs(id)
        if defInfo.noMangle then defInfo.name
        else s"${defInfo.name}${id.id}"
      }
      case ir.BinOp(op, lhs, rhs) =>
        s"${expr(lhs)} $op ${expr(rhs)}"
      case ir.Return(value) => s"return ${expr(value)}"
      case ir.Apply(lhs, rhs) =>
        s"${expr(lhs)}(${rhs.map(expr).mkString(", ")})"
      case a => a.toString()
    }
  }
}
