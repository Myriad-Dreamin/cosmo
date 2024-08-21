package cosmo

import ir._
import cosmo.syntax.Block

class Eval {
  var inits: List[Item] = List()
  var funcs: Map[String, Fn] = Map()
  var errors: List[String] = List()

  def eval(ast: syntax.Block): Eval = {
    block(ast, true)
    this
  }

  def block(ast: syntax.Block, topLevel: Boolean) = {
    ast.stmts.foreach {
      case syntax.Val(name, rhs) =>
        val value = expr(rhs)
        if (topLevel) {
          inits = Lit(0) :: inits
        }
        inits = value :: inits
      case syntax.Def(name, params, rhs) =>
        val value = expr(rhs)
        funcs += (name -> Fn(
          params.map(p => Param(p.name, p.ty.map(ty_).getOrElse("int"))),
          Some(value),
        ))
      case _ =>
        errors = "Invalid statement" :: errors
    }
  }

  def ty_(ast: syntax.Node): String = {
    ast match {
      case syntax.Ident("I32") => "int32_t"
      case syntax.Ident(name)  => name
      case _                   => "int"
    }
  }

  def expr(ast: syntax.Node): ir.Item = {
    ast match {
      case Block(stmts)          => Region(stmts.iterator.map(expr).toList)
      case syntax.Literal(value) => Opaque(value.toString)
      case syntax.Ident(name)    => Opaque(name)
      case syntax.Val(name, rhs) =>
        Opaque(s"int $name = ${opa(rhs)};")
      case syntax.Param(name, ty, init) =>
        val initExp = init.map(init => s" = ${opa(init)}").getOrElse("")
        Opaque(s"int $name${initExp};")
      case syntax.Var(name, rhs) =>
        Opaque(s"int $name = ${opa(rhs)};")
      case syntax.BinOp(op, lhs, rhs) =>
        Opaque(
          s"${opa(lhs)} $op ${opa(rhs)}",
        )
      case syntax.Apply(lhs, rhs) =>
        Opaque(s"${opa(lhs)}(${rhs.map(opa).mkString(",")});")
      case syntax.Return(value) =>
        Opaque(s"return ${opa(value)};")
      case syntax.Def(name, params, rhs) => Lit(0)
    }
  }

  def opa(ast: syntax.Node): String = {
    expr(ast) match {
      case Opaque(value) => value
      case _             => ""
    }
  }
}
