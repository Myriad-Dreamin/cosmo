package cosmo

import ir._

final class DefId(val id: Int) extends AnyVal

class Scopes {
  var scopes: List[Map[String, DefId]] = List(Map())

  def push() = {
    scopes = Map() :: scopes
  }

  def pop() = {
    scopes = scopes.tail
  }

  def withScope[T](f: => T): T = {
    push()
    val result = f
    pop()
    result
  }

  def get(name: String): Option[DefId] = {
    scopes.find(_.contains(name)).flatMap(_.get(name))
  }

  def set(name: String, value: DefId) = {
    scopes = scopes.updated(0, scopes.head.updated(name, value))
  }
}

class DefInfo(
    val name: String,
    val noMangle: Boolean,
    var upperBounds: List[Type],
    var lowerBounds: List[Type],
)

class Eval {
  var defAlloc = 0
  var defs: Map[DefId, DefInfo] = Map()
  var items: Map[DefId, Item] = Map()
  var scopes = new Scopes()
  var errors: List[String] = List()
  var module: ir.Region = Region(List())

  def eval(ast: syntax.Block): Eval = {
    newBuiltin("println")

    module = block(ast)
    this
  }

  def newDef(name: String, noMangle: Boolean = false): DefId = {
    defAlloc += 1
    val id = new DefId(defAlloc)
    scopes.set(name, id)
    defs += (id -> new DefInfo(name, noMangle, List(), List()))
    id
  }

  def newBuiltin(name: String) = {
    val id = newDef(name, noMangle = true)
    items += (id -> Opaque(s"$name"))
  }

  def block(ast: syntax.Block) = {
    val stmts = scopes.withScope {
      ast.stmts.map(expr)
    }

    Region(stmts)
  }

  def classBlock(ast: syntax.Block) = {
    val stmts = scopes.withScope {
      ast.stmts.flatMap {
        case syntax.Val(name, ty, init) =>
          Some(varItem(name, ty, init, true))
        case syntax.Var(name, ty, init) =>
          Some(varItem(name, ty, init, false))
        case syntax.Def(name, params, rhs) =>
          Some(defItem(syntax.Def(name, params, rhs)))
        case _ =>
          errors = "Invalid class body" :: errors
          None
      }
    }

    Region(stmts)
  }

  def varItem(
      name: String,
      ty: Option[syntax.Node],
      init: Option[syntax.Node],
      isContant: Boolean,
  ): ir.Item = {
    val initExpr = init.map(expr).getOrElse(NoneItem)
    val id = newDef(name)
    items += (id -> initExpr)
    ir.Var(id, initExpr, isContant)
  }

  def defItem(ast: syntax.Def) = {
    val syntax.Def(name, params, rhs) = ast
    val result = scopes.withScope {
      params.foreach { p =>
        val syntax.Param(name, ty, init) = p

        val id = newDef(name)

        ty.map(ty_).map { case initTy =>
          defs(id).upperBounds = defs(id).upperBounds :+ initTy
        }
        // todo: infer type from initExpr and body
        val initExpr = init.map(expr).getOrElse(Variable(id))

        items += (id -> initExpr)
      }

      val value = expr(rhs)

      Fn(
        params.map(p =>
          val id = scopes.get(p.name).get
          // todo: compute canonical type
          Param(p.name, id, defs(id).upperBounds.head),
        ),
        Some(value),
      )
    }
    var id = newDef(name)
    items += (id -> result)
    ir.Def(id)
  }

  def classItem(ast: syntax.Class) = {
    val syntax.Class(name, body) = ast
    val result = scopes.withScope {
      body match
        case body: syntax.Block =>
          classBlock(body)
        case _ =>
          errors = "Invalid class body" :: errors
          Region(List())
    }
    var id = newDef(name)
    var cls = ir.Class(id, result)
    items += (id -> cls)
    ir.Def(id)
  }

  def ty_(ast: syntax.Node): Type = {
    ast match {
      case syntax.Ident(name) =>
        name match
          case "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" =>
            IntegerTy.parse(name).get
          case _ =>
            TopTy
      case _ => TopTy
    }
  }

  def expr(ast: syntax.Node): ir.Item = {
    ast match {
      case b: syntax.Block => block(b)
      case syntax.Val(name, ty, init) =>
        varItem(name, ty, init, true)
      case syntax.Var(name, ty, init) =>
        varItem(name, ty, init, false)
      case d: syntax.Def =>
        defItem(d)
      case c: syntax.Class =>
        classItem(c)
      case syntax.Literal(value) => Opaque(value.toString)
      case syntax.Ident(name) =>
        scopes.get(name) match {
          case Some(id) => Variable(id)
          case None => errors = s"Undefined variable $name" :: errors; Lit(0)
        }
      case syntax.Param(name, ty, init) =>
        val initExp = init.map(init => s" = ${opa(init)}").getOrElse("")
        Opaque(s"int $name${initExp};")
      case syntax.BinOp(op, lhs, rhs) =>
        BinOp(op, expr(lhs), expr(rhs))
      case syntax.Apply(lhs, rhs) =>
        Apply(expr(lhs), rhs.map(expr))
      case syntax.Return(value) =>
        Return(expr(value))
    }
  }

  def opa(ast: syntax.Node): String = {
    expr(ast) match {
      case Opaque(value) => value
      case _             => ""
    }
  }
}
