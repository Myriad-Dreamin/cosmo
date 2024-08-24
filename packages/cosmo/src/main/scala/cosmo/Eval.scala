package cosmo

import ir._
import cosmo.system._
import cosmo.FileId

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

private val nameJoiner = "::";

class DefInfo(
    val name: String,
    val noMangle: Boolean,
    val namespaces: List[String],
    var upperBounds: List[Type],
    var lowerBounds: List[Type],
) {
  def nameStem(disambiguator: Int) =
    if (noMangle) name
    else mangledName(disambiguator)
  def fullName(disambiguator: Int) =
    if (noMangle) name
    else fullMangledName(disambiguator)
  // todo: ${disambiguator}
  def mangledName(disambiguator: Int) = name
  def fullMangledName(disambiguator: Int) =
    (namespaces :+ s"${name}").mkString(nameJoiner)
}

class Env(pacMgr: cosmo.PackageManager) {
  var defAlloc = 0
  var defs: Map[DefId, DefInfo] = Map()
  var values: Map[DefId, Value] = Map()
  var scopes = new Scopes()
  var errors: List[String] = List()
  var module: ir.Region = Region(List())
  var ns: List[String] = List()

  implicit val system: CosmoSystem = new JsPhysicalSystem()

  def eval(ast: syntax.Block): Env = {
    newBuiltin("println")
    newBuiltin("panic")

    module = block(ast)
    this
  }

  def newDefWithInfo(
      name: String,
      noMangle: Boolean = false,
  ): (DefId, DefInfo) = {
    defAlloc += 1
    val id = new DefId(defAlloc)
    scopes.set(name, id)
    val info = new DefInfo(name, noMangle, ns, List(), List())
    defs += (id -> info)
    (id, info)
  }

  def newDef(name: String, noMangle: Boolean = false): DefId = {
    val (id, info) = newDefWithInfo(name, noMangle)
    id
  }

  def newBuiltin(name: String) = {
    val id = newDef(name, noMangle = true)
    values += (id -> Opaque.expr(s"$name"))
  }

  def withNs[T](ns: String)(f: => T): T = {
    this.ns = ns :: this.ns
    val result = f
    this.ns = this.ns.tail
    result
  }

  def withNsParams[T](ns: String, params: Option[List[syntax.Param]])(
      f: => T,
  ): T = {
    val ns0 = ns
    val ns1 = params.map(_.map(_.name)).getOrElse(List())
    withNs(ns0) {
      scopes.withScope {
        params.iterator.flatten.foreach { p =>
          val syntax.Param(name, ty, init) = p

          val id = newDef(name)

          ty.map(typeExpr).map { case initTy =>
            defs(id).upperBounds = defs(id).upperBounds :+ initTy
          }
          // todo: infer type from initExpr and body
          val initExpr = init.map(expr).getOrElse(Variable(id))

          values += (id -> initExpr)
        }
        f
      }
    }
  }

  def resolveParams(params: Option[List[syntax.Param]]) = {
    params.map { params =>
      params.map(p =>
        val id = scopes.get(p.name).get
        // todo: compute canonical type
        Param(p.name, id, defs(id).upperBounds.head),
      )
    }
  }

  def block(ast: syntax.Block) = {
    val stmts = scopes.withScope {
      ast.stmts.map(expr)
    }

    Region(stmts)
  }

  def caseBlock(ast: syntax.CaseBlock) = {
    // , target: ir.Item
    val stmts = scopes.withScope {
      ast.stmts.map { case syntax.Case(cond, body) =>
        val condExpr = expr(cond)
        val bodyExpr = body.map(expr).getOrElse(NoneItem)
        Case(condExpr, bodyExpr)
      }
    }

    Region(stmts)
  }

  def baseClass(
      params: Option[List[syntax.Param]],
      ast: syntax.Block,
      info: DefInfo,
      id: DefId,
  ) = {
    var vars = List[ir.Var]();
    var defs = List[ir.Item]();
    scopes.withScope {
      withNs(info.name) {
        ast.stmts.foreach {
          case syntax.Val(name, ty, init) =>
            vars = vars :+ varItem(name, ty, init, true)
          case syntax.Var(name, ty, init) =>
            vars = vars :+ varItem(name, ty, init, false)
          case d: syntax.Def =>
            defs = defs :+ defItem(d)
          case _ =>
            errors = "Invalid class body" :: errors
        }
      }
    }

    Class(id, resolveParams(params), vars, defs)
  }

  def enumClass(
      params: Option[List[syntax.Param]],
      ast: syntax.CaseBlock,
      info: DefInfo,
      id: DefId,
  ) = {
    val stmts = scopes.withScope {
      withNs(info.name) {
        ast.stmts.map(enumVariant(_, info.name))
      }
    }

    EnumClass(id, resolveParams(params), stmts, None)
  }

  def enumVariant(node: syntax.Case, baseName: String) = {
    val syntax.Case(cond, body) = node;
    val (subName, params) = cond match {
      case syntax.Ident(name)                       => (name, List())
      case syntax.Apply(syntax.Ident(name), params) => (name, params)
      case _                                        => ("invalid", List())
    }

    val vars = params.zipWithIndex.map {
      case (n: syntax.Ident, index) =>
        val ty = if (n.name == baseName) { syntax.Self }
        else { n }
        syntax.Var(s"_${index}", Some(ty), None)
      // todo: replace self
      case (n: syntax.Apply, index) =>
        syntax.Var(s"_${index}", Some(n), None)
      case (_, index) => syntax.Var(s"_${index}", None, None)
    }

    val b = (body, vars) match {
      case (_, Nil) => body.getOrElse(syntax.Block(List()))
      case (Some(syntax.Block(bc)), vars) => syntax.Block(vars ++ bc)
      case (Some(n), vars)                => syntax.Block(vars :+ n)
      case _                              => syntax.Block(vars)
    }

    classItem(syntax.Class(subName, None, b))
  }

  def varItem(
      name: String,
      ty: Option[syntax.Node],
      init: Option[syntax.Node],
      isContant: Boolean,
  ): ir.Var = {
    val initExpr = init.map(expr)
    val id = newDef(name)
    initExpr.map { initExpr =>
      values += (id -> initExpr)
    }
    ty.map(typeExpr) match {
      case Some(initTy) =>
        defs(id).upperBounds = defs(id).upperBounds :+ initTy
      case None =>
        defs(id).upperBounds = defs(id).upperBounds :+ TopTy
    }
    ir.Var(id, initExpr.getOrElse(NoneItem), isContant)
  }

  def defItem(ast: syntax.Def) = {
    val syntax.Def(name, params, ret_ty, rhs) = ast
    var id = newDef(name)
    // todo: assign values before checking expr for recursive functions
    val result = withNsParams(name, params) {
      val ret = ret_ty.map(typeExpr)
      (params, ret) match {
        case (None, Some(UniverseTy)) =>
          rhs.map(typeExpr) match {
            case Some(ty) => TypeAlias(ty)
            case None =>
              errors = "Invalid type alias" :: errors; TypeAlias(TopTy)
          }
        case _ =>
          Fn(
            resolveParams(params),
            ret,
            Some(rhs.map(expr).getOrElse(NoneItem)),
          )
      }
    }
    values += (id -> result)
    ir.Def(id)
  }

  def classItem(ast: syntax.Class): ir.Def = {
    val syntax.Class(name, params, body) = ast
    val (id, defInfo) = newDefWithInfo(name)
    // todo: assign values before checking expr for recursive functions
    val cls = withNsParams(name, params) {
      body match
        case body: syntax.Block =>
          baseClass(params, body, defInfo, id)
        case caseBlock: syntax.CaseBlock =>
          enumClass(params, caseBlock, defInfo, id)
        case _ =>
          errors = "Invalid class body" :: errors
          Class.empty
    }
    values += (id -> cls)
    ir.Def(id)
  }

  def importNative(p: syntax.Node, dest: Option[syntax.Node]): ir.Def = {
    val (f, m) = pacMgr.loadModule(p) match {
      case Some((fid, env)) => (fid, env)
      case None => {
        errors = s"Failed to load module $p" :: errors
        return ir.Def(newDef("$module"))
      }
    }

    val (id, defInfo) = newDefWithInfo(if (dest.isEmpty) {
      val moduleName = p match {
        case syntax.Select(lhs, syntax.Ident(name)) =>
          name
        case syntax.Ident(name) =>
          name
        case _ =>
          "$module"
      }

      // println(s"importNative $moduleName")
      moduleName
    } else {
      "$module"
    })

    val v = NativeModule(m, f)

    val valTy = ValueTy(v)

    values += (id -> v)

    dest match {
      case Some(syntax.Ident(name)) =>
        val (id, defInfo) = newDefWithInfo(name)
        defs(id).upperBounds = defs(id).upperBounds :+ valTy
        values += (id -> v)
      case Some(v) =>
        errors = s"Invalid import destination $v" :: errors
      case None => {
        defs(id).upperBounds = defs(id).upperBounds :+ valTy
      }
    }

    ir.Def(id)
  }

  def importItem(p: syntax.Node, dest: Option[syntax.Node]): ir.Def = {
    val path = p match {
      case syntax.StringLit(s) =>
        s
      case syntax.Select(_, _) | syntax.Ident(_) =>
        return importNative(p, dest)
      case _ =>
        errors = s"Invalid import path" :: errors
        ""
    }

    val (kind, includePath) = if path startsWith "libc++/" then {
      (CModuleKind.Builtin, path.drop(7))
    } else if path.isEmpty then {
      (CModuleKind.Error, "bad import path")
    } else {
      (CModuleKind.Source, path)
    }
    val (id, defInfo) = newDefWithInfo("$module")

    val v = CModule(kind, includePath)
    val valTy = ValueTy(v)
    values += (id -> v)

    dest.map {
      case syntax.Ident(name) =>
        val (id, defInfo) = newDefWithInfo(name)
        defs(id).upperBounds = defs(id).upperBounds :+ valTy
        values += (id -> v)
      case v =>
        errors = s"Invalid import destination $v" :: errors
    }

    ir.Def(id)
  }

  def selectItem(lhs: syntax.Node, rhs: syntax.Ident): ir.Item = {
    val lhsItem = expr(lhs)
    val lhsVal = lhsItem match {
      case Variable(id) => values(id)
      case _            => return Select(lhsItem, rhs.name)
    }
    lhsVal match {
      // todo: process namespaces
      case NativeModule(_, _) => Opaque.expr(rhs.name)
      case _                  => Select(lhsItem, rhs.name)
    }
  }

  def typeExpr(node: syntax.Node): Type = {
    implicit val env = this
    implicit val level = 1
    // println(s"typeExpr $node")
    node match {
      case syntax.Semi(None)        => TopTy
      case syntax.Semi(Some(value)) => typeExpr(value)
      case syntax.Select(lhs, syntax.Ident(name)) =>
        typeExpr(lhs).instantiate match {
          case ValueTy(CModule(_, _))      => CppType(name, List())
          case ValueTy(NativeModule(_, _)) => NativeType(name)
          case NativeType(_)               => NativeType(name)
          case CppType(ns0, ns)            => CppType(name, ns :+ ns0)
          case _                           => TopTy
        }
      case syntax.Apply(lhs, rhs) =>
        val operands = rhs.map(typeExpr).map {
          case TypeVariable(nameHint, defId) => {
            val info = env.defs(defId)
            val name = info.nameStem(defId.id)
            TypeVariable(name, defId)
          }
          case ty => ty
        }
        // println(s"typeExpr apply ${typeExpr(lhs).instantiate} $operands")
        typeExpr(lhs).instantiate match {
          case c: CppType    => CppInsType(c, operands)
          case c: NativeType => NativeInsType(c, operands)
          case _             => TopTy
        }
      case syntax.Ident(name) =>
        name match
          case "Type"   => UniverseTy
          case "String" => StringTy
          case "bool"   => BoolTy
          case "i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" =>
            IntegerTy.parse(name).get
          case "f32" | "f64" | "f128" =>
            FloatTy.parse(name).get
          case name =>
            scopes.get(name) match {
              case Some(id) =>
                TypeVariable(name, id)
              case None =>
                TopTy
            }
      case syntax.Self => SelfTy
      case _           => TopTy
    }
  }

  def expr(ast: syntax.Node): ir.Item = {
    ast match {
      case b: syntax.Block            => block(b)
      case l: syntax.Loop             => Loop(expr(l.body))
      case f: syntax.For              => For(f.name, expr(f.iter), expr(f.body))
      case syntax.Semi(None)          => ir.NoneItem
      case syntax.Semi(Some(value))   => Semi(expr(value))
      case syntax.BinOp(op, lhs, rhs) => BinOp(op, expr(lhs), expr(rhs))
      case syntax.Apply(lhs, rhs)     => Apply(expr(lhs), rhs.map(expr))
      case syntax.Select(lhs, rhs)    => selectItem(lhs, rhs)
      case syntax.Return(value)       => Return(expr(value))
      case syntax.Break()             => Break()
      case syntax.Continue()          => Continue()
      case syntax.BoolLit(value)      => Opaque.expr(value.toString)
      case syntax.IntLit(value)       => Opaque.expr(value.toString)
      case syntax.StringLit(value) => Opaque.expr(s"""std::string("$value")""")
      case syntax.TodoLit          => TodoLit
      case syntax.Self             => SelfItem
      case d: syntax.Def           => defItem(d)
      case c: syntax.Class         => classItem(c)
      case syntax.Import(p, dest)  => importItem(p, dest)
      case syntax.Val(name, ty, init) => varItem(name, ty, init, true)
      case syntax.Var(name, ty, init) => varItem(name, ty, init, false)
      case syntax.If(cond, cont_bb, else_bb) =>
        If(expr(cond), expr(cont_bb), else_bb.map(expr))
      case syntax.Ident(name) =>
        scopes.get(name) match {
          case Some(id) => {
            // get type of id
            val info = defs(id)
            val ty = typeExpr(ast).instantiate(2, this)

            if (ty.level >= 2) {
              TypeVarRef(id)
            } else {
              Variable(id)
            }
          }
          case None => errors = s"Undefined variable $name" :: errors; Lit(0)
        }
      case b: syntax.CaseBlock =>
        errors = s"case block without match" :: errors
        Opaque.expr(s"0/* error: case block without match */")
      case syntax.Param(name, ty, init) =>
        val initExp = init.map(init => s" = ${exprOpa(init)}").getOrElse("")
        Opaque(Some(name), Some(s"int $name${initExp};"))
      case syntax.Case(cond, body) =>
        errors = s"case clause without match" :: errors
        Opaque.expr(s"0/* error: case clause without match */")
      case b: syntax.Match =>
        val lhs = expr(b.lhs)
        val rhs = b.rhs match {
          case b: syntax.CaseBlock => caseBlock(b)
          case b: syntax.Block =>
            if (b.stmts.isEmpty) Region(List())
            else {
              errors = s"match body contains non-case items" :: errors;
              Region(List())
            }
          case _ => {
            errors = s"Invalid match body" :: errors;
            Region(List())
          }
        }
        Match(lhs, rhs)
    }
  }

  def exprOpa(ast: syntax.Node): String = {
    expr(ast) match {
      case Opaque(Some(expr), _) => expr
      case _                     => ""
    }
  }
}
