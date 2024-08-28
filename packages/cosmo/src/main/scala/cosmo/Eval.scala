package cosmo

import ir._
import cosmo.system._
import cosmo.FileId

final class DefId(val id: Int) extends AnyVal {
  def defName(env: Env)(implicit topLevel: Boolean): String = {
    val defInfo = env.defs(this)
    if ((topLevel && defInfo.name == "main"))
      defInfo.name
    else defInfo.nameStem(this.id)
  }
}

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

  override def toString() = scopes.toString
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
  var items: Map[DefId, Item] = Map()
  var scopes = new Scopes()
  var errors: List[String] = List()
  var module: ir.Region = Region(List())
  var ns: List[String] = List()

  implicit val system: CosmoSystem = new JsPhysicalSystem()

  def eval(ast: syntax.Block): Env = {
    newBuiltin("println")
    newBuiltin("panic")

    newType("Type", UniverseTy)
    newType("String", StringTy)
    newType("bool", BoolTy)
    List(8, 16, 32, 64).foreach { width =>
      newType(s"i$width", IntegerTy(width, false))
      newType(s"u$width", IntegerTy(width, true))
    }
    List(32, 64, 128).foreach { width =>
      newType(s"f$width", FloatTy(width))
    }

    module = Region(ast.stmts.map(valueExpr))

    this
  }

  def newDefWithInfo(
      name: String,
      noMangle: Boolean = false,
      hidden: Boolean = false,
  ): (DefId, DefInfo) = {
    defAlloc += 1
    val id = new DefId(defAlloc)
    if (!hidden) {
      scopes.set(name, id)
    }
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
    items += (id -> Opaque.expr(s"$name"))
  }

  def newType(name: String, ty: Type) = {
    val id = newDef(name, noMangle = true)
    items += (id -> ty)
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

          val paramTy = ty.map(typeExpr);
          val paramTy2 = paramTy.orElse(
            name match {
              case "self" => Some(SelfTy)
              case _      => None
            },
          );
          paramTy2
            .map { case initTy =>
              defs(id).upperBounds = defs(id).upperBounds :+ initTy
            }
          // todo: infer type from initExpr and body
          // todo: level
          val initExpr = init.map(valueExpr).getOrElse(Variable(name, id, 0))

          items += (id -> initExpr)
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
        Param(p.name, id, defs(id).upperBounds.headOption.getOrElse(TopTy)),
      )
    }
  }

  def block(ast: syntax.Block) = {
    Region(scopes.withScope {
      ast.stmts.map(valueExpr)
    })
  }

  def caseBlock(ast: syntax.CaseBlock) = {
    // , target: ir.Item
    val stmts = scopes.withScope {
      ast.stmts.map { case syntax.Case(cond, body) =>
        val condExpr = valueExpr(cond)
        val bodyExpr = body.map(valueExpr).getOrElse(NoneItem)
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

    def classItem(item: syntax.Node) = item match {
      case syntax.Val(name, ty, init) =>
        vars = vars :+ varItem(name, ty, init, true)(0)
      case syntax.Var(name, ty, init) =>
        vars = vars :+ varItem(name, ty, init, false)(0)
      case d: syntax.Def =>
        defs = defs :+ defItem(d)
      case node =>
        errors = "Invalid class item" :: errors
    }

    scopes.withScope {
      withNs(info.name) {
        ast.stmts.foreach {
          case syntax.Semi(None)       =>
          case syntax.Semi(Some(stmt)) => classItem(stmt)
          case stmt                    => classItem(stmt)
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
  )(implicit level: Int): ir.Var = {
    val initExpr = init.map(valueExpr)
    val id = newDef(name)
    ty.map(typeExpr) match {
      case Some(initTy) =>
        defs(id).upperBounds = defs(id).upperBounds :+ initTy
      case None =>
        defs(id).upperBounds = defs(id).upperBounds :+ TopTy
    }
    val res = ir.Var(id, initExpr.getOrElse(NoneItem), isContant)
    if (level == 0) {
      items += (id -> res)
    } else {
      initExpr.map { initExpr =>
        items += (id -> initExpr)
      }
    }

    res
  }

  def defItem(ast: syntax.Def) = {
    val syntax.Def(name, params, ret_ty, rhs) = ast
    var id = newDef(name)
    // todo: assign items before checking expr for recursive functions
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
            Some(rhs.map(valueExpr).getOrElse(NoneItem)),
          )
      }
    }
    items += (id -> result)
    ir.Def(id)
  }

  def classItem(ast: syntax.Class): ir.Def = {
    val syntax.Class(name, params, body) = ast
    val (id, defInfo) = newDefWithInfo(name)
    // todo: assign items before checking expr for recursive functions
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
    items += (id -> cls)
    ir.Def(id)
  }

  def applyFunc(fn: Fn, args: List[Item])(implicit level: Int): Item = {
    val Fn(params, ret_ty, body) = fn
    ret_ty match {
      case Some(ir.Variable(_, id, _, Some(UniverseTy))) =>
        // println(s"applyFunc at compile time")
        return scopes.withScope {
          // println(s"applyFunc withScope")
          body.map(liftAsType).getOrElse(NoneItem)
        }
      case _ =>
    }

    ir.Apply(fn, args)
  }

  def applyClassItem(node: Class, args: List[Item]): ir.Interface = {
    val Class(rawId, params, vars, defItems) = node
    val rawDefInfo = defs(rawId)
    val (id, defInfo) = newDefWithInfo(rawDefInfo.name, hidden = true)
    val mp = vars.map { v =>
      (defs(v.id).name -> VarField(ir.Def(v.id)))
    } ++ defItems.map { case f =>
      errors = "Invalid class item" :: errors
      ("_bad" -> DefField(ir.Var(newDef("$error"), NoneItem, false)))
    }
    val ty = NativeInsType(NativeType(rawDefInfo.name), args);
    ir.Interface(this, ty, id, Map(mp: _*))
  }

  def applyInterface(iface: Interface, rhs: List[Item]): Item = {
    ClassInstance(iface)
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

    items += (id -> v)

    dest match {
      case Some(syntax.Ident(name)) =>
        val (id, defInfo) = newDefWithInfo(name)
        defs(id).upperBounds = defs(id).upperBounds :+ valTy
        items += (id -> v)
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
    items += (id -> v)

    dest.map {
      case syntax.Ident(name) =>
        val (id, defInfo) = newDefWithInfo(name)
        defs(id).upperBounds = defs(id).upperBounds :+ valTy
        items += (id -> v)
      case v =>
        errors = s"Invalid import destination $v" :: errors
    }

    ir.Def(id)
  }

  def internId(env: Env, id: DefId): DefId = {
    val info = env.defs(id)
    val (newId, newInfo) = newDefWithInfo(info.name)
    newInfo.upperBounds = info.upperBounds
    newInfo.lowerBounds = info.lowerBounds
    newId
  }

  def selectItem(lhs: Item, field: String)(implicit level: Int): Item = {
    // println(s"selectItem $lhs $field")
    var res = lhs match {
      case Variable(_, id, _, v) => v.map(selectItem(_, field))
      case NativeModule(env, fid) =>
        env.scopes.get(field).map(env.items(_)).map(EnvItem(env, _))
      case CModule(kind, path) => return CIdent(field, List())
      case CIdent(ns0, ns)     => return CIdent(field, ns :+ ns0)
      // case ValueTy(NativeModule(_, _)) => NativeType(name)
      // case NativeType(_)               => NativeType(name)
      // case _                           => TopTy
      case _ => None
    }
    // lhsVal match {
    //   // todo: process namespaces
    //   case NativeModule(_, _) => Opaque.expr(rhs.name)
    //   case _                  => Select(lhsItem, rhs.name)
    // }
    // typeExpr(lhs).instantiate match {
    //   case ValueTy(CModule(_, _))      => CIdent(name, List())
    //   case ValueTy(NativeModule(_, _)) => NativeType(name)
    //   case NativeType(_)               => NativeType(name)
    //   case CIdent(ns0, ns)             => CIdent(name, ns :+ ns0)
    //   case _                           => TopTy
    // }
    res.getOrElse(Select(lhs, field))
  }

  def applyItem(lhs: Item, rhs: List[Item])(implicit
      level: Int,
  ): Item = {
    // println(s"applyItem $lhs ${rhs}")
    lhs match {
      case Variable(_, id, _, v) => applyItem(v.getOrElse(items(id)), rhs)
      case EnvItem(env, item) => env.applyItem(item, rhs.map(EnvItem(this, _)))
      case fn: Fn             => applyFunc(fn, rhs)
      case cls: Class         => applyClassItem(cls, rhs)
      case iface: Interface   => applyInterface(iface, rhs)
      case v: CIdent          => CppInsType(v, rhs)
      case _                  => Apply(lhs, rhs)
    }
    // val operands = rhs.map(typeExpr).map {
    //   case Variable(nameHint, id, level, v) => {
    //     val info = env.defs(id)
    //     val name = info.nameStem(id.id)
    //     Variable(name, id, level, v)
    //   }
    //   case ty => ty
    // }
    // // println(s"typeExpr apply ${typeExpr(lhs).instantiate} $operands")
    // typeExpr(lhs).instantiate match {
    //   case c: CIdent     => CppInsType(c, operands)
    //   case c: NativeType => NativeInsType(c, operands)
    //   case _             => TopTy
    // }
  }

  def deref(lhs: Item)(implicit level: Int): Item = {
    lhs match {
      case f: Fn =>
        if (f.params.isEmpty) { applyItem(f, List()) }
        else { lhs }
      case _ => lhs
    }
  }

  def liftAsType(item: Item)(implicit level: Int): Item = {
    // println(s"liftAsType $item")
    item match {
      case Semi(value)  => liftAsType(value)
      case item: CIdent => item
      case _            => item
    }
  }

  def valueExpr(node: syntax.Node)(implicit level: Int = 0): Item = expr(node)
  def typeExpr(node: syntax.Node)(implicit level: Int = 1): Type = expr(node)
  def expr(ast: syntax.Node)(implicit level: Int): ir.Item = {
    ast match {
      case b: syntax.Block            => block(b)
      case l: syntax.Loop             => Loop(expr(l.body))
      case f: syntax.For              => For(f.name, expr(f.iter), expr(f.body))
      case syntax.Semi(None)          => ir.NoneKind(level)
      case syntax.Semi(Some(value))   => Semi(expr(value))
      case syntax.BinOp(op, lhs, rhs) => BinOp(op, expr(lhs), expr(rhs))
      case syntax.Apply(lhs, rhs)     => applyItem(expr(lhs), rhs.map(expr))
      case syntax.Select(lhs, rhs)    => deref(selectItem(expr(lhs), rhs.name))
      case syntax.Return(value)       => Return(expr(value))
      case syntax.Break()             => Break()
      case syntax.Continue()          => Continue()
      case syntax.BoolLit(value)      => Opaque.expr(value.toString)
      case syntax.IntLit(value)       => Opaque.expr(value.toString)
      case syntax.StringLit(value) => Opaque.expr(s"""std::string("$value")""")
      case syntax.TodoLit          => TodoLit
      case syntax.Self             => SelfVal
      case syntax.BigSelf          => SelfTy
      case d: syntax.Def           => defItem(d)
      case c: syntax.Class         => classItem(c)
      case syntax.Import(p, dest)  => importItem(p, dest)
      case syntax.Val(name, ty, init) => varItem(name, ty, init, true)
      case syntax.Var(name, ty, init) => varItem(name, ty, init, false)
      case syntax.If(cond, cont_bb, else_bb) =>
        If(expr(cond), expr(cont_bb), else_bb.map(expr))
      case syntax.Ident(name) =>
        scopes.get(name) match {
          case Some(id) =>
            // val ty = typeExpr(ast).instantiate(2, this)
            Variable(name, id, level, value = items.get(id).map(deref))
          case None =>
            errors = s"Undefined variable $name" :: errors;
            TopTy
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
    valueExpr(ast) match {
      case Opaque(Some(expr), _) => expr
      case _                     => ""
    }
  }

  def storeTy(ty: Type)(implicit topLevel: Boolean): String = {
    // println(s"storeTy $ty")
    ty match {
      case IntegerTy(size, isUnsigned) =>
        s"${if (isUnsigned) "u" else ""}int${size}_t"
      case FloatTy(size)                  => s"float${size}_t"
      case BoolTy                         => "bool"
      case StringTy                       => "CString"
      case SelfTy                         => "self_t"
      case ty: CIdent                     => ty.repr
      case ty: CppInsType                 => ty.repr(storeTy)
      case ty: NativeType                 => ty.repr
      case ty: NativeInsType              => ty.repr(storeTy)
      case EnvItem(env, v)                => env.storeTy(v)
      case classTy: Class                 => classTy.id.defName(this)
      case v: Variable if v.value.isEmpty => v.id.defName(this)
      case Variable(_, _, _, Some(v))     => storeTy(v)
      case ty                             => "auto"
    }
  }
}
