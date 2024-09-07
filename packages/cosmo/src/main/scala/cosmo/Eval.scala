package cosmo

import ir._
import cosmo.system._
import cosmo.FileId
import cosmo.syntax.Ident

final class DefId(val id: Int) extends AnyVal {
  def defName(env: Env, stem: Boolean = false)(implicit
      topLevel: Boolean,
  ): String = {
    val defInfo = env.defs(this)
    if ((topLevel && defInfo.name == "main"))
      defInfo.name
    else if (stem) defInfo.nameStem(this.id)
    else defInfo.fullMangledName(this.id)
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
    var inClass: Boolean = false,
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
  var defAlloc = DEF_ALLOC_START
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
    newType("self", SelfVal)
    newType("Self", SelfTy)
    // todo: size_t
    newType("usize", IntegerTy(64, true))
    newType("isize", IntegerTy(64, false))
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

  def newDefWithInfoOr(name: String, id: Option[DefId]) = {
    id match {
      case Some(id) => {
        scopes.set(name, id)
        (id, defs(id))
      }
      case None => newDefWithInfo(name)
    }
  }

  def paramsOr(classSelf: Option[Item], params: Option[List[syntax.Param]]) = {
    classSelf match {
      case Some(ir.Class(_, params, _, _))     => params.map(Right(_))
      case Some(ir.EnumClass(_, params, _, _)) => params.map(Right(_))
      case _                                   => params.map(Left(_))
    }
  }

  def internId(env: Env, id: DefId): DefId = {
    val info = env.defs(id)
    val (newId, newInfo) = newDefWithInfo(info.name)
    newInfo.upperBounds = info.upperBounds
    newInfo.lowerBounds = info.lowerBounds
    newId
  }

  def newDefWithInfo(
      name: String,
      noMangle: Boolean = false,
      inClass: Boolean = false,
      hidden: Boolean = false,
  ): (DefId, DefInfo) = {
    defAlloc += 1
    val id = new DefId(defAlloc)
    if (!hidden) {
      scopes.set(name, id)
    }
    val info =
      new DefInfo(name, noMangle, ns, List(), List(), inClass = inClass)
    defs += (id -> info)
    (id, info)
  }

  def newDef(
      name: String,
      noMangle: Boolean = false,
      inClass: Boolean = false,
  ): DefId = {
    val (id, info) = newDefWithInfo(name, noMangle, inClass = inClass)
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

  // def setSelfTy(item: Item) = newType("Self", item)

  def withNs[T](ns: String)(f: => T): T = {
    if ns.isEmpty() then return f
    this.ns = ns :: this.ns
    val result = f
    this.ns = this.ns.tail
    result
  }

  def withNsParams[T](
      ns: String,
      params: Option[Either[List[syntax.Param], List[Param]]],
  )(
      f: => T,
  ): T = {
    val ns0 = ns
    // val ns1 = params.map(_.map(_.name)).getOrElse(List())
    withNs(ns0) {
      scopes.withScope {
        params match {
          case None =>
            f
          case Some(Right(params)) =>
            params.iterator.foreach { p =>
              val Param(name, id, ty) = p
              scopes.set(name, id)
            }
            f
          case Some(Left(params)) =>
            params.iterator.foreach { p =>
              val syntax.Param(name, ty, init) = p

              val id = newDef(name)

              val paramTy = ty.map(typeExpr);
              val paramTy2 = paramTy.orElse(
                name match {
                  case "self" => Some(SelfTy)
                  case _      => None
                },
              );
              val tyLvl = paramTy2
                .map { case initTy =>
                  defs(id).upperBounds = defs(id).upperBounds :+ initTy
                  initTy.level
                }
                .getOrElse(0)
              // todo: infer type from initExpr and body
              val valLvl = (tyLvl - 1).max(0)
              val initExpr =
                init.map(valueExpr).getOrElse(Variable(name, id, valLvl))

              items += (id -> initExpr)
            }
            f
        }
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

  def deref(lhs: Item)(implicit level: Int): Item = {
    lhs match {
      case (f: Sig) if f.params.isEmpty    => applyItem(f, List())
      case Fn(_, f, _) if f.params.isEmpty => applyItem(f, List())
      case v @ EnumVariant(_, _, cls: Class)
          if cls.params.isEmpty && cls.vars.isEmpty =>
        applyItem(v, List())
      case _ => lhs
    }
  }

  def liftAsType(item: Item)(implicit level: Int): Item = {
    // debugln(s"liftAsType $item")
    item match {
      case Semi(value)  => liftAsType(value)
      case item: CIdent => item
      case _            => item
    }
  }

  def byRef(name: String, id: DefId)(implicit level: Int): Item = {
    // val ty = typeExpr(ast).instantiate(2, this)
    val v = items.get(id).map(deref)
    // debugln(s"byRef $name $id $v")5
    Variable(name, id, v.map(_.level).getOrElse(level), value = v)
  }

  def xEnv(env: Env, item: Item): Item = {
    // debugln(s"xEnv $item")
    item match {
      case EnvItem(env, v) if env == this => v
      case Apply(lhs, rhs)  => Apply(xEnv(env, lhs), rhs.map(xEnv(env, _)))
      case v: Interface     => v
      case v: ClassInstance => v
      case v: Opaque        => v
      case v: Lit           => v
      case v: Integer       => v
      case v: Str           => v
      case _                => EnvItem(env, item)
    }
  }

  def valueExpr(node: syntax.Node)(implicit level: Int = 0): Item = expr(node)
  def typeExpr(node: syntax.Node)(implicit level: Int = 1): Type = expr(node)
  def expr(ast: syntax.Node)(implicit level: Int): ir.Item = {
    ast match {
      case syntax.TodoLit             => TodoLit
      case syntax.Self                => SelfVal
      case syntax.BigSelf             => SelfTy
      case syntax.Semi(None)          => ir.NoneKind(level)
      case syntax.Semi(Some(value))   => Semi(expr(value))
      case l: syntax.Loop             => Loop(expr(l.body))
      case f: syntax.For              => For(f.name, expr(f.iter), expr(f.body))
      case b: syntax.Block            => block(b)
      case b: syntax.Match            => matchExpr(b)
      case syntax.UnOp(op, lhs)       => UnOp(op, expr(lhs))
      case syntax.BinOp(op, lhs, rhs) => BinOp(op, expr(lhs), expr(rhs))
      case syntax.As(lhs, rhs)        => As(expr(lhs), typeExpr(rhs))
      case syntax.Apply(lhs, rhs)     => applyItem(expr(lhs), rhs.map(expr))
      case syntax.Select(lhs, rhs)    => deref(selectItem(expr(lhs), rhs.name))
      case syntax.Return(value)       => Return(expr(value))
      case syntax.Break()             => Break()
      case syntax.Continue()          => Continue()
      case syntax.BoolLit(value)      => Opaque.expr(value.toString)
      case syntax.IntLit(value)       => Opaque.expr(value.toString)
      case syntax.FloatLit(value)     => Opaque.expr(value.toString)
      case syntax.StringLit(value) => Opaque.expr(s"""std::string("$value")""")
      case syntax.Ident(name) =>
        scopes.get(name) match {
          case Some(id) => byRef(name, id)
          case None =>
            errors = s"Undefined variable $name" :: errors;
            TopTy
        }
      case syntax.Import(p, dest) => importItem(p, dest)
      case syntax.Param(name, ty, init) =>
        val initExp = init.map(init => s" = ${exprOpa(init)}").getOrElse("")
        Opaque(Some(name), Some(s"int $name${initExp};"))
      case syntax.Val(name, ty, init) => varItem(newDef(name), ty, init, true)
      case syntax.Typ(name, ty, init) => varItem(newDef(name), ty, init, true)
      case syntax.Var(name, ty, init) => varItem(newDef(name), ty, init, false)
      case d: syntax.Def              => defItem(d, newDef(d.name))
      case c: syntax.Class =>
        resolveClassItem(c, classItem(c, classSelf = None))
      case t: syntax.Trait       => Opaque.expr(s"0/* trait: ${t} */")
      case syntax.KeyedArg(k, v) => KeyedArg(k, expr(v))
      case syntax.TmplApply(lhs, rhs) =>
        Opaque.expr(s"0/* tmpl: ${lhs} ${rhs} */")
      case syntax.If(cond, cont_bb, else_bb) =>
        If(expr(cond), expr(cont_bb), else_bb.map(expr))
      case b: syntax.CaseBlock =>
        errors = s"case block without match" :: errors
        Opaque.expr(s"0/* error: case block without match */")
      case syntax.Case(cond, body) =>
        errors = s"case clause without match" :: errors
        Opaque.expr(s"0/* error: case clause without match */")
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

  def matchExpr(b: syntax.Match)(implicit level: Int): Item = {
    var lhs = expr(b.lhs)
    val rhs = b.rhs match {
      case b: syntax.CaseBlock => b
      case b: syntax.Block =>
        if (b.stmts.isEmpty) syntax.CaseBlock(List())
        else {
          errors = s"match body contains non-case items" :: errors;
          return Match(lhs, Region(List()))
        }
      case _ => {
        errors = s"Invalid match body" :: errors;
        return Match(lhs, Region(List()))
      }
    }
    debugln(s"matchExpr $lhs on $rhs")

    var vMappings = Map[DefId, List[(EnumDestruct, Option[syntax.Node])]]()

    for (syntax.Case(destructor, body) <- rhs.stmts) {
      val splited = destructItem(lhs, destructor);

      splited match {
        case ed: EnumDestruct =>
          vMappings.get(ed.variant.variantOf) match {
            case Some(lst) =>
              vMappings =
                vMappings + (ed.variant.variantOf -> (lst :+ (ed, body)))
            case None =>
              vMappings = vMappings + (ed.variant.variantOf -> List((ed, body)))
          }
        case _ =>
          errors = s"Invalid destructed item $splited" :: errors
      }
    }

    // assert that there is only one match
    if (vMappings.size != 1) {
      errors = s"not implemented mixed enum match" :: errors
      return Match(lhs, Region(List()))
    }

    val (e, cases) = vMappings.head

    debugln(s"matchExpr mappings $e => $cases")

    var matchBody = List[(EnumVariant, Item)]()
    for ((ed, body) <- cases) {
      val variant = ed.variant
      val bindings = ed.bindings

      // const auto [nn] = std::get<Nat::kIdxSucc>(std::move((*this).data));
      // auto n = std::move(*nn);

      matchBody = matchBody :+ (variant, Region(
        body
          .map(body =>
            scopes.withScope {
              // bindings
              (variant.base match {
                case Class(_, params, vars, defItems) => vars
                case _                                => List()
              }).zip(bindings).map { (vv, name) =>
                val id = newDef(name)
                val ty: Type =
                  defs(vv.id).upperBounds.headOption.getOrElse(TopTy)
                val tyLvl = ty.level
                val valLvl = (tyLvl - 1).max(0)
                val res = Variable(name, id, valLvl)
                defs(id).upperBounds = defs(id).upperBounds :+ ty
                items += (id -> res)
              }
              List(EnumDestruct(lhs, variant, bindings, None)) :+ valueExpr(
                body,
              )
            },
          )
          .getOrElse(List()),
      ))
    }

    EnumMatch(lhs, e, matchBody, Unreachable)
  }

  def destructItem(lhs: Item, destructor: syntax.Node)(implicit
      level: Int,
  ): Item = {
    destructor match {
      case name: (syntax.Ident | syntax.Select) => {
        val variant = enumShape(expr(name)).get;
        EnumDestruct(lhs, variant, List(), None)
      }
      case syntax.Apply(name, rhs) => {
        val variant = enumShape(expr(name)).get;
        EnumDestruct(
          lhs,
          variant,
          rhs.map {
            case syntax.Ident(name) => name
            case _                  => ""
          },
          None,
        )
      }
      case _ => {
        errors = s"Invalid destructor $destructor" :: errors
        lhs
      }
    }
  }

  def enumShape(ty: Item): Option[EnumVariant] = {
    ty match {
      case iface: Interface => None
      case v: Variable if v.value.isEmpty =>
        enumShape(items.getOrElse(v.id, NoneItem))
      case Variable(_, _, _, Some(v)) => enumShape(v)
      case v: ir.EnumInstance         => Some(v.base)
      case v: ir.EnumVariant          => Some(v)
      case ty                         => None
    }
  }

  def applyItem(lhs: Item, rhs: List[Item])(implicit
      level: Int,
  ): Item = {
    debugln(s"applyItem $lhs |||| ${rhs}")
    lhs match {
      case Variable(_, id, _, Some(v)) if v.level > 0 => applyItem(v, rhs)
      case EnvItem(env, item) =>
        xEnv(env, env.applyItem(item, rhs.map(EnvItem(this, _))))
      case fn: Sig          => applyFunc(fn, rhs).getOrElse(Apply(fn, rhs))
      case fn: Fn           => applyFunc(fn.sig, rhs).getOrElse(Apply(fn, rhs))
      case cls: Class       => applyClassItem(cls, Some(rhs))
      case iface: Interface => applyInterface(iface, rhs)
      case ev: EnumVariant  => applyEnumVariant(ev, rhs)
      case v: CIdent if rhs.exists(_.level > 0) => CppInsType(v, rhs)
      case _                                    => Apply(lhs, rhs)
    }
    // val operands = rhs.map(typeExpr).map {
    //   case Variable(nameHint, id, level, v) => {
    //     val info = env.defs(id)
    //     val name = info.nameStem(id.id)
    //     Variable(name, id, level, v)
    //   }
    //   case ty => ty
    // }
    // // debugln(s"typeExpr apply ${typeExpr(lhs).instantiate} $operands")
    // typeExpr(lhs).instantiate match {
    //   case c: CIdent     => CppInsType(c, operands)
    //   case c: NativeType => NativeInsType(c, operands)
    //   case _             => TopTy
    // }
  }

  def applyFunc(fn: Sig, args: List[Item])(implicit
      level: Int,
  ): Option[Item] = {
    val Sig(params, ret_ty, body) = fn
    ret_ty match {
      case Some(ir.Variable(_, id, _, Some(UniverseTy))) =>
        // debugln(s"applyFunc at compile time")
        return scopes.withScope {
          // debugln(s"applyFunc withScope")
          Some(body.map(liftAsType).getOrElse(NoneItem))
        }
      case _ =>
    }

    None
  }

  def applyClassItem(node: Class, args: Option[List[Item]]): Item = {
    debugln(s"applyClassItem ${node.id} $args")
    val Class(clsId, params, vars, defItems) = node

    val rawDefInfo = defs(clsId)
    val (id, defInfo) = newDefWithInfo(rawDefInfo.name, hidden = true)
    val mp = vars.map { v =>
      (defs(v.id).name -> VarField(v))
    } ++ defItems.map { case f =>
      (defs(f.id).name -> DefField(f))
    }
    val classTy = NativeType(rawDefInfo.name);

    val ty = if (params.isEmpty) {
      classTy
    } else {
      args.map(NativeInsType(classTy, _)).getOrElse(classTy)
    };
    val iface = ir.Interface(this, ty, id, clsId, Map(mp: _*))
    if (!params.isEmpty || args.isEmpty) {
      return iface
    }

    return applyInterface(iface, args.getOrElse(List()))
  }

  def applyEnumItem(node: EnumClass, args: Option[List[Item]]): Item = {
    debugln(s"applyEnumItem ${node.id} $args")
    val EnumClass(clsId, params, variants, _) = node

    val rawDefInfo = defs(clsId)
    val (id, defInfo) = newDefWithInfo(rawDefInfo.name, hidden = true)
    val mp = variants.map { v => defs(v.id).name -> TypeField(v) }
    val classTy = NativeType(rawDefInfo.name);

    val ty = if (params.isEmpty) {
      classTy
    } else {
      args.map(NativeInsType(classTy, _)).getOrElse(classTy)
    };
    val iface = ir.Interface(this, ty, id, clsId, Map(mp: _*))
    if (!params.isEmpty || args.isEmpty) {
      return iface
    }

    return applyInterface(iface, args.getOrElse(List()))
  }

  def applyInterface(iface: Interface, rhs: List[Item]): Item = {
    debugln(s"applyInterface $iface $rhs")
    ClassInstance(iface, rhs)
  }

  def applyEnumVariant(ev: EnumVariant, rhs: List[Item])(implicit
      level: Int,
  ): Item = {
    val baseResult = applyItem(ev.base, rhs);
    debugln(s"applyEnumVariant $ev $rhs => base $baseResult")

    baseResult match {
      case ClassInstance(iface, args) => EnumInstance(iface, ev, args)
      case _ =>
        errors = s"Invalid enum variant base $baseResult" :: errors
        NoneItem
    }
  }

  def selectItem(lhs: Item, field: String)(implicit level: Int): Item = {
    // logln(s"selectItem $lhs $field")
    var res = lhs match {
      case Variable(_, id, _, v) => v.map(selectItem(_, field))
      case NativeModule(id, env, fid) =>
        env.scopes.get(field).map(env.byRef(field, _)).map(xEnv(env, _))
      case CModule(id, kind, path) => return CIdent(field, List(), level)
      case CIdent(ns0, ns, level)  => return CIdent(field, ns :+ ns0, level)
      case rawCls: EnumClass =>
        return selectItem(applyEnumItem(rawCls, None), field)
      case cls: Class => return selectItem(applyClassItem(cls, None), field)
      case interface: Interface =>
        interface.fields.get(field).map {
          case DefField(item)  => item
          case TypeField(item) => item
          case VarField(item)  => NoneItem
        }
      // case ValueTy(NativeModule(_, _)) => NativeType(name)
      // case NativeType(_)               => NativeType(name)
      // case _                           => TopTy
      case _ => None
    }
    // typeExpr(lhs).instantiate match {
    // }
    res.getOrElse(Select(lhs, field))
  }

  def importItem(p: syntax.Node, dest: Option[syntax.Node]): Item = {
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

    val v = CModule(id, kind, includePath)
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

    v
  }

  def importNative(p: syntax.Node, dest: Option[syntax.Node]): Item = {
    val (f, m) = pacMgr.loadModule(p) match {
      case Some((fid, env)) => (fid, env)
      case None => {
        errors = s"Failed to load module $p" :: errors
        return Unresolved(newDef("$module"))
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

      // debugln(s"importNative $moduleName")
      moduleName
    } else {
      "$module"
    })

    val v = NativeModule(id, m, f)

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

    v
  }

  def varItem(
      id: DefId,
      ty: Option[syntax.Node],
      init: Option[syntax.Node],
      isContant: Boolean,
  )(implicit level: Int): ir.Var = {
    val initExpr = init.map(valueExpr)
    val tyLvl = ty.map(typeExpr) match {
      case Some(initTy) =>
        defs(id).upperBounds = defs(id).upperBounds :+ initTy
        initTy.level
      case None =>
        defs(id).upperBounds = defs(id).upperBounds :+ TopTy
        1
    }
    val valLvl = (tyLvl - 1).max(0)
    val res = ir.Var(id, initExpr.getOrElse(NoneItem), isContant, valLvl)
    if (level == 0) {
      items += (id -> res)
    } else {
      initExpr.map { initExpr =>
        items += (id -> initExpr)
      }
    }

    res
  }

  def defItem(ast: syntax.Def, id: DefId, withBody: Boolean = true) = {
    val syntax.Def(name, params, ret_ty, rhs) = ast
    // todo: assign items before checking expr for recursive functions
    val sig = withNsParams("", params.map(Left(_))) {
      Sig(
        resolveParams(params),
        ret_ty.map(typeExpr),
        if (withBody) {
          rhs.map(valueExpr)
        } else {
          None
        },
      )
    }
    val f = Fn(id, sig, 0)
    items += (id -> f)
    f
  }

  def resolveClassItem(ast: syntax.Class, item: Item): Item = {
    classItem(ast, classSelf = Some(item))
  }

  def classItem(ast: syntax.Class, classSelf: Option[Item] = None): Item = {
    val classSelfId = classSelf.map {
      case Class(id, _, _, _)     => Some(id)
      case EnumClass(id, _, _, _) => Some(id)
      case _ =>
        errors = s"Invalid class self: $classSelf" :: errors
        None
    }.flatten
    val syntax.Class(name, params, body) = ast
    val (id, defInfo) = newDefWithInfoOr(name, classSelfId)
    // todo: assign items before checking expr for recursive functions
    val cls = withNsParams(name, paramsOr(classSelf, params)) {
      // classSelf.map(setSelfTy)
      body match
        case body: syntax.Block =>
          baseClass(params, body, defInfo, id, classSelf = classSelf)
        case caseBlock: syntax.CaseBlock =>
          enumClass(params, caseBlock, defInfo, id, classSelf = classSelf)
        case _ =>
          errors = "Invalid class body" :: errors
          Class.empty
    }
    items += (id -> cls)
    cls
  }

  def baseClass(
      params: Option[List[syntax.Param]],
      ast: syntax.Block,
      info: DefInfo,
      id: DefId,
      classSelf: Option[Item] = None,
  ) = {
    var vars = List[ir.Var]();
    var defItems = List[ir.Fn]();

    var nnOld = (name: String) =>
      classSelf match {
        case Some(ir.Class(_, params, vars, defItems)) =>
          val oid = vars
            .map(_.id)
            .concat(defItems.map(_.id))
            .find(x => defs(x).name == name)
          oid.map { scopes.set(name, _) }
          oid
        case None => None
        case item => {
          errors = s"Invalid class item predefined: $item" :: errors
          None
        }
      }
    val nn = (name: String) =>
      nnOld(name).getOrElse(newDef(name, inClass = true))

    def classItem(item: syntax.Node) = item match {
      case syntax.Val(name, ty, init) =>
        vars = vars :+ varItem(nn(name), ty, init, true)(0)
      case syntax.Var(name, ty, init) =>
        vars = vars :+ varItem(nn(name), ty, init, false)(0)
      case d: syntax.Def =>
        defItems =
          defItems :+ defItem(d, nn(d.name), withBody = !classSelf.isEmpty)
      case node =>
        errors = "Invalid class item" :: errors
    }

    ast.stmts.foreach {
      case syntax.Semi(None)       =>
      case syntax.Semi(Some(stmt)) => classItem(stmt)
      case stmt                    => classItem(stmt)
    }

    Class(id, resolveParams(params), vars, defItems)
  }

  def enumClass(
      params: Option[List[syntax.Param]],
      ast: syntax.CaseBlock,
      info: DefInfo,
      id: DefId,
      classSelf: Option[Item] = None,
  ) = {
    val fff = classSelf
      .map {
        case ir.EnumClass(_, _, variants, default) => (variants, default)
        case item =>
          errors = s"Invalid class item: $item" :: errors
          (List(), None)
      };
    var subs = fff.map(_._1).iterator.flatten
    val default = fff.map(_._2).flatten

    // val stmts =
    //   ast.stmts.map(e =>
    //     if e.cond == syntax.Ident("_") then
    //       baseClass()
    //     else enumVariant(_, id, info.name, classSelf = subs.nextOption()),
    //   )

    val stmts = ast.stmts
      .filter {
        case syntax.Case(syntax.Ident("_"), _) => false
        case _                                 => true
      }
      .map {
        enumVariant(_, id, info.name, classSelf = subs.nextOption())
      }
    val defaultCls = ast.stmts
      .find {
        case syntax.Case(syntax.Ident("_"), _) => true
        case _                                 => false
      }
      .map { case syntax.Case(_, body) =>
        baseClass(
          params,
          // todo: as instance of Block
          body.getOrElse(syntax.Block(List())).asInstanceOf[syntax.Block],
          info,
          id,
          classSelf = default,
        )
      }

    EnumClass(id, resolveParams(params), stmts, defaultCls)
  }

  def enumVariant(
      node: syntax.Case,
      baseId: DefId,
      baseName: String,
      classSelf: Option[EnumVariant] = None,
  ) = {
    val syntax.Case(cond, body) = node;
    val (subName, params) = cond match {
      case syntax.Ident(name)                       => (name, List())
      case syntax.Apply(syntax.Ident(name), params) => (name, params)
      case _                                        => ("invalid", List())
    }

    val vars = params.zipWithIndex.map {
      case (n: syntax.Ident, index) =>
        val ty = if (n.name == baseName) { syntax.BigSelf }
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

    val cls = classItem(
      syntax.Class(subName, None, b),
      classSelf = classSelf.map(_.base),
    )
    val vid = newDef(subName)
    EnumVariant(vid, baseId, cls)
  }

  def exprOpa(ast: syntax.Node): String = {
    valueExpr(ast) match {
      case Opaque(Some(expr), _) => expr
      case _                     => ""
    }
  }

  def defByName(id: DefId): String = defs(id).nameStem(id.id)

  def varByRef(vv: Variable)(implicit topLevel: Boolean): String = {
    val ir.Variable(n, id, level, v) = vv
    v.map {
      case v: CppInsType => Some(storeTy(v))
      case v: CIdent     => Some(v.repr)
      case _             => None
    }.flatten
      .getOrElse(defByName(id))
  }

  def storeTy(ty: Type)(implicit topLevel: Boolean): String = {
    debugln(s"storeTy $ty")
    ty match {
      case IntegerTy(size, isUnsigned) =>
        s"${if (isUnsigned) "u" else ""}int${size}_t"
      case FloatTy(size)      => s"float${size}_t"
      case BoolTy             => "bool"
      case StringTy           => "CString"
      case SelfTy             => "self_t"
      case ty: CIdent         => ty.repr
      case ty: CppInsType     => ty.repr(storeTy)
      case ty: NativeType     => ty.repr
      case ty: Interface      => storeTy(ty.ty)
      case ty: NativeInsType  => ty.repr(storeTy)
      case EnvItem(env, v)    => env.storeTy(v)
      case classTy: Class     => classTy.id.defName(this, stem = true)
      case classTy: EnumClass => classTy.id.defName(this, stem = true)
      case v: Variable if v.value.isEmpty => v.id.defName(this, stem = true)
      case Variable(_, _, _, Some(v))     => storeTy(v)
      case Apply(lhs, rhs) => {
        val lhsTy = storeTy(lhs)
        val rhsTy = rhs.map(storeTy).mkString(", ")
        s"$lhsTy<$rhsTy>"
      }
      case ty => "auto"
    }
  }
}
