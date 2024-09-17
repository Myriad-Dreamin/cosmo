package cosmo

import ir._
import cosmo.system._
import cosmo.FileId
import cosmo.syntax.Ident
import cosmo.syntax.CaseBlock

type SParam = syntax.Param;
type SParams = List[SParam];

final class DefId(val id: Int) extends AnyVal {}

class Scopes {
  var scopes: List[Map[String, DefInfo]] = List(Map())

  def withScope[T](f: => T): T = {
    scopes = Map() :: scopes; val result = f; scopes = scopes.tail
    result
  }

  def get(name: String): Option[DefInfo] =
    scopes.find(_.contains(name)).flatMap(_.get(name))

  def set(name: String, value: DefInfo) =
    scopes = scopes.updated(0, scopes.head.updated(name, value))

  def remove(name: String) =
    scopes = scopes.updated(0, scopes.head.removed(name))
}

class DefInfo(
    val name: String,
    val namespaces: List[String],
    var id: DefId,
    var env: Env,
    var ty: Type = TopTy,
    var impls: List[Impl] = List(),
    var noMangle: Boolean = false,
    var isVar: Boolean = false,
    var inClass: Boolean = false,
    var isBuiltin: Boolean = false,
    var isDependent: Boolean = true,
) {
  def defName(stem: Boolean = false)(implicit
      topLevel: Boolean,
  ): String = {
    if (noMangle || (topLevel && this.name == "main"))
      this.name
    else if (isVar || stem) this.nameStem(this.id.id)
    else this.fullMangledName(this.id.id)
  }
  def nameStem(disambiguator: Int) =
    if (noMangle) name
    else mangledName(disambiguator)
  def fullName(disambiguator: Int) =
    if (noMangle) name
    else fullMangledName(disambiguator)
  // todo: ${disambiguator}
  def mangledName(disambiguator: Int) = name
  def fullMangledName(disambiguator: Int) =
    val ens = env.fid.map(_.ns).getOrElse(List())
    ((ens ::: namespaces) :+ s"${name}").mkString("::")
  def value = env.items.get(id)
  def instantiateTy = ty // todo: instantiate type
}

object DefInfo {
  def just(id: Int, env: Env) = new DefInfo("", List(), DefId(id), env)
}

class Env(val fid: Option[FileId], pacMgr: cosmo.PackageManager) {
  var defAlloc = DEF_ALLOC_START
  var items: Map[DefId, Item] = Map()
  var scopes = new Scopes()
  var errors: List[String] = List()
  var module: ir.Region = Region(List())
  var ns: List[String] = List()
  var noCore = false
  var selfRef: Option[Item] = None

  implicit val system: CosmoSystem = new JsPhysicalSystem()

  def eval(ast: syntax.Block): Env = {
    if (!noCore) {
      importNative(libPath("std.prelude"), Some(Ident("_")))
    }

    newBuiltin("print")
    newBuiltin("println")

    newType("c_enum", CEnumTy)
    newType("Type", UniverseTy)
    newType("bool", BoolTy)
    newType("self", SelfVal)
    newType("Self", SelfTy)
    newType("Nothing", BottomTy)
    newType("str", StrTy)
    newType("any", TopTy)
    newType("Ref", RefTy(true, false))
    newType("Mut", RefTy(false, true))
    newType("RefMut", RefTy(true, true))
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
    newType("code", Unresolved(DefInfo.just(CODE_FUNC, this)))

    module = Region(ast.stmts.map(valueExpr))

    this
  }

  def newDefWithInfoOr(name: String, id: Option[DefInfo]) = {
    id.foreach(scopes.set(name, _))
    id.getOrElse(ct(name))
  }

  def paramsOr(classSelf: Option[Item], params: Option[SParams]) = {
    classSelf match {
      case Some(cls: ir.Class) => cls.params.map(Right(_))
      case _                   => params.map(Left(_))
    }
  }

  def ct(name: String, hidden: Boolean = false): DefInfo = {
    defAlloc += 1
    val id = new DefId(defAlloc)
    val info =
      new DefInfo(name, ns, id, this)
    if (!hidden) {
      scopes.set(name, info)
    }
    info
  }

  def newBuiltin(name: String) = {
    val info = ct(name); info.noMangle = true; info.isBuiltin = true
    items += (info.id -> Opaque.expr(s"$name"))
  }

  def newType(name: String, ty: Type) = {
    val info = ct(name); info.noMangle = true; info.isBuiltin = true
    items += (info.id -> ty)
  }

  def createInfer(info: DefInfo, lvl: Int) = InferVar(info, level = lvl)

  def withNs[T](ns: String)(f: => T): T = {
    if ns.isEmpty() then return f
    this.ns = ns :: this.ns
    val result = f
    this.ns = this.ns.tail
    result
  }

  def withNsParams[T](ns: String, params: Option[Either[SParams, List[Param]]])(
      f: => T,
  ): T = {
    debugln(s"withNsParams $ns $params")
    withNs(ns) {
      scopes.withScope {
        params.foreach {
          case Right(params) =>
            params.iterator.foreach { case Param(info) =>
              scopes.set(info.name, info)
            }
          case Left(params) =>
            params.iterator.foreach { case SParam(name, ty, init, _) =>
              varItem(ct(name), ty, init.map(valueExpr), false)(0)
            }
        }
        f
      }
    }
  }

  def resolveParams(params: Option[SParams]) = {
    params.map { params =>
      params.map(p =>
        val info = scopes.get(p.name).get;
        // todo: compute canonical type
        Param(info),
      )
    }
  }

  def liftAsType(item: Item)(implicit level: Int): Type = {
    debugln(s"liftAsType $item")
    item match {
      case Semi(value)  => liftAsType(value)
      case item: CIdent => CIdent(item.name, item.ns, 1)
      case item: CppInsType =>
        CppInsType(
          liftAsType(item.target).asInstanceOf[CIdent],
          item.arguments.map(liftAsType),
        )
      case _ => item
    }
  }

  def byName(name: String)(implicit level: Int) = scopes.get(name) match {
    case Some(id) => byRef(id)
    case None =>
      errors = s"Undefined variable $name in $fid" :: errors;
      Unresolved(ct(name))
  }

  def byRef(info: DefInfo)(implicit level: Int): Item = {
    val v = items.get(info.id).map(deref)
    debugln(s"byRef $info ${v.map(_.level)}")
    v match {
      case Some(v: ir.Term) => v
      case _ =>
        Term(info, v.map(_.level).getOrElse(level), value = v)
    }
  }

  def deref(lhs: Item)(implicit level: Int): Item = {
    lhs match {
      case (f: Sig) if f.params.isEmpty    => $apply(f, List())
      case Fn(_, f, _) if f.params.isEmpty => $apply(f, List())
      // case v @ ClassCon(_, v, cls: Class, bounds)
      //     if cls.params.isEmpty && cls.isPhantomClass =>
      //   $apply(v, List())
      case _ => lhs
    }
  }

  def valueExpr(node: syntax.Node)(implicit level: Int = 0): Item = expr(node)
  def typeExpr(node: syntax.Node)(implicit level: Int = 1): Type = expr(node)
  def expr(ast: syntax.Node)(implicit level: Int): ir.Item = {
    ast match {
      // literals
      case syntax.TodoLit        => TodoLit
      case syntax.BoolLit(value) => Opaque.expr(value.toString)
      case syntax.IntLit(value) =>
        if (value.isValidInt) Integer(value.toInt)
        else Opaque.expr(value.toString)
      case syntax.FloatLit(value)  => Opaque.expr(value.toString)
      case syntax.StringLit(value) => Str(value)
      case syntax.Ident("self")    => SelfVal
      case syntax.Ident("Self")    => SelfTy
      case syntax.Ident(name)      => byName(name)
      case syntax.ArgsLit(values) => {
        if (values.exists(_.isInstanceOf[syntax.KeyedArg])) {
          var dict = Map[String, Item]();
          for (v <- values) {
            v match {
              case syntax.KeyedArg(k, v) => dict += (castKey(k) -> expr(v))
              case _ =>
                errors = s"cannot mix pos arg with keyed args" :: errors
            }
          }
          return DictLit(dict)
        }

        var arr = List[Item]();
        for (v <- values) {
          arr = arr :+ expr(v)
        }
        return TupleLit(arr)
      }
      // control flow
      case l: syntax.Loop        => Loop(expr(l.body))
      case w: syntax.While       => While(expr(w.cond), expr(w.body))
      case f: syntax.For         => For(f.name, expr(f.iter), expr(f.body))
      case syntax.Break()        => Break()
      case syntax.Continue()     => Continue()
      case syntax.Return(value)  => Return(expr(value))
      case syntax.If(cond, x, y) => If(expr(cond), expr(x), y.map(expr))
      // expressions
      case b: syntax.Block                           => block(b)
      case b: syntax.Match                           => matchExpr(b)
      case syntax.UnOp("&", syntax.UnOp("mut", lhs)) => RefItem(expr(lhs), true)
      case syntax.UnOp("&", lhs) => RefItem(expr(lhs), false)
      case syntax.UnOp("mut", lhs) =>
        errors = s"mut must be used after &" :: errors
        expr(lhs)
      case syntax.UnOp(op, lhs)       => unOp(op, expr(lhs))
      case syntax.BinOp(op, lhs, rhs) => binOp(op, expr(lhs), expr(rhs))
      case syntax.As(lhs, rhs)        => As(expr(lhs), typeExpr(rhs))
      case syntax.Apply(lhs, rhs)     => $apply(expr(lhs), rhs.map(expr))
      // todo: check is compile time
      case syntax.Select(lhs, rhs, _) => deref(select(expr(lhs), rhs.name))
      case syntax.Semi(None)          => ir.NoneKind(level)
      case syntax.Semi(Some(value))   => Semi(expr(value))
      // todo: decorator
      case syntax.Decorate(syntax.Apply(Ident("noCore"), _), _) =>
        noCore = true
        NoneItem
      case syntax.Decorate(lhs, rhs) => expr(rhs)
      // declarations
      case syntax.Import(p, dest) => $import(p, dest)
      case syntax.Val(x, ty, y)   => varItem(ct(x), ty, y.map(expr), true)
      case syntax.Typ(x, ty, y)   => varItem(ct(x), ty, y.map(typeExpr), true)
      case syntax.Var(x, ty, y)   => varItem(ct(x), ty, y.map(expr), false)
      case d: syntax.Def          => defItem(d, ct(d.name))
      case c: syntax.Class        => classItem(c, Some(classItem(c, None)))
      case d: syntax.Impl         => implItem(d)
      // syntax errors
      case SParam(name, _, _, _) => Opaque.expr(s"panic(\"param: $name\")")
      case syntax.KeyedArg(k, v) => KeyedArg(castKey(k), expr(v))
      case syntax.TmplApply(Ident("a"), rhs) => Rune(rhs.head._1.head.toInt)
      case syntax.TmplApply(Ident("c"), rhs) => Rune(rhs.head._1.head.toInt)
      case syntax.TmplApply(Ident("b"), rhs) => Bytes(rhs.head._1.getBytes())
      case syntax.TmplApply(lhs, rhs) =>
        Opaque.expr(s"0/* tmpl: ${lhs} ${rhs} */")
      case b: syntax.CaseBlock =>
        errors = s"case block without match" :: errors
        Opaque.expr(s"0/* error: case block without match */")
      case syntax.Case(cond, body) =>
        errors = s"case clause without match" :: errors
        Opaque.expr(s"0/* error: case clause without match */")
    }
  }

  def castKey(key: syntax.Node)(implicit level: Int): String = key match {
    case syntax.Ident(name)  => name
    case syntax.StringLit(s) => s
    case _ => {
      errors = s"Invalid key" :: errors;
      ""
    }
  }

  def block(ast: syntax.Block) = Region(scopes.withScope {
    ast.stmts.map(valueExpr)
  })

  def matchExpr(b: syntax.Match)(implicit level: Int): Item = {
    var lhs = expr(b.lhs)
    var lhsTy = lhs match {
      case Term(defInfo, _, v) => Some(defInfo.ty)
      case _                   => None
    }
    val ce = lhsTy.map(isCEnum).getOrElse(false)
    if (ce) {
      return matchCEnum(b)
    }

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

    var vMappings =
      Map[String, List[(EnumDestruct, Option[syntax.Node])]]()

    for (syntax.Case(destructor, body) <- rhs.stmts) {
      val splited = destruct(lhs, destructor);

      splited match {
        case ed: EnumDestruct =>
          // todo: stable toString
          val variantBase = ed.variant.variantOf.get
          val vs = storeTy(variantBase)(false)
          vMappings.get(vs) match {
            case Some(lst) =>
              vMappings = vMappings + (vs -> (lst :+ (ed, body)))
            case None =>
              vMappings = vMappings + (vs -> List((ed, body)))
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

    val (_, cases) = vMappings.head
    val ty = cases.head._1.variant.variantOf.get

    debugln(s"matchExpr mappings $ty => $cases")

    var matchBody = List[(Class, Item)]()
    for ((ed, body) <- cases) {
      val variant = ed.variant
      val bindings = ed.bindings

      matchBody = matchBody :+ (variant, Region(
        body
          .map(body =>
            scopes.withScope {
              // bindings
              variant.vars.zip(bindings).map { (vv, name) =>
                val defInfo = ct(name); defInfo.isVar = true
                defInfo.ty = vv.item.id.ty
                val ty: Type = defInfo.ty
                val tyLvl = ty.level
                val valLvl = (tyLvl - 1).max(0)
                val res = Term(defInfo, valLvl)
                items += (defInfo.id -> res)
              }
              List(EnumDestruct(lhs, variant, bindings, None)) :+ valueExpr(
                body,
              )
            },
          )
          .getOrElse(List()),
      ))
    }

    EnumMatch(lhs, ty, matchBody, Unreachable)
  }

  def matchCEnum(b: syntax.Match)(implicit level: Int): Item = {
    val lhs = expr(b.lhs)
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

    var defaultCase: Option[Item] = None
    val cases = rhs.stmts.flatMap {
      case syntax.Case(syntax.Ident("_"), body) =>
        defaultCase = Some(body.map(valueExpr).getOrElse(NoneItem))
        None
      case syntax.Case(destructor, body) =>
        Some((expr(destructor), body.map(valueExpr).getOrElse(NoneItem)))
    }

    CEnumMatch(lhs, cases, defaultCase)
  }

  def binOp(op: String, lhs: Item, rhs: Item): Item = op match {
    case "<:" => Bool(isSubtype(lhs, rhs))
    case _    => BinOp(op, lhs, rhs)
  }

  def unOp(op: String, lhs: Item): Item = UnOp(op, lhs)

  def destruct(lhs: Item, by: syntax.Node)(implicit level: Int): Item = {
    val (name, rhs) = by match {
      case name: (syntax.Ident | syntax.Select) => (name, List())
      case syntax.Apply(name, rhs)              => (name, rhs)
      case _ => {
        errors = s"Invalid destructor $by" :: errors
        return lhs
      }
    }
    val variant = enumShape(expr(name)) match {
      case Some(v) => v
      case None =>
        errors = s"Invalid enum variant $name" :: errors
        return lhs
    }
    val binding = rhs.map {
      case syntax.Ident(name) => name
      case _                  => ""
    }
    EnumDestruct(lhs, variant, binding, None)
  }

  def $apply(lhs: Item, rhs: List[Item])(implicit level: Int): Item = {
    debugln(s"apply $lhs |||| ${rhs}")
    lhs match {
      case Term(id, _, Some(Unresolved(id2))) if id2.id.id == CODE_FUNC =>
        return rhs.head match {
          case Str(content) => Opaque.stmt(content)
          case s: Opaque    => s
          case _            => Opaque.expr("0 /* code */")
        }
      case Term(id, _, Some(RefTy(isRef, isMut))) =>
        assert(rhs.length == 1)
        return if (isRef) {
          RefItem(rhs.head, isMut)
        } else {
          rhs.head
        }
      case Term(id, _, Some(v)) if v.level > 0 =>
        id.env.$apply(v, rhs)
      case f: Sig    => applyF(f, None, rhs).getOrElse(applyS(f, rhs))
      case f: Fn     => applyF(f.sig, Some(f), rhs).getOrElse(applyS(f, rhs))
      case s: Select => applySM(s, rhs)
      case c: Class  => applyC(c, Some(rhs))
      // case i: Interface => applyI(i, rhs)
      // id: DefInfo,
      // variantOf: Option[Type],
      // base: Class,
      case v: CIdent if rhs.exists(_.level > 0) => CppInsType(v, rhs)
      case BoundField(_, by, _, TypeField(ev: EnumVariant)) =>
        applyE(ev, by, rhs)
      case HKTInstance(ty, syntax) =>
        val res = hktTranspose(syntax, $apply(ty, rhs))
        if (res.level == 0) {
          res
        } else {
          HKTInstance(res, Apply(syntax, rhs))
        }
      case _ => Apply(lhs, rhs)
    }
  }

  def applyF(fn: Sig, info: Option[Fn], args: List[Item]): Option[Item] = {
    val Sig(params, ret_ty, body) = fn
    if (ret_ty.map(_.level).getOrElse(0) <= 1) {
      return None;
    }

    implicit val level = 1;
    return scopes.withScope {
      val castedArgs = params.iterator.flatten.zip(args).map { case (p, a) =>
        val info = p.id
        scopes.set(info.name, info)
        val casted = castTo(a, info.ty)
        items += (info.id -> casted)
        // todo: constrain types
        casted
      }
      val value = body.map(liftAsType).map(evalExpr).getOrElse(NoneItem)
      Some(hktRef(info, castedArgs.toList, value))
    }
  }

  def applyS(f: Item, args: List[Item]): Apply =
    debugln(s"applySyntax $f $args")
    f match {
      case f: Sig => Apply(f, castArgs(f.params, args))
      case f: Fn  => Apply(f, castArgs(f.sig.params, args))
      // todo: free variables inside
      // todo: this is badly refactored
      case bf @ BoundField(lhs, _, _, DefField(f: Fn)) =>
        Apply(bf, castArgs(f.sig.params, args, Some(Left(()))))
      case _ => Apply(f, args)
    }

  def castArgs(
      eParams: Option[List[Param]],
      args: List[Item],
      self: Option[Either[Unit, Item]] = None,
  ): List[Item] = {
    val params = eParams.map { p =>
      self match {
        case None => p
        case Some(Left(())) =>
          if (p.headOption.exists(_.id.name == "self")) {
            p.tail
          } else {
            p
          }
        case Some(Right(self)) => ???
      }
    }
    val paramsLength = params.map(_.length).getOrElse(0)
    if (paramsLength != args.length) {
      println(("self", self))
      errors =
        s"Invalid number of arguments (${paramsLength} v.s. ${args.length}) $params v.s. $args" :: errors
      return args
    }

    var argsPair = params.iterator.flatten.zip(args);
    argsPair.map { case (p, a) =>
      val info = p.id
      val casted = castTo(a, info.ty)
      items += (info.id -> casted)
      // todo: cast type
      casted
    }.toList
  }

  def castTo(item: Item, ty: Type): Item = {
    debugln(s"castTo $item to $ty")
    item
  }

  def applySM(lhs: Select, args: List[Item]): Item = {
    debugln(s"dispatch syntax method $lhs $args")

    Apply(lhs, args)
  }

  def applyM(lhs: Item, field: String, args: List[Item])(implicit
      level: Int,
  ): Item = {
    // val goal = dispatch(lhs, lhs, field, false)
    // debugln(s"dispatch method $lhs $field as $goal")

    // goal match {
    //   case Some(goal, field, casted) if !field.isInstanceOf[TypeField] =>
    //     applyS(BoundField(lhs, goal, casted, field), args)
    //   case _ => $apply(select(lhs, field), args)
    // }
    ???
  }

  def dispatch(
      lhs: Item,
      by: Item,
      field: String,
      casted: Boolean,
  )(implicit level: Int): Option[Item] = {
    debugln(s"dispatch select($field, $casted) of $lhs by $by")
    def contDispatch(by: Item, nextCasted: Boolean) =
      if (nextCasted && casted) {
        None
      } else {
        dispatch(lhs, by, field, nextCasted)
      }

    def dispatchImpls(id: DefInfo): Option[Item] = {
      val impls = id.impls.flatMap { i =>
        i.fields.find(_.item.id.name == field).map(BoundField(lhs, i, true, _))
      }
      debugln(s"dispatchImpls ${id.defName(false)(false)} $field $impls")
      (impls.headOption) match {
        case Some(r) =>
          if (!impls.tail.isEmpty) {
            errors = s"multiple impls for $field" :: errors
          }
          Some(r)
        case _ => None
      }
    }

    by match {
      case SelfVal =>
        val r = selfRef match {
          case Some(r) => r
          case None =>
            errors = s"no self target in this scope" :: errors
            return None
        }
        dispatch(lhs, r, field, casted)
      case Term(_, _, Some(v)) =>
        dispatch(lhs, v, field, casted)
      case Term(id, _, None) =>
        // case Variable(id, _, v) => v.map(id.env.select(_, field))
        debugln(s"dispatch variable $id.$field, ty ${id.ty}")
        None
      case NativeModule(info, env, fid) =>
        return Some(env.scopes.get(field).map(env.byRef).getOrElse {
          errors = s"Undefined item $field in ${info.name}" :: errors
          Unresolved(ct(field))
        })
      case CModule(id, kind, path) => return Some(CIdent(field, List(), level))
      case CIdent(ns0, ns, level) =>
        return Some(CIdent(field, ns :+ ns0, level))
      case i: Var =>
        debugln(s"dispatch var $i.$field, ty ${i.id.ty}")
        i.init.flatMap(dispatch(lhs, _, field, casted))
      case i: Impl =>
        i.fields
          .find(_.item.id.name == field)
          .map(BoundField(lhs, i, casted, _))
          .orElse(
            contDispatch(i.cls, true),
          )
      case c: Class =>
        debugln(s"dispatch class $c.$field")
        c.fields
          .find(_.item.id.name == field)
          .map(BoundField(lhs, c, casted, _))
          .orElse(dispatchImpls(c.id))
      // .orElse(dispatch(lhs, c.base, field, false))
      case ClassInstance(con, _) =>
        debugln(s"dispatch class instance $con.$field")
        dispatch(lhs, con, field, casted)
          .orElse(con.variantOf.flatMap(dispatch(lhs, _, field, true)))
      // case EnumInstance(iface, id, variantOf, base, _) =>
      //   dispatch(lhs, base, field, casted)
      //     .orElse(dispatch(lhs, variantOf, field, true))
      case HKTInstance(ty, syntax) =>
        Some(
          HKTInstance(
            dispatch(lhs, ty, field, casted).get,
            Select(syntax, field),
          ),
        )
      case _ => None
    }
  }

  def applyC(node: Class, args: Option[List[Item]]): Item = {
    debugln(s"applyClass ${node.id} $args")
    val Class(clsInfo, params, baseArgsT, vars, fields, isAbstract, _, _) =
      node
    val baseArgs = baseArgsT.getOrElse(List())
    val ty = (args, params.map(_.length).map(l => l < baseArgs.length)) match {
      case (Some(aas), Some(false)) =>
        return node.copy(args = Some(baseArgs ::: aas))
      case _ => node
    };
    val defInfo = ct(clsInfo.name, hidden = true)
    defInfo.ty = ty

    // Check conflict
    // val existings = Map[String, VField]();
    // def addField(f: VField) = if (existings.contains(f.item.id.name)) {
    //   errors = s"conflict field ${f.item.id.name}" :: errors
    // } else {
    //   existings += (f.item.id.name -> f)
    // }
    // vars.foreach(addField)
    // fields.foreach(addField)

    (args, params.isEmpty) match { // todo: cast fields
      case (Some(args), true) => ClassInstance(node, args)
      case _                  => node
    }
  }

  def applyE(ev: EnumVariant, by: Type, rhs: List[Item])(implicit
      level: Int,
  ): Item = {
    val baseResult = $apply(ev.base, rhs);
    debugln(s"applyEnumVariant $ev $rhs => base $baseResult")

    baseResult match {
      // todo: poor performance
      case ins: ClassInstance =>
        ins.copy(con = ins.con.copy(variantOf = Some(by)))
      case _ =>
        errors = s"Invalid enum variant base $baseResult" :: errors
        NoneItem
    }
  }

  def select(lhs: Item, field: String)(implicit level: Int): Item = {
    debugln(s"selectItem $lhs ${lhs.getClass().getName()} $field")
    val goal = dispatch(lhs, lhs, field, false)
    debugln(s"dispatch select $lhs $field as $goal")
    goal.getOrElse(Select(lhs, field))
  }

  def $import(p: syntax.Node, dest: Option[syntax.Node]): Item = {
    val path = p match {
      case syntax.StringLit(s) => s
      case _: (syntax.Select | syntax.Ident) =>
        return importNative(p, dest)
      case _ =>
        errors = s"Invalid import path" :: errors
        ""
    }
    val (kind, includePath) = if path startsWith "@lib/c++/" then {
      (CModuleKind.Builtin, path.drop(9))
    } else if path.isEmpty then {
      (CModuleKind.Error, "bad import path")
    } else {
      (CModuleKind.Source, path)
    }
    val defInfo = ct("$module")
    importDest(dest, defInfo, CModule(defInfo, kind, includePath))
  }

  def importNative(p: syntax.Node, dest: Option[syntax.Node]): Item = {
    val defInfo = ct(p match {
      case _ if (!dest.isEmpty)                      => "$module"
      case syntax.Select(lhs, syntax.Ident(name), _) => name
      case syntax.Ident(name)                        => name
      case _                                         => "$module"
    })
    val moduleIns = pacMgr.loadModule(p) match {
      case Some((fid, env)) => NativeModule(defInfo, env, fid)
      case None => {
        errors = s"Failed to load module $p" :: errors
        Unresolved(defInfo)
      }
    }
    importDest(dest, defInfo, moduleIns)
  }

  def importDest(dest: Option[syntax.Node], defInfo: DefInfo, v: Item) = {
    val di = dest match {
      case Some(syntax.Ident("_")) =>
        val env = v.asInstanceOf[NativeModule].env
        val exts = env.scopes.scopes.head
        for ((name, info) <- exts.filter(!_._2.isBuiltin)) {
          var c = ct(name)
          // c.ty = info.ty.instantiate
          scopes.set(name, c)
          items += (c.id -> env.byRef(info)(0))
        }

        ct("$module")
      case Some(syntax.Ident(name)) => ct(name)
      case Some(v) =>
        errors = s"Invalid import destination $v" :: errors
        ct("$module")
      case None => defInfo
    }
    di.ty = v
    items += (di.id -> v)
    v
  }

  def varItem(
      defInfo: DefInfo,
      oty: Option[syntax.Node],
      initExprE: Option[Item],
      isContant: Boolean,
  )(implicit level: Int): ir.Var = {
    defInfo.isVar = true;
    val initExpr = initExprE.map(normalizeExpr)
    val initTy = (oty.map(typeExpr), defInfo.name) match {
      case (Some(ty), _)  => Some(ty)
      case (None, "self") => Some(SelfTy)
      case _ => {
        initExpr match {
          case Some(initExpr) => tyOf(initExpr)
          case None =>
            errors =
              s"either typing or initial expression must be provided for variable $defInfo" :: errors
            None
        }
      }
    }
    val valLvl = (initTy.map(_.level).getOrElse(0) - 1).max(0)
    val res = ir.Var(defInfo, initExpr, isContant, valLvl)
    defInfo.ty = initTy.getOrElse(createInfer(defInfo, valLvl + 1))
    items += (defInfo.id -> initExpr.getOrElse(res))
    items += (defInfo.id -> byRef(defInfo))
    res
  }

  def defItem(ast: syntax.Def, defInfo: DefInfo, withBody: Boolean = true) = {
    debugln(s"defItem ${defInfo.name}")
    val syntax.Def(name, params, ret_ty, rhs) = ast
    val sig = withNsParams("", params.map(Left(_))) {
      Sig(
        resolveParams(params),
        ret_ty.map(typeExpr),
        None,
      )
    }
    val f = Fn(defInfo, sig, sig.resolveLevel)
    items += (defInfo.id -> f)

    if (withBody) {
      val annotated = sig.ret_ty
      val body = withNsParams("", params.map(Left(_))) {
        rhs.map(e => normalizeExpr(valueExpr(e)))
      }
      val bodyTy = body.flatMap(tyOf)
      debugln(s"defItem $name, $bodyTy <: $annotated")
      // we have already checked annotated <: bodyTy when we are
      // making valueExpr.
      val sigRetTy = annotated.orElse(bodyTy)

      val sig2 = sig.copy(body = body, ret_ty = sigRetTy)
      val l2 = sig2.resolveLevel;

      defInfo.isDependent =
        if l2 > 0 then body.map(isDependent).getOrElse(false) else false

      val f2 = f.copy(sig = sig2, level = l2)
      items += (defInfo.id -> f2)
      f2
    } else {
      f
    }
  }

  def classItem(ast: syntax.Class, classSelf: Option[Class] = None): Class = {
    val syntax.Class(name, params, body, isAbstract) = ast
    val defInfo = newDefWithInfoOr(name, classSelf.map(_.id))
    val ss = selfRef
    val cls = withNsParams(name, paramsOr(classSelf, params)) {
      classSelf.foreach(cls => selfRef = Some(cls))
      val (vars, restFields) = body match
        case _: syntax.CaseBlock if isAbstract =>
          errors = "Cannot have an enumerated trait" :: errors; (List(), List())
        case body: syntax.Block =>
          baseClass(body, classSelf.map(p => p.vars ::: p.restFields))
        case caseBlock: syntax.CaseBlock =>
          (List(), enumClass(caseBlock, defInfo, classSelf.map(_.restFields)))
        case _ =>
          val kind = if isAbstract then "trait" else "class"
          errors = s"Invalid $kind body" :: errors; (List(), List())
      Class(defInfo, resolveParams(params), None, vars, restFields, isAbstract)
    }
    defInfo.ty = cls
    selfRef = ss
    items += (defInfo.id -> cls)
    cls
  }

  def baseClass(ast: syntax.Block, fields: Option[List[VField]]) = {
    val withBody = !fields.isEmpty
    var vars = List[VarField]();
    var rests = List[VField]();

    val nn = (name: String) => {
      // todo: high complexity
      val oid = fields.iterator.flatten.map(_.item.id).find(x => x.name == name)
      val info = ct(name); info.inClass = true;
      oid.foreach { scopes.set(name, _) }
      oid.getOrElse(info)
    }

    def classItem(item: syntax.Node) = item match {
      // todo: syntax.Typ
      case syntax.Val(x, ty, y) =>
        vars = vars :+ VarField(varItem(nn(x), ty, y.map(valueExpr), true)(0))
      case syntax.Var(x, ty, y) =>
        vars = vars :+ VarField(varItem(nn(x), ty, y.map(valueExpr), false)(0))
      case d: syntax.Def =>
        rests = rests :+ DefField(defItem(d, nn(d.name), withBody = withBody))
      case node =>
        errors = s"Invalid class item $node" :: errors
    }

    ast.stmts.foreach {
      case syntax.Semi(None)       =>
      case syntax.Semi(Some(stmt)) => classItem(stmt)
      case stmt                    => classItem(stmt)
    }

    (vars, rests)
  }

  def enumClass(
      ast: syntax.CaseBlock,
      info: DefInfo,
      fields: Option[List[VField]],
  ) = {
    var subs = fields.iterator.flatten
      .filter(_.isInstanceOf[TypeField])
      .map(_.item.asInstanceOf[EnumVariant])
    var selfRestFields2 = fields.map(_.filter(!_.isInstanceOf[TypeField]))

    val stmts: List[VField] = ast.stmts
      .filter(!_.isWildcard)
      .map(p =>
        TypeField(
          enumVariant(p, info, info.name, classSelf = subs.nextOption()),
        ),
      )
    val restFields = ast.stmts
      .find(_.isWildcard)
      .map { case syntax.Case(_, body) =>
        baseClass(
          // todo: as instance of Block
          body.getOrElse(syntax.Block(List())).asInstanceOf[syntax.Block],
          fields = selfRestFields2,
        )
      } match {
      case Some((vars, restFields)) =>
        if (!vars.isEmpty) {
          errors = s"Invalid default class for enum" :: errors
        }
        restFields
      case None => List()
    }

    restFields ::: stmts
  }

  def enumVariant(
      node: syntax.Case,
      baseId: DefInfo,
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
        val ty = if (n.name == baseName) { syntax.Ident("Self") }
        else { n }
        syntax.Var(s"_${index}", Some(ty), None)
      // todo: replace self
      case (n: syntax.Apply, index) =>
        syntax.Var(s"_${index}", Some(n), None)
      case (_, index) => syntax.Var(s"_${index}", None, None)
    }

    val b = (body, vars) match {
      case (_, Nil) => body.getOrElse(syntax.Block(List()))
      case (Some(syntax.Block(bc)), vars) => syntax.Block(vars ::: bc)
      case (Some(n), vars)                => syntax.Block(vars :+ n)
      case _                              => syntax.Block(vars)
    }

    val cls = classItem(
      // todo: trait enum
      syntax.Class(subName, None, b, false),
      classSelf = classSelf.map(_.base),
    )
    val info = ct(subName); info.inClass = true; cls.id.inClass = true;
    EnumVariant(info, baseId, cls)
  }

  def implItem(ast: syntax.Impl) = {
    val syntax.Impl(rhs, lhs, params, body) = ast
    val defInfo = ct("$impl", hidden = true)
    val ss = selfRef
    val impl = withNsParams("", params.map(Left(_))) {
      val (iface, cls) = (lhs.map(typeExpr), typeExpr(rhs))
      val defs = body match {
        case body: syntax.Block =>
          val (vars, decls) = baseClass(body, None)
          if (!vars.isEmpty) {
            errors = s"impl cannot have vars" :: errors
          }
          selfRef =
            Some(Impl(defInfo, resolveParams(params), iface.get, cls, decls))
          baseClass(body, Some(vars ::: decls))._2
        case _ => errors = s"Invalid impl body" :: errors; List()
      }
      Impl(defInfo, resolveParams(params), iface.get, cls, defs)
    }
    selfRef = ss
    items += (defInfo.id -> impl)
    associateImpl(impl, impl.cls)
    impl
  }

  def associateImpl(impl: Impl, cls: Type): Unit = {
    debugln(s"associateImpl $impl to $cls")
    val id = cls match {
      case cls: Class => cls.id
      case v: Term    => v.id
      case _ =>
        errors = s"Invalid impl target $cls" :: errors
        return
    }
    id.impls = id.impls :+ impl
  }

  def exprOpa(ast: syntax.Node): String = {
    valueExpr(ast) match {
      case Opaque(Some(expr), _) => expr
      case _                     => ""
    }
  }

  def defByName(info: DefInfo): String = info.defName(stem = false)(false)

  def varByRef(vv: Term)(implicit topLevel: Boolean): String = {
    val ir.Term(id, level, v) = vv
    v.map {
      case v: CppInsType => Some(storeTy(v))
      case v: CIdent     => Some(v.repr)
      case _             => None
    }.flatten
      .getOrElse(defByName(id))
  }

  def hktRef(
      f: Option[Fn],
      args: List[Item],
      value: Item,
  ): Item = {
    if f.isEmpty then return value
    def ins(ty: Type) = HKTInstance(ty, Apply(f.get, args))
    value match {
      case _: (CIdent | CppInsType | ClassInstance) => value
      case i: Class                                 => ins(i)
      case _ =>
        throw new Exception(
          s"cannot ref dependent type $value ${value.getClass().getName()}",
        )
    }
  }

  def hktTranspose(syntax: Item, res: Item): Item = {
    debugln(s"hktTranspose $syntax $res")
    res match {
      case ClassInstance(con, args) =>
        val hktCon = con.copy(resolvedAs = Some(syntax))
        ClassInstance(hktCon, args)
      case _ => ???
    }
  }

  def storeTy(ty: Type)(implicit topLevel: Boolean): String = {
    debugln(s"storeTy $ty")
    ty match {
      case IntegerTy(size, isUnsigned) =>
        s"${if (isUnsigned) "u" else ""}int${size}_t"
      case FloatTy(size)   => s"float${size}_t"
      case UnitTy          => "void"
      case BoolTy          => "bool"
      case StrTy           => "::str"
      case SelfTy          => "self_t"
      case TopTy           => "auto"
      case BottomTy        => "void"
      case ty: Integer     => "int32_t"
      case ty: Str         => "::str"
      case ty: CIdent      => ty.repr
      case ty: CppInsType  => ty.repr(storeTy)
      case ty: HKTInstance => ty.repr(storeTy)
      case ty: TupleLit =>
        s"std::tuple<${ty.elems.map(storeTy).mkString(", ")}>"
      case cls: Class if cls.resolvedAs.isDefined =>
        (cls.variantOf, cls.resolvedAs) match {
          case (Some(v), Some(Select(lhs, _))) => storeTy(HKTInstance(v, lhs))
          case _ => storeTy(HKTInstance(cls, cls.resolvedAs.get))
        }
      case cls: Class                 => cls.repr(storeTy)
      case v: Term if v.value.isEmpty => v.id.defName(stem = false)
      case v: Var if v.init.isEmpty   => v.id.defName(stem = false)
      case v: Fn                      => v.id.defName(stem = false)
      case Term(_, _, Some(v))        => storeTy(v)
      case RefItem(lhs, isMut) =>
        s"${if (isMut) "" else "const "}${storeTy(lhs)}&"
      case Apply(lhs, rhs) => {
        val lhsTy = storeTy(lhs)
        val rhsTy = rhs.map(storeTy).mkString(", ")
        s"$lhsTy<$rhsTy>"
      }
      case ty => "auto"
    }
  }

  // : Normalization Part

  def evalExpr(item: Item)(implicit level: Int): Item = {
    debugln(s"evalExpr $item $level")
    val e = evalExpr;
    item match {
      case CppInsType(target, arguments) => CppInsType(target, arguments.map(e))
      case Term(id, lvl, value) if level <= lvl => items(id.id)
      case _                                    => item
    }
  }

  def normalizeExpr(body: Item): Item = {
    debugln(s"normalizeExpr $body")
    body match {
      case Semi(value) => normalizeExpr(value)
      case _           => body
    }
  }

  // : Type Checker Part

  def isDependent(body: Item): Boolean = {
    body match {
      case Semi(value) => isDependent(value)
      case _: (CIdent | CppInsType) =>
        false
      case _ => true
    }
  }

  def isCEnum(ty: Type): Boolean = ty match {
    case Term(id, _, Some(CEnumTy)) => true
    case Term(id, _, _)             => isCEnum(items(id.id))
    case CEnumTy                    => true
    case _                          => false
  }

  def enumShape(ty: Item): Option[Class] = {
    ty match {
      case v: Term if v.value.isEmpty =>
        enumShape(items.getOrElse(v.id.id, NoneItem))
      case Term(_, _, Some(v)) => enumShape(v)
      case BoundField(_, _, _, TypeField(v: EnumVariant)) =>
        Some(v.base.copy(variantOf = Some(v.variantOf.ty)))
      case ClassInstance(con, _)             => enumShape(con)
      case v: Class if v.variantOf.isDefined => Some(v)
      case ty                                => None
    }
  }

  def tyOf(lhs: Item): Option[Type] = {
    debugln(s"tyOf $lhs")
    lhs match {
      case Semi(t)             => tyOf(t)
      case _: Integer          => Some(IntegerTy(32, false))
      case _: Rune             => Some(IntegerTy(32, false))
      case _: Str              => Some(StrTy)
      case _: (Apply | Select) => Some(TopTy)
      case _: (While | Loop | For | Break | Continue) => Some(UnitTy)
      case Unreachable                                => Some(BottomTy)
      case _: (CIdent | Class | CppInsType) =>
        Some(UniverseTy)
      case v: ClassInstance                 => Some(v.con)
      case BoundField(_, _, _, VarField(v)) => Some(v.id.ty)
      case b: BinOp             => coerceTy(tyOf(b.lhs), tyOf(b.rhs))
      case If(_, x, y)          => coerceTy(tyOf(x), y.flatMap(tyOf))
      case Term(id, _, Some(v)) => tyOf(v)
      case v: Var =>
        debugln(s"tyOf(Var) ${v.id.ty}")
        Some(v.id.ty)
      case reg: Region => {
        reg.stmts.lastOption match {
          case Some(Semi(_)) | None => Some(UnitTy)
          case Some(v)              => tyOf(v)
        }
      }
      case EnumMatch(_, _, cases, d) => {
        val types = (cases.map(_._2) :+ d).map(tyOf).flatten
        debugln(s"coerce enumMatch $types")
        types.lastOption
      }
      case CEnumMatch(_, cases, d) => {
        val types = (cases.map(_._2).map(tyOf) :+ d.flatMap(tyOf)).flatten
        debugln(s"coerce enumMatch $types")
        types.lastOption
      }
      case _ =>
        throw new Exception(
          s"program is not well typed, because of $lhs (${lhs.getClass().getName()}).",
        )
    }
  }

  def coerceTy(lhs: Option[Type], rhs: Option[Type]): Option[Type] = {
    // todo: corece correctly
    lhs.orElse(rhs)
  }

  def isSubtype(lhs: Item, rhs: Item): Boolean = {
    debugln(s"isSubtype $lhs $rhs")
    true
  }
}
