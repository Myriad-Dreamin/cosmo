package cosmo

import ir._
import cosmo.system._
import cosmo.FileId
import cosmo.syntax.Ident
import cosmo.syntax.CaseBlock
import cosmo.syntax.FloatLit

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
    var isOverride: Boolean = false,
    var isVirtual: Boolean = false,
    var isConstantVal: Boolean = true,
) {
  def defName(stem: Boolean = false): String = {
    if (noMangle) this.name
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
  var builtinClasses = Map[Item, Class]()
  var selfRef: Option[Item] = None
  var selfImplRef: Option[Item] = None

  implicit val system: CosmoSystem = new JsPhysicalSystem()

  def builtins() = {
    newBuiltin("print")
    newBuiltin("println")

    newType("c_enum", CEnumTy)
    newType("Type", UniverseTy)
    newType("bool", BoolTy)
    newType("self", SelfVal, SelfTy)
    newType("Self", SelfTy, UniverseTy)
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
    this
  }

  def eval(ast: syntax.Block): Env = {
    if (!noCore) {
      importNative(libPath("std.prelude"), Some(Ident("_")))
    }

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

  def newType(name: String, ty: Type, tyty: Type = UniverseTy) = {
    val info = ct(name); info.noMangle = true; info.isBuiltin = true
    info.ty = tyty
    builtinClasses += (ty -> Class.empty(this, false).copy(id = info))
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
            params.iterator.foreach { case Param(info, _) =>
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
        Param(info, (info.ty.level - 1).max(0)),
      )
    }
  }

  def liftAsType(item: Item): Type = {
    debugln(s"liftAsType $item")
    item match {
      case Semi(value)  => liftAsType(value)
      case item: CIdent => CIdent(item.name, item.ns, 1)
      case item: CppInsType =>
        CppInsType(
          liftAsType(item.target).asInstanceOf[CIdent],
          item.arguments.map(liftAsType),
        )
      case SelfVal => SelfTy
      case _       => item
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

  def derefPtr(lhs: syntax.Node)(implicit level: Int): Item = {
    lhs match {
      case Ident("self") => SelfVal
      case lhs           => UnOp("*", expr(lhs))
    }
  }

  def deref(lhs: Item)(implicit level: Int): Item = {
    lhs match {
      case (f: Sig) if f.params.isEmpty    => $apply(f, List())
      case Fn(_, f, _) if f.params.isEmpty => $apply(f, List())
      case BoundField(_, by, _, TypeField(ev: EnumVariant))
          if ev.base.justInit =>
        $apply(lhs, List())
      case cls: Class if cls.justInit =>
        $apply(cls, List())
      case _ => lhs
    }
  }

  def valueExpr(node: syntax.Node)(implicit level: Int = 0): Item = expr(node)
  def typeExpr(node: syntax.Node)(implicit level: Int = 1): Type = expr(node)
  def expr(ast: syntax.Node)(implicit level: Int): ir.Item = {
    ast match {
      // literals
      case syntax.TodoLit        => TodoLit
      case syntax.BoolLit(value) => Bool(value)
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
        errors = s"mut must be used after &" :: errors; expr(lhs)
      case syntax.UnOp("*", lhs)      => derefPtr(lhs)
      case syntax.UnOp(op, lhs)       => UnOp(op, expr(lhs))
      case syntax.BinOp(op, lhs, rhs) => binOp(op, expr(lhs), expr(rhs))
      case syntax.As(lhs, rhs)        => As(expr(lhs), typeExpr(rhs))
      // todo: check is compile time
      case syntax.Select(lhs, rhs, _) => deref(select(expr(lhs), rhs.name))
      case syntax.Apply(lhs, rhs)     => $apply(expr(lhs), rhs.map(expr))
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

      val stmts = body.map(body =>
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
          List(EnumDestruct(lhs, variant, bindings, None)) :+ valueExpr(body)
        },
      );
      matchBody = matchBody :+ (variant, Region(stmts.getOrElse(List())))
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

  def binOp(op: String, lhs: Item, rhs: Item): Item = op match {
    case "<:" => Bool(isSubtype(lhs, rhs))
    case _    => BinOp(op, lhs, rhs)
  }

  def select(lhs: Item, field: String)(implicit level: Int): Item = {
    debugln(s"selectItem $lhs ${lhs.getClass().getName()} $field")
    val x = dispatch(lhs, lhs, field, false)
    debugln(s"selectItem $lhs $field => $x")
    x.getOrElse(Select(lhs, field))
  }

  def dispatch(lhs: Item, by: Item, field: String, casted: Boolean)(implicit
      level: Int,
  ): Option[Item] = {
    debugln(s"dispatch select($field, $casted) of $lhs by $by")
    def contDispatch(by: Item, nextCasted: Boolean) =
      if (nextCasted && casted) then None
      else dispatch(lhs, by, field, nextCasted)

    def ls(b: BoundField) = b match {
      case b @ BoundField(s @ SelfVal, _, _, DefField(f)) =>
        val s2 = f.sig.selfIsMut match {
          case Some(true)  => RefItem(s, true) // todo: check self mutability
          case Some(false) => RefItem(s, false)
          case _           => SelfTy
        }
        b.copy(lhs = s2)
      case b @ BoundField(RefItem(s @ SelfVal, isMut), _, _, DefField(f)) =>
        val s2 = f.sig.selfIsMut match {
          case Some(true) =>
            if (!isMut) {
              errors = s"self is not mutable" :: errors
            }
            RefItem(s, isMut)
          case Some(false) => RefItem(s, isMut)
          case _           => SelfTy
        }
        b.copy(lhs = s2)
      case b => b
    }
    def dFields(by: Item, v: Iterable[VField]) =
      v.find(_.name == field).map(BoundField(lhs, by, casted, _)).map(ls)

    def dImpls(id: DefInfo): Option[Item] = {
      val impls = id.impls.flatMap { i =>
        i.fields.find(_.name == field).map(BoundField(lhs, i, true, _)).map(ls)
      }
      if (impls.headOption.isDefined && !impls.tail.isEmpty) {
        errors = s"multiple impls for $field $impls" :: errors
      }
      impls.headOption
    }

    by match {
      case SelfVal =>
        selfRef match {
          case Some(r) => dispatch(lhs, r, field, casted)
          case None =>
            errors = s"no self target in this scope" :: errors; None
        }
      case RefItem(r, isMut)   => dispatch(lhs, r, field, casted)
      case Term(_, _, Some(v)) => dispatch(lhs, v, field, casted)
      case Term(id, _, None)   => dispatch(lhs, id.ty, field, casted)
      case NativeModule(info, env, fid) =>
        return Some(env.scopes.get(field).map(env.byRef).getOrElse {
          errors = s"Undefined item $field in ${info.name}" :: errors
          Unresolved(ct(field))
        })
      case CModule(id, kind, path) => return Some(CIdent(field, List(), level))
      case CIdent(ns0, ns, lvl)    => return Some(CIdent(field, ns :+ ns0, lvl))
      case i: Var                  => dispatch(lhs, i.id.ty, field, casted)
      case i: Impl  => dFields(i, i.fields).orElse(contDispatch(i.cls, true))
      case c: Class => dFields(c, c.fields).orElse(dImpls(c.id))
      case ClassInstance(con, _) =>
        dispatch(lhs, con, field, casted)
          .orElse(con.variantOf.flatMap(dispatch(lhs, _, field, true)))
      case HKTInstance(ty, syntax) =>
        val vRes = dispatch(lhs, ty, field, casted).get
        Some(HKTInstance(vRes, Select(syntax, field)))
      case _ => None
    }
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
          val r = rhs.head;
          checkMutability(r, isMut);
          RefItem(r, isMut)
        } else {
          rhs.head
        }
      case Term(id, _, Some(v))                 => $apply(v, rhs)
      case v: CIdent if rhs.exists(_.level > 0) => CppInsType(v, rhs)
      case f: Sig                               => applyF(f, None, rhs)
      case f: Fn                                => applyF(f.sig, Some(f), rhs)
      case c: Class                             => applyC(c, Some(rhs))
      case BoundField(_, by, _, TypeField(ev: EnumVariant)) =>
        $apply(ev.base.copy(variantOf = Some(by)), rhs)
      case BoundField(that, by, _, DefField(f)) =>
        Apply(lhs, castArgs(f.sig.params, rhs, Some(Right(that))))
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

  def applyF(fn: Sig, info: Option[Fn], args: List[Item]): Item = {
    val Sig(params, ret_ty, body) = fn
    if (ret_ty.map(_.level).getOrElse(0) <= 1) {
      return Apply(info.getOrElse(fn), castArgs(fn.params, args));
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
      hktRef(info, castedArgs.toList, value)
    }
  }

  def applyC(node: Class, args: Option[List[Item]]): Item = {
    debugln(s"applyClass ${node.id} ${node.variantOf} $args")
    val Class(clsInfo, params, baseArgsT, vars, fields, isAbstract, _, _) =
      node
    val baseArgs = baseArgsT.getOrElse(List())
    val isTypeLevel = params.map(_.length).map(l => baseArgs.length < l);
    args match {
      case Some(args) if isTypeLevel.getOrElse(false) =>
        return node.copy(args = Some(baseArgs ::: args))
      case Some(args)             => ClassInstance(node, args)
      case None if params.isEmpty => ClassInstance(node, List())
      case _                      => node
    };
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
      isConstant: Boolean,
  )(implicit level: Int): ir.Var = {
    defInfo.isVar = true;
    val initExpr = initExprE.map(normalizeExpr)
    val initTy = (oty.map(typeExpr), defInfo.name) match {
      case (Some(ty), _)  => Some(ty)
      case (None, "self") => Some(RefItem(SelfTy, false))
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
    val res = ir.Var(defInfo, initExpr, isConstant, valLvl)
    defInfo.isConstantVal = isConstant
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
      if (isAbstract) {
        restFields.foreach {
          case DefField(f) => f.id.isVirtual = true
          case _           =>
        }
      }
      Class(defInfo, resolveParams(params), None, vars, restFields, isAbstract)
    }
    defInfo.ty = cls
    selfRef = ss
    items += (defInfo.id -> cls)

    // Check conflict
    var existings = Map[String, VField]();
    def addField(f: VField) = if (existings.contains(f.name)) {
      errors = s"conflict field ${f.name}" :: errors
    } else {
      existings += (f.name -> f)
    }
    cls.fields.foreach(addField)

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
    EnumVariant(info, cls)
  }

  def implItem(ast: syntax.Impl) = {
    val syntax.Impl(rhs, lhs, params, body) = ast
    val defInfo = ct("$impl", hidden = true)
    val ss = selfImplRef
    val ss2 = selfRef
    val impl = withNsParams("", params.map(Left(_))) {
      val (iface, cls) = (lhs.map(typeExpr), typeExpr(rhs))
      selfRef = Some(cls)
      val defs = body match {
        case body: syntax.Block =>
          selfImplRef =
            Some(Impl(defInfo, resolveParams(params), iface.get, cls, List()))
          val (vars, decls) = baseClass(body, None)
          if (!vars.isEmpty) {
            errors = s"impl cannot have vars" :: errors
          }
          selfImplRef =
            Some(Impl(defInfo, resolveParams(params), iface.get, cls, decls))
          baseClass(body, Some(vars ::: decls))._2
        case _ => errors = s"Invalid impl body" :: errors; List()
      }
      defs.foreach { d => d.item.id.isOverride = true }
      Impl(defInfo, resolveParams(params), iface.get, cls, defs)
    }
    selfRef = ss2
    selfImplRef = ss
    items += (defInfo.id -> impl)
    associateImpl(impl, impl.cls)
    impl
  }

  def associateImpl(impl: Impl, cls: Type): Unit = {
    if (canonicalTy(cls).isBuilitin) {
      return associateImpl(impl, classRepr(cls))
    }

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

  def castArgs(
      eParams: Option[List[Param]],
      eArgs: List[Item],
      self: Option[Either[Unit, Item]] = None,
  ): List[Item] = {
    val params = eParams.map(_.filter(_.level == 0)).map { p =>
      self match {
        case None => p
        case Some(_) =>
          if (p.headOption.exists(_.id.name == "self")) {
            p.tail
          } else {
            p
          }
      }
    }

    val firstArgs = eArgs.takeWhile(_.level > 0)
    val args = eArgs.drop(firstArgs.length)

    val paramsLength = params.map(_.length).getOrElse(0)
    if (paramsLength != args.length) {
      println(("self", self))
      errors =
        s"Invalid number of arguments (${paramsLength} v.s. ${args.length}) $params v.s. $args" :: errors
      return args
    }

    var argsPair = params.iterator.flatten.zip(args);
    firstArgs ::: (argsPair.map { case (p, a) =>
      val info = p.id
      val casted = castTo(a, info.ty)
      items += (info.id -> casted)
      // todo: cast type
      casted
    }.toList)
  }

  def castTo(item: Item, nty: Type): Item = {
    val ty = canonicalTy(nty)
    debugln(s"castTo $item to $nty ($ty)")
    ty match {
      case TopTy | UniverseTy => item
      case RefItem(rty, rhsIsMut) =>
        item match {
          case l: RefItem => {
            if (rhsIsMut) {
              checkMutability(item, rhsIsMut);
            }
            if (rhsIsMut && !l.isMut) {
              errors = s"Cannot cast to mut ref" :: errors
              return Opaque.expr(
                s"\n#error \"Cannot cast to mut ref\"\n /* ref $item */",
              )
            }
            val lty = canonicalTy(item)
            val lIsR = isSubtype(lty, rty);
            val rIsL = isSubtype(rty, lty);
            debugln(s"castTo $item to $rty ($lty) $lIsR $rIsL")
            if (lIsR && rIsL) {
              debugln(s"$item is exact $rty")
              return item
            }
            if (!lIsR) {
              errors = s"No way to cast $item ($lty) to $nty ($rty)" :: errors
              return Opaque.expr(
                s"\n#error \"No way to cast\"\n /* ref $item */",
              )
            }

            rty match {
              case tr: Class if tr.isAbstract => {
                As(item, implClass(lty, tr).get)
              }
              case _ =>
                As(item, nty)
            }
          }
          case l: Str if isSubtype(rty, StrTy) => RefItem(l, false)
          case l if !rhsIsMut                  => castTo(RefItem(l, false), nty)
          case _ =>
            errors = s"Must ref item $item" :: errors;
            Opaque.expr(s"\n#error \"Must ref item\"\n /* ref $item */")
        }
      case _ => return item
    }
  }

  def exprOpa(ast: syntax.Node): String = {
    valueExpr(ast) match {
      case Opaque(Some(expr), _) => expr
      case _                     => ""
    }
  }

  def defByName(info: DefInfo): String = info.defName(stem = false)

  def varByRef(vv: Term)(implicit topLevel: Boolean): String = {
    val ir.Term(id, level, v) = vv
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
      case cls: Class if cls.variantOf.isDefined => storeTy(cls.variantOf.get)
      case cls: Class                            => cls.repr(storeTy)
      case v: Term if v.value.isEmpty            => v.id.defName(stem = false)
      case v: Var if v.init.isEmpty              => v.id.defName(stem = false)
      case v: Fn                                 => v.id.defName(stem = false)
      case Term(_, _, Some(v))                   => storeTy(v)
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

  def checkMutability(item: Item, isMut: Boolean): Unit = {
    debugln(s"checkMutability $item $isMut")
    val lhsIsMut = item match {
      case RefItem(lhs, rhsIsMut) => return checkMutability(lhs, rhsIsMut)
      case SelfTy                 => return
      case v: Term                => !v.id.isConstantVal
      case v: Var                 => !v.id.isConstantVal
      case _                      => return
    }
    if (!lhsIsMut && isMut) {
      errors = s"Cannot cast to mut ref" :: errors
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

  def eqType(lhs: Item, rhs: Item): Boolean = {
    val lty = canonicalTy(lhs)
    val rty = canonicalTy(rhs)
    isSubtype(lty, rty) && isSubtype(rty, lty)
  }

  def canonicalTy(rhs: Item): Item = {
    debugln(s"canonicalTy $rhs")
    rhs match {
      case SelfVal if selfImplRef.isDefined =>
        selfRef.map(canonicalTy).getOrElse(TopTy)
      case SelfVal                               => SelfTy
      case Term(_, level, Some(v)) if level == 1 => canonicalTy(v)
      case v: Term                               => canonicalTy(v.id.ty)
      case v: Var                                => canonicalTy(v.id.ty)
      case RefItem(lhs, isMut) => RefItem(canonicalTy(lhs), isMut)
      case _                   => rhs
    }
  }

  def isSubtype(lhs: Item, rhs: Item): Boolean = {
    debugln(s"isSubtype $lhs $rhs")
    rhs match {
      case Term(_, _, Some(v)) => isSubtype(lhs, v)
      case RefItem(rhs, isMut) => isSubtype(lhs, rhs)
      // todo: same level
      case cls: Class         => implClass(lhs, cls).isDefined
      case TopTy | UniverseTy => true
      case BottomTy           => false
      case StrTy | BoolTy if isBuiltin(lhs, rhs) => true
      case _ => {
        lhs match {
          case Term(_, _, Some(v)) => isSubtype(v, rhs)
          case RefItem(lhs, isMut) => isSubtype(lhs, rhs)
          case BottomTy            => true
          case StrTy | BoolTy      => isBuiltin(lhs, rhs)
          case _                   => false
        }
      }
    }
  }

  def isBuiltin(lhs: Item, rhs: Item): Boolean = {
    debugln(s"isBuiltin $lhs $rhs")
    lhs match {
      case Term(_, _, Some(v)) => isBuiltin(v, rhs)
      case TopTy | UniverseTy  => true
      case BottomTy            => true
      case Bool(_)             => lhs == rhs || rhs == BoolTy
      case Str(_)              => lhs == rhs || rhs == StrTy
      case _                   => lhs == rhs
    }
  }

  def classRepr(lhs: Type): Class = {
    lhs match {
      case ClassInstance(con, _) => con
      case v: Class              => v
      case Term(_, _, Some(v))   => classRepr(v)
      case RefItem(lhs, isMut)   => classRepr(lhs)
      case _ if lhs.isBuilitin   => builtinClasses(lhs)
      case l @ (Bool(_))         => builtinClasses(l.ty)
      case l @ (Str(_))          => builtinClasses(l.ty)
      case l @ (Integer(_))      => builtinClasses(l.ty)
      case Unresolved(_)         => Class.empty(this, false)
      case _ => throw new Exception(s"cannot get class $lhs")
    }
  }

  def implClass(lhs: Item, goal: Class): Option[Item] = {
    val cls = classRepr(lhs)
    debugln(
      s"implClass? $goal(${goal.isAbstract}) for $lhs ($cls) impls ${cls.id.impls} ",
    )
    if (cls.id == goal.id) {
      return Some(cls)
    }
    if (!goal.isAbstract) {
      return None;
    }
    cls.id.impls.find { i => isSubtype(i.iface, goal) }
  }

  def enumShape(ty: Item): Option[Class] = {
    ty match {
      case v: Term if v.value.isEmpty =>
        enumShape(items.getOrElse(v.id.id, NoneItem))
      case Term(_, _, Some(v)) => enumShape(v)
      case BoundField(_, by, _, TypeField(v: EnumVariant)) =>
        Some(v.base.copy(variantOf = Some(by)))
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
      case RefItem(lhs, isMut)              => tyOf(lhs).map(RefItem(_, isMut))
      case v: ClassInstance                 => Some(v.con)
      case BoundField(_, _, _, VarField(v)) => Some(v.id.ty)
      case b: BinOp             => coerceTy(tyOf(b.lhs), tyOf(b.rhs))
      case If(_, x, y)          => coerceTy(tyOf(x), y.flatMap(tyOf))
      case SelfVal              => Some(SelfTy)
      case Term(id, _, Some(v)) => tyOf(v)
      case Term(id, level, _) if level == 0 => Some(id.ty)
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
}
