package cosmo

import ir._
import cosmo.system._
import cosmo.FileId
import cosmo.syntax.Ident

type SParam = syntax.Param;
type SParams = List[SParam];

final class DefId(val id: Int) extends AnyVal {}

class Scopes {
  var scopes: List[Map[String, DefInfo]] = List(Map())

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

  def get(name: String): Option[DefInfo] = {
    scopes.find(_.contains(name)).flatMap(_.get(name))
  }

  def set(name: String, value: DefInfo) = {
    scopes = scopes.updated(0, scopes.head.updated(name, value))
  }

  override def toString() = scopes.toString
}

private val nameJoiner = "::";

class DefInfo(
    val name: String,
    val noMangle: Boolean,
    val namespaces: List[String],
    var id: DefId,
    var env: Env,
    var upperBounds: List[Type] = List(),
    var lowerBounds: List[Type] = List(),
    var inClass: Boolean = false,
) {
  def defName(stem: Boolean = false)(implicit
      topLevel: Boolean,
  ): String = {
    if ((topLevel && this.name == "main"))
      this.name
    else if (stem) this.nameStem(this.id.id)
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
    (namespaces :+ s"${name}").mkString(nameJoiner)
}

object DefInfo {
  def just(id: Int, env: Env) =
    new DefInfo("", false, List(), DefId(id), env)
}

class Env(pacMgr: cosmo.PackageManager) {
  var defAlloc = DEF_ALLOC_START
  var items: Map[DefId, Item] = Map()
  var scopes = new Scopes()
  var errors: List[String] = List()
  var module: ir.Region = Region(List())
  var ns: List[String] = List()

  implicit val system: CosmoSystem = new JsPhysicalSystem()

  def eval(ast: syntax.Block): Env = {
    newBuiltin("println")
    newBuiltin("panic")

    newType("c_enum", CEnumTy)
    newType("Ref", RefTy)
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
      case Some(ir.Class(_, params, _, _)) => params.map(Right(_))
      case _                               => params.map(Left(_))
    }
  }

  def ct(
      name: String,
      noMangle: Boolean = false,
      inClass: Boolean = false,
      hidden: Boolean = false,
  ): DefInfo = {
    defAlloc += 1
    val id = new DefId(defAlloc)
    val info =
      new DefInfo(name, noMangle, ns, id, this, inClass = inClass)
    if (!hidden) {
      scopes.set(name, info)
    }
    info
  }

  def newBuiltin(name: String) = {
    val id = ct(name, noMangle = true)
    items += (id.id -> Opaque.expr(s"$name"))
  }

  def newType(name: String, ty: Type) = {
    val id = ct(name, noMangle = true)
    items += (id.id -> ty)
  }

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
    withNs(ns) {
      scopes.withScope {
        params.foreach {
          case Right(params) =>
            params.iterator.foreach { case Param(name, id, ty) =>
              scopes.set(name, id)
            }
          case Left(params) =>
            params.iterator.foreach { case SParam(name, ty, init) =>
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
        val info = scopes.get(p.name).get
        // todo: compute canonical type
        Param(p.name, info, info.upperBounds.headOption.getOrElse(TopTy)),
      )
    }
  }

  def liftAsType(item: Item)(implicit level: Int): Item = {
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
    case Some(id) => byRef(name, id)
    case None =>
      errors = s"Undefined variable $name" :: errors;
      Unresolved(ct(name))
  }

  def byRef(name: String, info: DefInfo)(implicit level: Int): Item = {
    val v = items.get(info.id).map(deref)
    debugln(s"byRef $name $info ${v.map(_.level)}")
    Variable(name, info, v.map(_.level).getOrElse(level), value = v)
  }

  def deref(lhs: Item)(implicit level: Int): Item = {
    lhs match {
      case (f: Sig) if f.params.isEmpty    => applyItem(f, List())
      case Fn(_, f, _) if f.params.isEmpty => applyItem(f, List())
      case v @ EnumVariantIns(iface, _, _, cls: Class)
          if cls.params.isEmpty && cls.isPhantomClass =>
        applyItem(v, List())
      case _ => lhs
    }
  }

  def valueExpr(node: syntax.Node)(implicit level: Int = 0): Item = expr(node)
  def typeExpr(node: syntax.Node)(implicit level: Int = 1): Type = expr(node)
  def expr(ast: syntax.Node)(implicit level: Int): ir.Item = {
    ast match {
      // literals
      case syntax.TodoLit          => TodoLit
      case syntax.Self             => SelfVal
      case syntax.BigSelf          => SelfTy
      case syntax.BoolLit(value)   => Opaque.expr(value.toString)
      case syntax.IntLit(value)    => Opaque.expr(value.toString)
      case syntax.FloatLit(value)  => Opaque.expr(value.toString)
      case syntax.StringLit(value) => Str(value)
      case syntax.Ident(name)      => byName(name)
      // control flow
      case l: syntax.Loop        => Loop(expr(l.body))
      case f: syntax.For         => For(f.name, expr(f.iter), expr(f.body))
      case syntax.Break()        => Break()
      case syntax.Continue()     => Continue()
      case syntax.Return(value)  => Return(expr(value))
      case syntax.If(cond, x, y) => If(expr(cond), expr(x), y.map(expr))
      // expressions
      case b: syntax.Block            => block(b)
      case b: syntax.Match            => matchExpr(b)
      case syntax.UnOp(op, lhs)       => UnOp(op, expr(lhs))
      case syntax.BinOp(op, lhs, rhs) => BinOp(op, expr(lhs), expr(rhs))
      case syntax.As(lhs, rhs)        => As(expr(lhs), typeExpr(rhs))
      case syntax.Apply(lhs, rhs)     => applyItem(expr(lhs), rhs.map(expr))
      case syntax.Select(lhs, rhs)    => deref(selectItem(expr(lhs), rhs.name))
      case syntax.Semi(None)          => ir.NoneKind(level)
      case syntax.Semi(Some(value))   => Semi(expr(value))
      // declarations
      case syntax.Import(p, dest) => importItem(p, dest)
      case syntax.Val(x, ty, y)   => varItem(ct(x), ty, y.map(expr), true)
      case syntax.Typ(x, ty, y)   => varItem(ct(x), ty, y.map(typeExpr), true)
      case syntax.Var(x, ty, y)   => varItem(ct(x), ty, y.map(expr), false)
      case d: syntax.Def          => defItem(d, ct(d.name))
      case c: syntax.Class        => classItem(c, Some(classItem(c, None)))
      case t: syntax.Trait        => Opaque.expr(s"0/* trait: ${t} */")
      // syntax errors
      case SParam(name, _, _)    => Opaque.expr(s"panic(\"param: $name\")")
      case syntax.KeyedArg(k, v) => KeyedArg(k, expr(v))
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

  def block(ast: syntax.Block) = Region(scopes.withScope {
    ast.stmts.map(valueExpr)
  })

  def isCEnum(ty: Item): Boolean = ty match {
    case Variable(_, id, _, Some(CEnumTy)) => true
    case Variable(_, id, _, _)             => isCEnum(items(id.id))
    case CEnumTy                           => true
    case _                                 => false
  }

  def matchExpr(b: syntax.Match)(implicit level: Int): Item = {
    var lhs = expr(b.lhs)
    var lhsTy = lhs match {
      case Variable(_, defInfo, _, v) => defInfo.upperBounds.headOption
      case _                          => None
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
      Map[String, (Interface, List[(EnumDestruct, Option[syntax.Node])])]()

    for (syntax.Case(destructor, body) <- rhs.stmts) {
      val splited = destructItem(lhs, destructor);

      splited match {
        case ed: EnumDestruct =>
          // todo: stable toString
          val vs = storeTy(ed.variant.variantOf.ty)(false)
          vMappings.get(vs) match {
            case Some((_, lst)) =>
              vMappings =
                vMappings + (vs -> (ed.variant.variantOf, lst :+ (ed, body)))
            case None =>
              vMappings =
                vMappings + (vs -> (ed.variant.variantOf, List((ed, body))))
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

    val (_, (e, cases)) = vMappings.head

    debugln(s"matchExpr mappings $e => $cases")

    var matchBody = List[(EnumVariantIns, Item)]()
    for ((ed, body) <- cases) {
      val variant = ed.variant
      val bindings = ed.bindings

      matchBody = matchBody :+ (variant, Region(
        body
          .map(body =>
            scopes.withScope {
              // bindings
              variant.base.vars.zip(bindings).map { (vv, name) =>
                val defInfo = ct(name)
                val ty: Type =
                  defInfo.upperBounds.headOption.getOrElse(TopTy)
                val tyLvl = ty.level
                val valLvl = (tyLvl - 1).max(0)
                val res = Variable(name, defInfo, valLvl)
                defInfo.upperBounds = defInfo.upperBounds :+ ty
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

    EnumMatch(lhs, e, matchBody, Unreachable)
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

  def destructItem(lhs: Item, destructor: syntax.Node)(implicit
      level: Int,
  ): Item = {
    destructor match {
      case name: (syntax.Ident | syntax.Select) => {
        val variant = enumShape(expr(name));
        if (variant.isEmpty) {
          errors = s"Invalid enum variant $name" :: errors
          return lhs
        }
        EnumDestruct(lhs, variant.get, List(), None)
      }
      case syntax.Apply(name, rhs) => {
        val variant = enumShape(expr(name));
        if (variant.isEmpty) {
          errors = s"Invalid enum variant $name" :: errors
          return lhs
        }
        EnumDestruct(
          lhs,
          variant.get,
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

  def enumShape(ty: Item): Option[EnumVariantIns] = {
    ty match {
      case iface: Interface => None
      case v: Variable if v.value.isEmpty =>
        enumShape(items.getOrElse(v.id.id, NoneItem))
      case Variable(_, _, _, Some(v)) => enumShape(v)
      case v: ir.EnumInstance         => Some(v.base)
      case v: EnumVariantIns          => Some(v)
      case ty                         => None
    }
  }

  def applyItem(lhs: Item, rhs: List[Item])(implicit
      level: Int,
  ): Item = {
    debugln(s"applyItem $lhs |||| ${rhs}")
    lhs match {
      case Variable(_, id, _, Some(Unresolved(id2)))
          if id2.id.id == CODE_FUNC =>
        return rhs.head match {
          case Str(content) => Opaque.stmt(content)
          case s: Opaque    => s
          case _            => Opaque.expr("0 /* code */")
        }
      case Variable(_, id, _, Some(RefTy)) =>
        assert(rhs.length == 1)
        return RefItem(rhs.head)
      case Variable(_, id, _, Some(v)) if v.level > 0 =>
        id.env.applyItem(v, rhs)
      case fn: Sig          => applyFunc(fn, rhs).getOrElse(Apply(fn, rhs))
      case fn: Fn           => applyFunc(fn.sig, rhs).getOrElse(Apply(fn, rhs))
      case cls: Class       => applyClassItem(cls, Some(rhs))
      case iface: Interface => applyInterface(iface, rhs)
      case ev: EnumVariantIns                   => applyEnumVariant(ev, rhs)
      case v: CIdent if rhs.exists(_.level > 0) => CppInsType(v, rhs)
      case _                                    => Apply(lhs, rhs)
    }
  }

  def applyFunc(fn: Sig, args: List[Item])(implicit
      level: Int,
  ): Option[Item] = {
    val Sig(params, ret_ty, body) = fn
    ret_ty match {
      case Some(ir.Variable(_, id, _, Some(UniverseTy))) =>
        implicit val level = 1;
        return scopes.withScope {
          params.iterator.flatten.zip(args).foreach { case (p, a) =>
            val Param(name, info, ty) = p
            scopes.set(name, info)
            items += (info.id -> a)
            info.lowerBounds = info.lowerBounds :+ ty
          }
          Some(body.map(liftAsType).map(evalExpr).getOrElse(NoneItem))
        }
      case _ =>
    }

    None
  }

  def evalExpr(item: Item)(implicit level: Int): Item = {
    debugln(s"evalExpr $item $level")
    val e = evalExpr;
    item match {
      case CppInsType(target, arguments) => CppInsType(target, arguments.map(e))
      case Variable(nameHint, id, lvl, value) if level <= lvl => items(id.id)
      case _                                                  => item
    }
  }

  def applyClassItem(node: Class, args: Option[List[Item]]): Item = {
    debugln(s"applyClassItem ${node.id} $args")
    val Class(clsInfo, params, vars, restFields) = node
    val defInfo = ct(clsInfo.name, hidden = true)
    val classTy = NativeType(clsInfo.name);

    val ty = if (params.isEmpty) {
      classTy
    } else {
      args.map(NativeInsType(classTy, _)).getOrElse(classTy)
    };

    def fields = (vars ::: restFields).map(p => p.item.id.name -> p)
    val iface = ir.Interface(ty, defInfo, clsInfo, Map(fields.toSeq: _*))
    if (!params.isEmpty || args.isEmpty) {
      return iface
    }

    return applyInterface(iface, args.getOrElse(List()))
  }

  def applyInterface(iface: Interface, rhs: List[Item]): Item = {
    debugln(s"applyInterface $iface $rhs")
    ClassInstance(iface, rhs)
  }

  def applyEnumVariant(ev: EnumVariantIns, rhs: List[Item])(implicit
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
    debugln(s"selectItem $lhs ${lhs.getClass().getName()} $field")
    var res = lhs match {
      case Variable(_, id, _, v) => v.map(id.env.selectItem(_, field))
      case NativeModule(info, env, fid) =>
        return env.byRef(field, env.scopes.get(field).get)
      case CModule(id, kind, path) => return CIdent(field, List(), level)
      case CIdent(ns0, ns, level)  => return CIdent(field, ns :+ ns0, level)
      case cls: Class => return selectItem(applyClassItem(cls, None), field)
      case interface: Interface =>
        interface.fields.get(field).map {
          case TypeField(EnumVariant(id, _, base)) =>
            EnumVariantIns(id, interface, field, base)
          // todo: bound to interface
          case DefField(item)  => item
          case TypeField(item) => item
          case VarField(item)  => NoneItem
        }
      case _ => None
    }
    // typeExpr(lhs).instantiate match {
    // }
    res.getOrElse(Select(lhs, field))
  }

  def importItem(p: syntax.Node, dest: Option[syntax.Node]): Item = {
    val path = p match {
      case syntax.StringLit(s) => s
      case _: (syntax.Select | syntax.Ident) =>
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
    val defInfo = ct("$module")
    importDest(dest, defInfo, CModule(defInfo, kind, includePath))
  }

  def importNative(p: syntax.Node, dest: Option[syntax.Node]): Item = {
    val defInfo = ct(p match {
      case _ if (!dest.isEmpty)                   => "$module"
      case syntax.Select(lhs, syntax.Ident(name)) => name
      case syntax.Ident(name)                     => name
      case _                                      => "$module"
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
    var di = dest match {
      case Some(syntax.Ident(name)) => ct(name)
      case Some(v) =>
        errors = s"Invalid import destination $v" :: errors
        ct("$module")
      case None => defInfo
    }
    di.upperBounds = di.upperBounds :+ v
    items += (di.id -> v)
    v
  }

  def varItem(
      defInfo: DefInfo,
      oty: Option[syntax.Node],
      initExpr: Option[Item],
      isContant: Boolean,
  )(implicit level: Int): ir.Var = {
    val initTy = (oty.map(typeExpr), defInfo.name) match {
      case (Some(ty), _)  => ty
      case (None, "self") => SelfTy
      case _              => TopTy
    }
    val valLvl = (initTy.level - 1).max(0)
    val res = ir.Var(defInfo, initExpr, isContant, valLvl)
    defInfo.upperBounds = defInfo.upperBounds :+ initTy
    items += (defInfo.id -> {
      if level == 0 then res else initExpr.getOrElse(res)
    })
    res
  }

  def defItem(ast: syntax.Def, defInfo: DefInfo, withBody: Boolean = true) = {
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
    val tyLevel = (sig.ret_ty.map(_.level - 1).getOrElse(0)).max(0)
    val f = Fn(defInfo, sig, tyLevel)
    items += (defInfo.id -> f)
    f
  }

  def classItem(ast: syntax.Class, classSelf: Option[Class] = None): Class = {
    val classSelfId = classSelf.map(_.id)
    val syntax.Class(name, params, body) = ast
    val defInfo = newDefWithInfoOr(name, classSelfId)
    // todo: assign items before checking expr for recursive functions
    val cls = withNsParams(name, paramsOr(classSelf, params)) {
      body match
        case body: syntax.Block =>
          val fields = classSelf.map(p => p.vars ::: p.restFields)
          baseClass(params, body, defInfo, fields = fields)
        case caseBlock: syntax.CaseBlock =>
          enumClass(params, caseBlock, defInfo, classSelf = classSelf)
        case _ =>
          errors = "Invalid class body" :: errors
          Class.empty(this)
    }
    items += (defInfo.id -> cls)
    cls
  }

  def baseClass(
      params: Option[List[SParam]],
      ast: syntax.Block,
      info: DefInfo,
      fields: Option[List[VField]] = None,
  ) = {
    val wb = !fields.isEmpty
    var vars = List[VarField]();
    var rests = List[VField]();

    val nn = (name: String) => {
      // todo: high complexity
      val oid = fields.iterator.flatten.map(_.item.id).find(x => x.name == name)
      oid.map { scopes.set(name, _) }
      oid.getOrElse(ct(name, inClass = true))
    }

    def classItem(item: syntax.Node) = item match {
      // todo: syntax.Typ
      case syntax.Val(x, ty, y) =>
        vars = vars :+ VarField(varItem(nn(x), ty, y.map(valueExpr), true)(0))
      case syntax.Var(x, ty, y) =>
        vars = vars :+ VarField(varItem(nn(x), ty, y.map(valueExpr), false)(0))
      case d: syntax.Def =>
        rests = rests :+ DefField(defItem(d, nn(d.name), withBody = wb))
      case node =>
        errors = "Invalid class item" :: errors
    }

    ast.stmts.foreach {
      case syntax.Semi(None)       =>
      case syntax.Semi(Some(stmt)) => classItem(stmt)
      case stmt                    => classItem(stmt)
    }

    Class(info, resolveParams(params), vars, rests)
  }

  def enumClass(
      params: Option[List[syntax.Param]],
      ast: syntax.CaseBlock,
      info: DefInfo,
      classSelf: Option[Item] = None,
  ) = {
    val selfRestFields = classSelf
      .map {
        case ir.Class(_, _, vars, restFields) => restFields
        case item =>
          errors = s"Invalid class item: $item" :: errors
          List()
      };
    var subs = selfRestFields.iterator.flatten
      .filter(_.isInstanceOf[TypeField])
      .map(_.item.asInstanceOf[EnumVariant])
    var selfRestFields2 =
      selfRestFields.map(_.filter(!_.isInstanceOf[TypeField]))

    val stmts: List[VField] = ast.stmts
      .filter(!_.isWildcard)
      .map(p =>
        TypeField(
          enumVariant(p, info, info.name, classSelf = subs.nextOption()),
        ),
      )
    val defaultCls = ast.stmts
      .find(_.isWildcard)
      .map { case syntax.Case(_, body) =>
        baseClass(
          params,
          // todo: as instance of Block
          body.getOrElse(syntax.Block(List())).asInstanceOf[syntax.Block],
          info,
          fields = selfRestFields2,
        )
      }
    val restFields = defaultCls match {
      case Some(cls) =>
        if (!cls.vars.isEmpty) {
          errors = s"Invalid default class for enum" :: errors
        }
        cls.restFields
      case None => List()
    }

    Class(info, resolveParams(params), List(), restFields ::: stmts)
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
      case (Some(syntax.Block(bc)), vars) => syntax.Block(vars ::: bc)
      case (Some(n), vars)                => syntax.Block(vars :+ n)
      case _                              => syntax.Block(vars)
    }

    val cls = classItem(
      syntax.Class(subName, None, b),
      classSelf = classSelf.map(_.base),
    )
    val vid = ct(subName)
    EnumVariant(vid, baseId, cls)
  }

  def exprOpa(ast: syntax.Node): String = {
    valueExpr(ast) match {
      case Opaque(Some(expr), _) => expr
      case _                     => ""
    }
  }

  def defByName(info: DefInfo): String = info.nameStem(info.id.id)

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
      case FloatTy(size)                  => s"float${size}_t"
      case BoolTy                         => "bool"
      case StringTy                       => "CString"
      case SelfTy                         => "self_t"
      case ty: CIdent                     => ty.repr
      case ty: CppInsType                 => ty.repr(storeTy)
      case ty: NativeType                 => ty.repr
      case ty: Interface                  => storeTy(ty.ty)
      case ty: NativeInsType              => ty.repr(storeTy)
      case classTy: Class                 => classTy.id.defName(stem = true)
      case v: Variable if v.value.isEmpty => v.id.defName(stem = true)
      case v: Var if v.init.isEmpty       => v.id.defName(stem = true)
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
