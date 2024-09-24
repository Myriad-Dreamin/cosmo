package cosmo

import scala.collection.mutable.{
  ListBuffer,
  LinkedHashMap as MutMap,
  LongMap as MutLongMap,
}

import ir._
import cosmo.system._
import cosmo.FileId
import cosmo.syntax.Ident
import cosmo.syntax.CaseBlock
import cosmo.syntax.FloatLit
import scala.annotation.tailrec
import cosmo.inst.InstModule

type EParam = VarExpr;
type EParams = List[EParam];

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

class Env(val fid: Option[FileId], val pacMgr: cosmo.PackageManager)
    extends ExprEnv
    with TypeEnv
    with inst.InstEnv {

  var noCore = false
  var syntaxOnly = false

  var defAlloc = DEF_ALLOC_START
  var defs = List[DefInfo]()
  var currentDef: Option[DefId] = None
  var currentRegion = (-1, -1)
  var defParents = Map[DefId, (Option[DefId], (Int, Int))]()
  var items: Map[DefId, Item] = Map()
  var scopes = new Scopes()
  var errors: List[String] = List()
  var ns: List[String] = List()
  var builtinClasses = Map[Item, Class]()
  var selfRef: Option[Item] = None
  var selfImplRef: Option[Item] = None
  var rawDeps = Map[FileId, Option[Env]]()
  var checkStatus = MutLongMap[Unit]()

  val patHolder = Str("$")
  var moduleAst: Region = Region(List(), false)
  var moduleInst = InstModule()
  var module: ir.Region = Region(List(), false)

  /// Builtin Items

  def builtins() = {
    newBuiltin("print")
    newBuiltin("println")
    newBuiltin("unreachable")
    newBuiltin("unimplemented")
    newBuiltin("panic")

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

  /// Entry

  def exprStage(ast: syntax.Block): Env = {
    // FIXME: better noCore handling
    ast.stmts.takeWhile {
      case syntax.Decorate(syntax.Apply(Ident("noCore"), _, _), _) =>
        noCore = true; false
      case syntax.Decorate(syntax.Apply(Ident("syntaxOnly"), _, _), _) =>
        syntaxOnly = true; false
      case _ => true
    }
    if (!syntaxOnly && !noCore) then
      expr(syntax.Import(libPath("std.prelude"), Some(Ident("_"))))

    moduleAst = Region(ast.stmts.map(expr), true)

    this
  }

  def evalStage: Env = {
    if (syntaxOnly) return this

    val m = valTerm(moduleAst)
    if !m.isInstanceOf[Region] then err("module must be a block")
    module = m.asInstanceOf[Region]

    this
  }

  def emitTask: Env = {
    if (syntaxOnly) return this

    moduleInst = emitModule(module)

    this
  }

  def report: Env = {
    for (error <- errors) {
      println(error)
    }
    this
  }

  /// Item Creation

  def err(msg: String): Item =
    errors = errors :+ msg; return NoneItem

  /// Creates Def
  def ct(src: syntax.Ident | String, hidden: Boolean = false): DefInfo = {
    val (name, pos) = src match {
      case node @ syntax.Ident(name) => (name, Some(node.offset, node.end))
      case name: String              => (name, None)
    }

    defAlloc += 1
    val info = new DefInfo(name, ns, new DefId(defAlloc), this)
    info.pos = pos
    info.isHidden = hidden
    defs = info :: defs
    defParents += (info.id -> (currentDef, currentRegion))

    if (!hidden) then scopes.set(name, info)
    info
  }

  /// Creates Infer Variable
  def createInfer(info: DefInfo, lvl: Int) = InferVar(info, level = lvl)

  /// Creates Reference
  def byRef(info: DefInfo)(implicit level: Int): Ref = {
    val v = items.get(info.id).map(deref)
    debugln(s"byRef $info ${v.map(_.level)}")
    v match {
      case Some(v: Ref) => v
      case _            => Ref(info, v.map(_.level).getOrElse(level), value = v)
    }
  }

  def valTermO(node: Option[Item])(implicit level: Int = 0): Item =
    node.map(valTerm).getOrElse(NoneItem)
  def valTerm(node: Item)(implicit level: Int = 0): Item = term(node)
  def tyTerm(node: Item)(implicit level: Int = 1): Type = term(node)
  def term(item: Item)(implicit level: Int): ir.Item = {
    if !item.isInstanceOf[Expr] then return item
    item.asInstanceOf[Expr] match {
      // control flow, todo: duplicate patterns
      case item: (Break | Continue | Opaque) => item
      case Return(v)                         => Return(term(v))
      case If(cond, x, y)        => If(term(cond), term(x), y.map(term))
      case Loop(body)            => Loop(term(body))
      case While(cond, body)     => While(term(cond), term(body))
      case For(name, iter, body) => For(term(name), term(iter), term(body))
      case Region(stmts, semi)   => Region(stmts.map(term), semi)
      // operations
      case Name(id, None) =>
        val t = byRef(id);
        if t.value.isEmpty then err(s"undefined $id")
        t
      case Name(id, Some(of))          => term(of)
      case UnOp("&", UnOp("mut", lhs)) => RefItem(term(lhs), true)
      case UnOp("&", lhs)              => RefItem(term(lhs), false)
      case UnOp("mut", lhs) =>
        errors = s"mut must be used after &" :: errors; term(lhs)
      case UnOp("*", lhs)      => derefPtr(lhs)
      case UnOp(op, lhs)       => UnOp(op, term(lhs))
      case BinOp(op, lhs, rhs) => binOp(op, term(lhs), term(rhs))
      case As(lhs, rhs)        => As(term(lhs), tyTerm(rhs))
      case KeyedArg(k, v)      => KeyedArg(term(k), term(v))
      // todo: check is compile time
      case SelectExpr(lhs, rhs) => deref(select(term(lhs), rhs))
      case Apply(lhs, rhs)      => $apply(term(lhs), rhs.map(term))
      case b: MatchExpr         => matchTerm(b)
      case ItemE(item)          => term(item)
      // declarations
      case v: VarExpr      => checkVar(v)
      case d: DefExpr      => checkDef(d)
      case c: ClassExpr    => checkClass(c)
      case i: ImplExpr     => checkImpl(i)
      case d: DestructExpr => checkDestruct(d)
      case Hole(id)        => err(s"hole $id in the air")
      case cr: CaseRegion  => err(s"case region $cr in the air")
      case c: CaseExpr =>
        errE(s"case clause without match")
        Opaque.expr(s"0/* error: case clause without match $c */")
    }
  }

  def expGG(ast: syntax.Node)(implicit level: Int): ir.Item = ???

  /// imports

  def $import(p: syntax.Node, dest: Option[syntax.Node]): Item = {
    val path = p match {
      case syntax.StrLit(s) => s
      case _: (syntax.Select | syntax.Ident) =>
        return importNative(p, dest)
      case _ =>
        err("Invalid import path")
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
    val moduleIns = CModule(defInfo, kind, includePath)
    items += (defInfo.id -> moduleIns)
    defInfo.ty = moduleIns
    moduleIns
  }

  def importNative(p: syntax.Node, dest: Option[syntax.Node]): Item = {
    val defInfo = ct(p match {
      case _ if (!dest.isEmpty)                      => "$module"
      case syntax.Select(lhs, syntax.Ident(name), _) => name
      case syntax.Ident(name)                        => name
      case _                                         => "$module"
    })
    val fid = pacMgr.resolvePackage(p)
    val env = pacMgr.loadModule(fid)
    rawDeps += (fid -> env)
    val moduleIns = env match {
      case Some(env) => NativeModule(defInfo, env)
      case None => {
        err(s"Failed to load module $p")
        Unresolved(defInfo)
      }
    }
    items += (defInfo.id -> moduleIns)
    defInfo.ty = moduleIns
    moduleIns
  }

  /// Expressions

  def derefPtr(lhs: Item)(implicit level: Int): Item = {
    lhs match {
      case SelfVal => SelfVal
      case lhs     => UnOp("*", term(lhs))
    }
  }

  def deref(lhs: Item)(implicit level: Int): Item = {
    lhs match {
      case f: Fn if f.params.isEmpty => $apply(f, List())
      case BoundField(_, by, _, EnumField(ev: Class)) if ev.justInit =>
        $apply(lhs, List())
      case cls: Class if cls.justInit =>
        $apply(cls, List())
      case _ => lhs
    }
  }

  def binOp(op: String, lhs: Item, rhs: Item): Item = op match {
    case "<:" => Bool(isSubtype(lhs, rhs))
    case _    => BinOp(op, lhs, rhs)
  }

  def select(lhs: Item, field: String)(implicit level: Int): Item = {
    logln(s"select $lhs ${lhs.getClass().getName()} $field")
    val x = dispatch(lhs, lhs, field, false)
    logln(s"select $lhs $field => $x")
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
        val s2 = f.selfIsMut match {
          case Some(true)  => RefItem(s, true) // todo: check self mutability
          case Some(false) => RefItem(s, false)
          case _           => SelfTy
        }
        b.copy(lhs = s2)
      case b @ BoundField(RefItem(s @ SelfVal, isMut), _, _, DefField(f)) =>
        val s2 = f.selfIsMut match {
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
    def dFields(by: Item, v: FieldMap) =
      v.get(field).map(BoundField(lhs, by, casted, _)).map(ls)

    def dImpls(id: DefInfo): Option[Item] = {
      val impls = id.impls.flatMap { i =>
        i.fields.get(field).map(BoundField(lhs, i, true, _)).map(ls)
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
      case RefItem(r, isMut)  => dispatch(lhs, r, field, casted)
      case Ref(_, _, Some(v)) => dispatch(lhs, v, field, casted)
      case Ref(id, _, None)   => dispatch(lhs, id.ty, field, casted)
      case NativeModule(info, env) =>
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
    logln(s"apply $lhs |||| ${rhs}")
    lhs match {
      case Ref(id, _, Some(Unresolved(id2))) if id2.id.id == CODE_FUNC =>
        return rhs.head match {
          case Str(content) => Opaque.stmt(content)
          case e: Opaque    => e
          case _            => Opaque.expr("0 /* code */")
        }
      case Ref(id, _, Some(RefTy(isRef, isMut))) =>
        assert(rhs.length == 1)
        return if (isRef) {
          val r = rhs.head;
          checkedMut(r, isMut);
          RefItem(r, isMut)
        } else {
          rhs.head
        }
      case Ref(id, _, Some(v))                  => $apply(v, rhs)
      case v: CIdent if rhs.exists(_.level > 0) => CppInsType(v, rhs)
      case BoundField(_, by, _, EnumField(ev)) =>
        println(s"applyEnum $lhs $rhs")
        applyC(ev.copy(variantOf = Some(by)), rhs)
      case BoundField(that, by, _, DefField(f)) =>
        Apply(lhs, castArgs(f.params, rhs))
      case f: Fn    => applyF(f, rhs)
      case c: Class => applyC(c, rhs)
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

  def applyF(fn: Fn, args: List[Item]): Item = {
    if (fn.ret_ty.level <= 1) then return Apply(fn, castArgs(fn.params, args))
    implicit val level = 1;
    return scopes.withScope {
      // val castedArgs = params.iterator.flatten.zip(args).map { case (p, a) =>
      //   val info = p.id
      //   scopes.set(info.name, info)
      //   val casted = castTo(a, info.ty)
      //   items += (info.id -> casted)
      //   // todo: constrain types
      //   casted
      // }
      // val value = body.map(lift).map(eval).getOrElse(NoneItem)
      // hktRef(info, castedArgs.toList, value)
      ???
    }
  }

  def applyC(node: Class, args: List[Item]): Item = {
    logln(s"applyClass ${node.id} ${node.variantOf} $args")
    val typeParams = node.rawParams
    val allArgs = node.args.getOrElse(List()) ::: args
    val isTypeLevel =
      typeParams.map(allArgs.length < _.length).getOrElse(false);
    if (isTypeLevel) {
      return node.copy(args = Some(allArgs))
    }

    val castedArgs = castArgs(node.params, allArgs)
    val typeLevelArgs = typeParams.map(p => castedArgs.take(p.size))

    val clsFinal = node.copy(args = typeLevelArgs)
    ClassInstance(clsFinal, castedArgs.drop(typeParams.size))
  }

  case class MatchCaseInfo(
      destructor: syntax.Node,
      body: Option[syntax.Node],
      pattern: Item,
  );

  case class MatchInfo(
      lhs: Item,
      cases: List[MatchCaseInfo],
      defaultCase: Option[Item],
  );

  def matchTerm(b: MatchExpr)(implicit level: Int): Item = {
    val lhs = term(b.lhs)
    val lhsTy = tyOf(lhs) match
      case None     => return err("cannot match a untyped value")
      case Some(ty) => ty;
    val lhsShape = curryView(lhs, lhsTy);

    logln(s"matchExpr $lhs ($lhsTy qualified as $lhsShape) on ${b.body}")

    // // Calculate the kind of match.
    // var defaultCase: Option[Item] = None
    // var matchCases: List[MatchCaseInfo] = List()

    val (patterns, restTy) =
      b.body.cases.foldLeft((List[(Item, Item)](), lhsShape)) {
        case ((patterns, lhs), CaseExpr(destructor, body)) =>
          val (pattern, rests) = matchPat(lhs, curryExpr(destructor))
          (patterns :+ (pattern, valTermO(body)), rests)
      }

    // var vMappings =
    //   Map[String, List[(EnumDestruct, Option[syntax.Node])]]()

    // matchCases.foreach {
    //   case MatchCaseInfo(destructor, body, ed: EnumDestruct) =>
    //     // todo: stable toString
    //     val variantBase = ed.variant.variantOf.get
    //     val vs = storeTy(variantBase)
    //     vMappings.get(vs) match {
    //       case Some(lst) =>
    //         vMappings = vMappings + (vs -> (lst :+ (ed, body)))
    //       case None =>
    //         vMappings = vMappings + (vs -> List((ed, body)))
    //     }
    //   // Check if the value matches.
    //   case _ =>
    //     errors = s"not implemented mixed enum match" :: errors
    // }

    // // assert that there is only one match
    // if (vMappings.size != 1) {
    //   errors = s"not implemented mixed enum match" :: errors
    //   return NoneItem
    // }

    // val (_, cases) = vMappings.head
    // val ty = cases.head._1.variant.variantOf.get

    // debugln(s"matchExpr mappings default $defaultCase")
    // debugln(s"matchExpr mappings $ty => $cases")

    // var matchBody = List[(Class, Item)]()
    // for ((ed, body) <- cases) {
    //   val variant = ed.variant
    //   val bindings = ed.bindings

    //   val stmts = body.map(body =>
    //     scopes.withScope {
    //       // bindings
    //       variant.vars.zip(bindings).map { (vv, name) =>
    //         val defInfo = ct(name); defInfo.isVar = true
    //         defInfo.ty = vv.item.id.ty
    //         val ty: Type = defInfo.ty
    //         val tyLvl = ty.level
    //         val valLvl = (tyLvl - 1).max(0)
    //         val res = Term(defInfo, valLvl)
    //         items += (defInfo.id -> res)
    //       }
    //       List(EnumDestruct(lhs, variant, bindings, None)) :+ valTerm(body)
    //     },
    //   );
    //   matchBody = matchBody :+ (variant, Region(stmts.getOrElse(List())))
    // }

    // val defaultCaseItem = defaultCase.getOrElse(Unreachable)
    // TypeMatch(lhs, ty, matchBody, defaultCaseItem)

    // checkedDestructed(restTy)
    ValueMatch(lhs, lhsTy, patterns, Some(Unreachable))
  }

  /// Declarations

  def noteDecl(id: DeclItem) = items += (id.id.id -> id)
  def checkDecl[T](id: DeclExpr, f: => T): T =
    if checkStatus.contains(id.id.id.id) then throw new Exception("recursive")
    checkStatus += (id.id.id.id.toLong -> ());
    val res = f
    checkStatus -= id.id.id.id
    res

  def checkVar(v: VarExpr): ir.Var = {
    val VarExpr(info, oty, initExprE) = v
    info.isVar = true;
    val initExpr = initExprE.map(valTerm).map(normalize)
    val initTy = (oty.map(tyTerm), info.name) match {
      case (Some(ty), _)  => Some(ty)
      case (None, "self") => Some(RefItem(SelfTy, false))
      case _              => initExpr.flatMap(tyOf)
    }
    val valLvl = (initTy.map(_.level).getOrElse(0) - 1).max(0)
    val res = ir.Var(info, initExpr, valLvl)
    info.ty = initTy.getOrElse(createInfer(info, valLvl + 1))
    items += (info.id -> initExpr.getOrElse(res))
    res
  }

  def checkDestruct(d: DestructExpr): Item = {
    ???
  }

  def resolveParams(params: Option[EParams]) = params.map { params =>
    params.map(p => Param(checkDecl(p, checkVar(p)), false))
  }

  def checkDef(e: DefExpr) = {
    debugln(s"defItem ${e.id.name}")
    val DefExpr(info, ps, constraints, ret_ty, rhs) = e

    val params = resolveParams(ps);
    val annotated = ret_ty.map(tyTerm)
    val infer = annotated.getOrElse(createInfer(info, 1));
    val fn = Fn(info, params, infer, None, 0);
    noteDecl(fn);
    val body = rhs.map(e => normalize(valTerm(e)))
    val bodyTy = body.flatMap(tyOf)
    debugln(s"defItem $info, $bodyTy <: $annotated")
    // we have already checked annotated <: bodyTy when we are
    // making valTerm.
    val sigRetTy = annotated.orElse(bodyTy).getOrElse(infer)
    val level = (sigRetTy.level - 1).max(0);
    val fn2 = fn.copy(ret_ty = sigRetTy, body = body, level)

    info.isDependent =
      if level > 0 then body.map(isDependent).getOrElse(false) else false

    noteDecl(fn2);
    fn2
  }

  def checkClass(e: ClassExpr): Class = {
    val ClassExpr(info, ps, constraints, fields) = e
    val ss = selfRef; selfRef = Some(e);

    val params = resolveParams(ps);
    val cls2 = Class(info, params, fields); selfRef = Some(cls2);
    noteDecl(cls2);
    println(s"check class ${fields.values.toSeq}")
    for (f <- fields.values.toSeq) {
      fields.addOne(f.name -> checkField(f))
      println(s"checked field ${f.name}")
    }
    val cls = Class(info, params, fields)
    info.ty = cls

    selfRef = ss
    noteDecl(cls);
    cls
  }

  def checkImpl(e: ImplExpr) = {
    val ImplExpr(info, ps, constraints, i, c, fields) = e
    val cls = tyTerm(c);
    val ss2 = selfRef; selfRef = Some(cls);
    val ss = selfImplRef; selfImplRef = Some(e);

    val iface = i.map(tyTerm);
    val params = resolveParams(ps);
    val impl2 = Impl(info, params, iface.get, cls, fields);
    selfRef = Some(impl2);
    noteDecl(impl2);
    for (f <- fields.values.toSeq) {
      fields.addOne(f.name -> checkField(f))
    }
    val impl = Impl(info, params, iface.get, cls, fields)
    info.ty = impl;
    associateImpl(impl, impl.cls)

    selfRef = ss2; selfImplRef = ss
    noteDecl(impl);
    impl
  }

  def checkField(f: VField) = {
    f match {
      case e: VarField =>
        VarField(checkVar(e.item.id.syntax.asInstanceOf[VarExpr]), e.index)
      case e: EDefField  => DefField(checkDef(e.item))
      case e: EEnumField => EnumField(checkClass(e.item))
      case _             => ???
    }
  }

  def associateImpl(impl: Impl, cls: Type): Unit = {
    if (canonicalTy(cls).isBuilitin) {
      return associateImpl(impl, classRepr(cls))
    }

    debugln(s"associateImpl $impl to $cls")
    val id = cls match {
      case cls: Class => cls.id
      case v: Ref     => v.id
      case _ =>
        errors = s"Invalid impl target $cls" :: errors
        return
    }
    id.impls = id.impls :+ impl
  }
}
