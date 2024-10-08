package cosmo

import scala.collection.mutable.{
  ListBuffer,
  LinkedHashMap as MutMap,
  ArrayBuffer as MutArray,
  LongMap as MutLongMap,
}
import scala.annotation.tailrec

import ir._
import ir.typed._
import cosmo.system._
import cosmo.FileId
import cosmo.syntax.Ident

type De = DeclExpr

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

class Env(val source: Source, val pacMgr: cosmo.PackageManager)
    extends ExprEnv
    with TypeEnv {

  def fid = source.fid

  var noCore = false
  var syntaxOnly = false

  var defAlloc = DEF_ALLOC_START
  var defs = List[DefInfo]()
  var currentDef: Option[DefId] = None
  var currentRegion = (-1, -1)
  var defParents = Map[DefId, (Option[DefId], (Int, Int))]()
  var scopes = new Scopes()
  var errors: List[String] = List()
  var ns: List[String] = List()
  var builtinClasses = Map[Term | Expr, Class]()
  var selfRef: Option[Term] = None
  var selfImplRef: Option[Term] = None
  var rawDeps = Map[FileId, Option[Env]]()
  var checkStatus = MutLongMap[Unit]()

  var checked = MutLongMap[Term]()
  var items: Map[DefId, Term] = Map()

  var typeSym = DefInfo.just(0, this)

  val patHolder = Str("$")
  var moduleAst: untyp.Region = untyp.Region(List(), false)
  var module: typed.Region = typed.Region(List(), false)

  /// Builtin Items

  def builtins() = {
    newBuiltin("print")
    newBuiltin("println")
    newBuiltin("decltype")
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
    newType("cstd", CIdent("std", List(), UniverseTy))
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
    val cls = Class.empty(this, false).copy(id = info, resolvedAs = Some(ty));
    builtinClasses += (ty -> cls)
    items += (info.id -> ty)
  }

  /// Entry

  def exprStage(ast: syntax.Block): Env = {
    typeSym = scopes.get("Type").get
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

    moduleAst = untyp.Region(ast.stmts.map(expr), true)
    this
  }

  def evalStage: Env = {
    typeSym = scopes.get("Type").get
    module =
      if (syntaxOnly) then Region(List(), true)
      else valTerm(moduleAst).asInstanceOf[Region]
    this
  }

  def report: Env = {
    for (error <- errors) {
      debugln(error)
    }
    this
  }

  /// Item Creation

  def err(msg: String): NoneKind =
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

  /// Creates Reference
  def byRef(info: DefInfo)(implicit anno: Term): Term = {
    items.get(info.id) match {
      case Some(v) => return v
      case None    =>
    }
    val v = checked.get(info.id.id.toLong).map(deref)
    debugln(s"byRef $info")
    v match {
      case Some(v: Ref) => v
      case _            => Ref(info, value = v)
    }
  }

  def valTermO(node: Option[Expr]): Term =
    node.map(valTerm).getOrElse(NoneItem)
  def valTerm(node: Term | Expr): Term = term(node)(TopTy)
  def tyTerm(node: Term | Expr): Type = term(node)(UniverseTy)
  // todo: unify
  def unify(lhs: Term, rhs: Term): Term = lhs
  def termTyped(lhs: Term)(implicit rhs: Term): ir.Term = lhs match {
    case v: Value      => v
    case d: DefExpr    => d
    case t: SimpleType => t
    case o: Opaque     => o
    case r @ Ref(id, v) if rhs == UniverseTy =>
      items.get(id.id).getOrElse(r)
    case r: Ref                => r
    case BinOp(op, lhs, rhs)   => binOp(op, termTyped(lhs), termTyped(rhs))
    case Apply(lhs, rhs)       => $apply(termTyped(lhs), rhs.map(termTyped))
    case BinInst(op, lhs, rhs) => binInst(op, termTyped(lhs), termTyped(rhs))
  }
  def term(l: Term | Expr)(implicit rhs: Term): ir.Term = {
    val lhs = l match {
      case l: Term => return termTyped(l)(rhs)
      case l: Expr => l
    }
    debugln(s"term $lhs: $rhs")
    // solution
    val sol = lhs match {
      // control flow, todo: duplicate patterns
      case item: untyp.Break       => Break()
      case item: untyp.Continue    => Continue()
      case untyp.Opaque(x, y)      => Opaque(x, y)
      case untyp.Return(v)         => Return(term(v))
      case untyp.If(cond, x, y)    => If(term(cond), term(x), y.map(term))
      case untyp.Loop(body)        => Loop(term(body))
      case untyp.While(cond, body) => While(term(cond), term(body))
      case untyp.For(name, iter, body) =>
        For(term(name), term(iter), term(body))
      case untyp.Region(stmts, semi) => Region(stmts.map(term), semi)
      // operations
      case untyp.UnOp("&", untyp.UnOp("mut", lhs)) => RefItem(term(lhs), true)
      case untyp.UnOp("&", lhs)                    => RefItem(term(lhs), false)
      case untyp.UnOp("mut", lhs) =>
        errors = s"mut must be used after &" :: errors; term(lhs)
      case untyp.UnOp("*", lhs)      => derefPtr(lhs)
      case untyp.UnOp(op, lhs)       => UnOp(op, term(lhs))
      case untyp.BinOp(op, lhs, rhs) => binOp(op, term(lhs), term(rhs))
      case untyp.As(lhs, rhs)        => As(term(lhs), tyTerm(rhs))
      case untyp.KeyedArg(k, v)      => KeyedArg(term(k), term(v))
      // todo: check is compile time
      case untyp.SelectExpr(lhs, rhs) => deref(select(term(lhs), rhs))
      case untyp.Apply(lhs, rhs)      => $apply(term(lhs), rhs.map(term))
      case untyp.TmplApply(lhs, strings, args) =>
        tmplApply(term(lhs), strings, args.map { case (a, b) => (term(a), b) })
      case b: untyp.MatchExpr => checkMatch(b)
      case untyp.ItemE(item)  => item
      // todo: typed and untyed expressions
      case i: untyp.BinInst => BinInst(i.op, term(i.lhs), term(i.rhs))
      // declarations
      case De(v: VarExpr)   => v.checked
      case De(d: DefExpr)   => d.checked
      case De(c: ClassExpr) => c.checked
      case De(i: Impl)      => i.checked
      case De(Name(id, None)) =>
        val t = byRef(id);
        t match {
          case Ref(_, None) => err(s"undefined reference $id")
          case _            =>
        }
        t
      case De(Name(id, Some(of))) =>
        val t = byRef(of);
        t match {
          case Ref(_, None) => err(s"undefined reference $id")
          case _            =>
        }
        t
      case De(h: Hole)           => Hole(h.id)
      case d: untyp.DestructExpr => checkDestruct(d)
      case t: untyp.TupleLit     => TupleLit(t.elems.map(term))
      case cr: untyp.CaseRegion  => err(s"case region $cr in the air")
      case c: untyp.CaseExpr =>
        errE(s"case clause without match")
        Opaque.expr(s"0/* error: case clause without match $c */")
    }
    debugln(s"term $lhs: $rhs => $sol")
    sol match {
      case t: ExprTerm => {
        lhs.info.sol = sol
        t.info = lhs.info
      }
      case _ =>
    }
    sol
  }

  /// imports

  def $import(p: syntax.Node, dest: Option[syntax.Node]): Term = {
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

  def importNative(p: syntax.Node, dest: Option[syntax.Node]): Term = {
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

  def derefPtr(lhs: Expr)(implicit anno: Term): Term = {
    lhs match {
      case untyp.ItemE(SelfVal) => SelfVal
      case lhs                  => UnOp("*", term(lhs))
    }
  }

  def deref(lhs: Term)(implicit anno: Term): Term = {
    lhs match {
      case f: DefExpr if f.params.isEmpty => $apply(f, List())
      case BoundField(_, by, _, EnumField(ev: Class)) if ev.justInit =>
        $apply(lhs, List())
      case cls: Class if cls.justInit =>
        $apply(cls, List())
      case _ => lhs
    }
  }

  def binOp(op: String, lhs: Term, rhs: Term): Term = {
    val lhsTy = tyOf(lhs)
    val rhsTy = tyOf(rhs)
    debugln(s"binOp $lhsTy $op $rhsTy")

    (lhsTy, op) match {
      case (_, "<:") => Bool(isSubtype(lhs, rhs))
      case (l: IntegerTy, s @ ("+" | "-" | "*" | "/")) =>
        if (rhsTy != l) {
          err(s"mismatched types $lhsTy $s $rhsTy")
          return BinOp(s, lhs, rhs)
        }
        val op = s match {
          case "+" => BinInstIntOp.Add
          case "-" => BinInstIntOp.Sub
          case "*" => BinInstIntOp.Mul
          case "/" => BinInstIntOp.Div
        }
        binInst(BinInstOp.Int(l, op), lhs, rhs)
      case _ => BinOp(op, lhs, rhs)
    }
  }

  def binInst(op: BinInstOp, lhs: Term, rhs: Term): Term = {
    (op, lhs, rhs) match {
      case (BinInstOp.Int(ir.IntegerTy(64, _), op), Int64(l), Int64(r)) =>
        op match {
          case BinInstIntOp.Add => Int64(l + r)
          case BinInstIntOp.Sub => Int64(l - r)
          case BinInstIntOp.Mul => Int64(l * r)
          case BinInstIntOp.Div => Int64(l / r)
          case BinInstIntOp.Rem => Int64(l % r)
          case BinInstIntOp.And => Int64(l & r)
          case BinInstIntOp.Or  => Int64(l | r)
          case BinInstIntOp.Xor => Int64(l ^ r)
          case BinInstIntOp.Shl => Int64(l << r)
          case BinInstIntOp.Shr => Int64(l >> r)
          case BinInstIntOp.Sar => Int64(l >>> r)
          case BinInstIntOp.Eq  => Bool(l == r)
          case BinInstIntOp.Ne  => Bool(l != r)
          case BinInstIntOp.Lt  => Bool(l < r)
          case BinInstIntOp.Le  => Bool(l <= r)
          case BinInstIntOp.Gt  => Bool(l > r)
          case BinInstIntOp.Ge  => Bool(l >= r)
        }
      case (op, lhs, rhs) => BinInst(op, lhs, rhs)
    }
  }

  def select(lhs: Term, field: String)(implicit anno: Term): Term = {
    debugln(s"select $lhs ${lhs.getClass().getName()} $field")
    val x = dispatch(lhs, lhs, field, false)
    debugln(s"select $lhs $field => $x")
    x.getOrElse(Select(lhs, field))
  }

  def dispatch(lhs: Term, by: Term, field: String, casted: Boolean)(implicit
      anno: Term,
  ): Option[Term] = {
    debugln(s"dispatch select($field, $casted) of $lhs by $by")
    def contDispatch(by: Term, nextCasted: Boolean) =
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

    def dField(fields: FieldMap, field: String): Option[VField] = {
      fields.get(field).map {
        case e: (EDefField | EEnumField) =>
          val f = checkField(e)
          fields.addOne(f.name -> f); f
        case f => f
      }
    }

    def dFields(by: Term, v: FieldMap) =
      dField(v, field).map(BoundField(lhs, by, casted, _)).map(ls)

    def dImpls(id: DefInfo): Option[Term] = {
      val impls = id.impls.flatMap { i =>
        dField(i.fields, field).map(BoundField(lhs, i, true, _)).map(ls)
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
      case RefItem(r, isMut) => dispatch(lhs, r, field, casted)
      case Ref(_, Some(v))   => dispatch(lhs, v, field, casted)
      case Ref(id, None)     => dispatch(lhs, id.ty, field, casted)
      case NativeModule(info, env) =>
        return Some(env.scopes.get(field).map(env.byRef).getOrElse {
          errors = s"Undefined item $field in ${info.name}" :: errors
          Unresolved(ct(field))
        })
      case CModule(id, kind, path) => return Some(CIdent(field, List(), anno))
      case CIdent(ns0, ns, lvl)    => return Some(CIdent(field, ns :+ ns0, lvl))
      case i: VarExpr              => dispatch(lhs, i.id.ty, field, casted)
      case i: Impl =>
        dFields(i, i.fields).orElse(contDispatch(i.cls, true))
      case c: Class => dFields(c, c.fields).orElse(dImpls(c.id))
      case ClassInstance(con, _) =>
        dispatch(lhs, con, field, casted)
          .orElse(con.variantOf.flatMap(dispatch(lhs, _, field, true)))
      case HKTInstance(ty, syntax) =>
        val vRes = dispatch(lhs, ty, field, casted).get
        Some(HKTInstance(vRes, Select(syntax, field)))
      case i: (Int64 | Float32 | Float64 | Str | Bool) =>
        dispatch(lhs, tyOf(i), field, casted)
      case i: (IntegerTy | FloatTy) =>
        dispatch(lhs, builtinClasses(i), field, casted)
      case i @ (StrTy | BoolTy) =>
        dispatch(lhs, builtinClasses(i), field, casted)
      case _ => None
    }
  }

  def $apply(lhs: Term, rhs: List[Term])(implicit anno: Term): Term = {
    debugln(s"apply $lhs |||| ${rhs}")
    lhs match {
      case Unresolved(id2) if id2.id.id == CODE_FUNC =>
        return rhs.head match {
          case Str(content) => Opaque.stmt(content)
          case e: Opaque    => e
          case _            => Opaque.expr("0 /* code */")
        }
      case Ref(id, Some(RefTy(isRef, isMut))) =>
        assert(rhs.length == 1)
        return if (isRef) {
          val r = rhs.head;
          checkedMut(r, isMut);
          RefItem(r, isMut)
        } else {
          rhs.head
        }
      // todo: call universe type
      case UniverseTy =>
        rhs.headOption.map(lift).map(eval).getOrElse(UniverseTy)
      case Ref(id, Some(v))                            => $apply(v, rhs)
      case v: CIdent if rhs.exists(_.ty == UniverseTy) => CppInsType(v, rhs)
      case BoundField(_, by, _, EnumField(ev)) =>
        applyC(ev.copy(variantOf = Some(by)), rhs)
      case BoundField(that, by, _, DefField(f)) =>
        Apply(lhs, castArgs(f.params, rhs))
      case f: DefExpr => applyF(f, rhs)
      case c: Class   => applyC(c, rhs)
      case HKTInstance(ty, syntax) =>
        val res = hktTranspose(syntax, $apply(ty, rhs))
        if (res.isValLevel) {
          res
        } else {
          HKTInstance(res, Apply(syntax, rhs))
        }
      case _ => Apply(lhs, rhs)
    }
  }

  def tmplApply(
      lhs: Term,
      strings: List[String],
      rhs: List[(Term, Option[String])],
  )(implicit anno: Term): Term = {
    debugln(s"tmplApply $lhs $strings $rhs")
    lhs match {
      case Name(i, _) if i.name == "s" =>
        debugln(s"tmplApply $lhs $strings $rhs")
        val t = Opaque.expr("cosmo::mkString")
        val args = ListBuffer[Term]()
        val sIter = strings.iterator
        val rIter = rhs.iterator
        args.addOne(Str(sIter.next()))
        while (sIter.hasNext) {
          val (r, opt) = rIter.next()
          args.addOne(fmtItem(r, opt))
          val s = sIter.next()
          args.addOne(Str(s))
        }
        Apply(t, args.toList)
      case _ => Opaque.expr(s"0 /* tmplApply: ${lhs}, ${strings}, ${rhs} */")
    }
  }

  def fmtItem(v: Term, opt: Option[String])(implicit anno: Term) = v match {
    case s: Str                => s
    case Int64(value)          => Str(value.toString)
    case Float32(value)        => Str(value.toString)
    case Float64(value)        => Str(value.toString)
    case Opaque(Some(expr), _) => Str(expr)
    case _                     => runtimeToStr(v)
  }

  def runtimeToStr(v: Term)(implicit anno: Term) = {
    $apply(deref(select(v, "toString")), List())
  }

  def applyF(fn: DefExpr, args: List[Term])(implicit anno: Term): Term = {
    if (fn.isValLevel && anno != UniverseTy) then
      return Apply(fn, castArgs(fn.params, args))
    val f = fn.checked
    return scopes.withScope {
      val castedArgs = f.params.zip(args).map { case (p, a) =>
        val info = p.id
        scopes.set(info.name, info)
        val casted = castTo(a, info.ty)
        items += (info.id -> casted)
        // todo: constrain types
        casted
      }
      val value = f.body.map(lift).map(eval).getOrElse(NoneItem)
      hktRef(Some(f), castedArgs.toList, value)
    }
  }

  def applyC(node: Class, args: List[Term]): Term = {
    debugln(s"applyClass ${node.id} ${node.variantOf} $args")
    val typeParams = node.rawParams
    val tpSize = typeParams.map(_.size).getOrElse(0);
    val allArgs = node.args.getOrElse(List()) ::: args
    val al = allArgs.length;
    val isTypeLevel =
      if typeParams.isEmpty then false
      else al < tpSize || (al == tpSize && !args.isEmpty)
    if (isTypeLevel) {
      return node.copy(args = Some(allArgs))
    }

    val castedArgs = castArgs(node.params, allArgs)
    val typeLevelArgs = typeParams.map(p => castedArgs.take(tpSize))

    val clsFinal = node.copy(args = typeLevelArgs)
    ClassInstance(clsFinal, castedArgs.drop(tpSize))
  }

  def checkMatch(b: untyp.MatchExpr)(implicit anno: Term): Term = {
    import MatchM._
    val lhs = term(b.lhs)
    val lhsShape = curryTermView(lhs);

    debugln(s"checkMatch $lhs ($lhsShape) on ${b.body}")
    val state = b.body.cases.foldLeft(Init) {
      case (m, untyp.CaseExpr(destructor, bodyExpr)) =>
        val pattern = matchPat(lhsShape, curryExpr(destructor))
        m.addPattern(lhs, pattern, valTermO(bodyExpr))
    }

    state.finalize(id => id)
  }

  // match state machine
  enum MatchM {

    case Init; case Finalized(body: Term);
    case MVar(lhs: Term, rhs: VarExpr, inner: MatchM)
    case MValue(lhs: Term, values: List[(Term, MatchM)])
    case MEnum(lhs: Term, use: Class, cases: MutMap[DefId, MatchM])
    case MClass(cd: ClassDestruct, inner: MatchM)

    type M = MatchM
    type CD = ClassDestruct
    type ED = EnumDestruct
    type UD = MatchM => MatchM

    def addPattern(lhs: Term, pattern: Term, body: => Term): M = {
      addOne(lhs, pattern, dropCase(body))
    }

    def finalize(body: UD = id => id): Term = {
      val res = doFinalize(body)
      debugln(s"finalize $this => $res")
      res match {
        case Init            => NoneItem
        case Finalized(body) => body
        case _               => err(s"unreachable pattern")
      }
    }

    def dropCase(body: => Term): UD = {
      case Init => Finalized(body)
      case m    => err(s"cannot add body to state $m, with body $body"); m
    }

    def coverageCheck(body: UD, ifEscaping: => Unit): Term = {
      val orElse = body(Init)
      orElse match {
        case Init => ifEscaping
        case _    =>
      }

      orElse.finalize(_ => Finalized(Unreachable))
    }

    def addOne(lhs: Term, pattern: Term, body: UD): M = pattern match {
      case _ if this.isInstanceOf[MatchM.Finalized] =>
        err(s"unreachable pattern $pattern"); this
      case _: NoneKind => this
      case Bool(constantly) => {
        if !constantly then err(s"unreachable pattern $pattern")
        val cont = if constantly then body else dropCase(Unreachable)
        this.doFinalize(cont)
      }
      case v: VarExpr                => this.addVar(lhs, v, body)
      case typed.BinOp("==", _, rhs) => this.addValue(lhs, rhs, body)
      case b: typed.BinOp            => debugln(s"binop $b"); ???
      case ed: EnumDestruct          => this.addEnum(lhs, ed, body)
      case cd: ClassDestruct         => this.addClass(lhs, cd, body)
      case _ => throw new Exception(s"cannot add invalid pattern $this")
    }

    final def addVar(lhs: Term, rhs: VarExpr, body: UD): M = this match {
      case Init => MVar(lhs, rhs, body(Init))
      case MVar(u, v, inner) =>
        err(
          s"exhaused case, already bind lhs before, lhs is $lhs, bound $v, expected rebind $rhs",
        );
        this
      case v: (MValue | MEnum | MClass) =>
        v.doFinalize(_.addVar(lhs, rhs, body))
      case state => throw new Exception(s"cannot add var at state $state")
    }

    def addValue(lhs: Term, v: Term, body: UD) = this match {
      // todo: value coverage checking
      case MValue(l, values) => MValue(l, values :+ (v, body(Init)))
      case Init              => MValue(lhs, List((v, body(Init))))
      case state => throw new Exception(s"cannot add value at state $state")
    }

    @tailrec
    final def addClass(lhs: Term, c: CD, body: UD): M = this match {
      case Init =>
        debugln(s"addClass init $c $body")
        val params = classBinds(c.cls).map {
          case d: DeclItem => genVar(d.id, None)
          case v           => genVar(ct("b"), Some(v))
        }
        if params.isEmpty then return doFinalize(body)
        val cd = ClassDestruct(lhs, c.cls, params)
        MClass(cd, Init).addClass(lhs, c, body)
      case MClass(cd, inner) =>
        if (cd.cls.id.id != c.cls.id.id) {
          err(s"cannot mix classes match previous $cd, next $c")
          return this
        }
        // todo: O(n) length check
        val paramSize = cd.bindings.length;
        if (c.bindings.length != paramSize) {
          throw new Exception(s"bindings length mismatch $cd $c")
        }

        var pairs = cd.bindings.zip(c.bindings).iterator;
        def binder(m: M): M = {
          pairs.nextOption() match {
            case Some((lv, rv)) => m.addOne(lv, rv, binder)
            case None           => body(m)
          }
        }

        binder(inner) match {
          case Finalized(body) => doFinalize(dropCase(body))
          case next            => MClass(cd, next)
        }
      case state => throw new Exception(s"cannot add class at state $state")
    }

    @tailrec
    final def addEnum(lhs: Term, e: ED, body: UD): M = this match {
      case Init =>
        val cls = e.variant
        val variantOf = cls.variantOf.get.asInstanceOf[Class]
        if (variantOf.variantOf.isDefined) then
          throw new Exception("nested enum")
        debugln(s"addEnum init $e $cls $variantOf $body")
        MEnum(lhs, variantOf, MutMap()).addEnum(lhs, e, body)
      case MEnum(lhs, use, cases) =>
        val cls = e.variant
        val variantOf = cls.variantOf.get.asInstanceOf[Class]
        if (variantOf.variantOf.isDefined) then
          throw new Exception("nested enum")
        if (use.id.id != variantOf.id.id) {
          err(s"cannot mix enums match previous $use, next $variantOf")
          return this
        }
        debugln(s"addEnum cont $e $cls $variantOf $body")
        val contD = ClassDestruct(e.item, e.variant, e.bindings)
        cases.updateWith(cls.id.id) { m =>
          Some(m.getOrElse(Init).addClass(lhs, contD, body))
        }
        this
      case state => throw new Exception(s"cannot add enum at state $state")
    }

    // rename: Option[Ref],
    def doFinalize(body: UD): M =
      this match {
        case Init         => body(Init)
        case b: Finalized => b
        case MVar(_, rename, inner) if rename.id.name == "_" =>
          Finalized(inner.finalize(body))
        case MVar(t, v, inner) =>
          v.doCheck(t.ty, Some(t))
          Finalized(placeVal(v, inner.finalize(body)))
        case MValue(lhs, values) =>
          debugln(s"finalize values $values $body")
          val cases = values.map { case (v, m) =>
            (v, m.finalize(body))
          }

          val orElse = coverageCheck(
            body, {
              err(s"must add a branch to close value match when matching $lhs")
            },
          );
          Finalized(ValueMatch(lhs, tyOf(lhs), cases, orElse))
        case MClass(cd, inner) =>
          debugln(s"finalize classes $cd $body")
          Finalized(placeVal(cd, inner.finalize(body)))
        case MEnum(lhs, use, caseMs) =>
          debugln(s"finalize enums $use $caseMs $body")
          val useVariants = use.variants
          val numOfVariants = useVariants.length
          var cases = ListBuffer[(Class, Term)]()
          val missings = ListBuffer[EnumField]()
          for (v <- useVariants) {
            caseMs.get(v.item.id.id) match {
              case None =>
                missings.addOne(v)
              case Some(state) =>
                cases.addOne((v.item, state.finalize(body)))
            }
          }

          val orElse = coverageCheck(
            body, {
              for (v <- missings) {
                err(s"missing case $v when matching $lhs on $use")
              }
            },
          );
          Finalized(TypeMatch(lhs, use, cases.toList, orElse))
      }
  }

  def placeVal(v: Term, m: Term): Term = m match {
    case Region(stmts, semi) => Region(v +: stmts, semi)
    case m                   => Region(List(v, m), false)
  }

  /// Declarations

  def noteDecl(id: DeclItem) = items += (id.id.id -> id)
  def noteDeclTerm(id: DeclTerm) = items += (id.id.id -> id)
  def checkDecl[T <: Term](id: DeclTerm, f: => T): T =
    val intId = id.id.id.id
    if checked.contains(intId) then return checked(intId).asInstanceOf[T]
    if checkStatus.contains(intId) then throw new Exception("recursive")
    checkStatus += (intId.toLong -> ());
    val res = f
    checkStatus -= intId
    checked += (intId.toLong -> res)
    res

  def genVar(id: DefInfo, init: Option[Term]): VarExpr = {
    val nv = ct(id.name + s"_${defAlloc + 1}")
    VarExpr.generate(nv, id.ty, init)
  }

  def checkVar(v: VarExpr): ir.VarExpr = {
    debugln(s"checkVar $v")
    val VarExpr(info, oty, initExprE) = v
    info.isVar = true;
    val initExpr = initExprE.map(valTerm).map(normalize)
    val initTy = oty.map(tyTerm).orElse {
      info.name match {
        case "self" => Some(RefItem(SelfTy, false))
        case _      => initExpr.map(tyOf)
      }
    } match {
      case Some(t) => t
      case None =>
        info.ty = UniverseTy
        createInfer(info)
    }

    v.doCheck(initTy, initExpr)
    v
  }

  def checkDestruct(d: untyp.DestructExpr): Term = {
    ???
  }

  def resolveParams(e: FuncLike) = e.synParams.map { params =>
    params.map(p => { p.checked; Param(p, false) })
  }

  def checkDef(e: DefExpr) = {
    debugln(s"defItem ${e.id.name}")
    val DefExpr(info, retTyE, rhs) = e

    val params = resolveParams(e);
    e.doCheckParams(params)
    val annotated = retTyE.map(tyTerm)
    val infer = annotated.getOrElse(createInfer(info));
    e.doCheckRetTy(infer)
    checked += (info.id.id.toLong -> e)
    noteDecl(e);
    val body = rhs.map(e => normalize(valTerm(e)))
    val bodyTy = body.map(tyOf)
    debugln(s"defItem $info, $bodyTy <: $annotated")
    // we have already checked annotated <: bodyTy when we are
    // making valTerm.
    val sigRetTy = annotated.orElse(bodyTy).getOrElse(infer)
    e.doCheckBody(sigRetTy, body)

    info.isDependent =
      if e.isTypeLevel then body.map(isDependent).getOrElse(false) else false

    e
  }

  def checkClass(e: ClassExpr): Class = {
    val (ClassExpr(info), fields) = (e, e.fields)
    val ss = selfRef; selfRef = Some(e);

    val params = resolveParams(e);
    val cls2 = Class(info, params, fields); selfRef = Some(cls2);
    noteDecl(cls2);
    for (f <- fields.values.toSeq) {
      fields.addOne(f.name -> checkField(f))
    }
    val cls = Class(info, params, fields)
    info.ty = cls

    selfRef = ss
    noteDecl(cls);
    cls
  }

  def checkImpl(e: Impl) = {
    val (Impl(info, i, c), fields) = (e, e.fields)
    e.rawParams = resolveParams(e);
    val cls = tyTerm(c);
    e.cls = cls;
    val ss2 = selfRef; selfRef = Some(cls);
    val ss = selfImplRef; selfImplRef = Some(e);
    noteDeclTerm(e);

    e.iface = i.map(tyTerm).get;
    for (f <- fields.values.toSeq) {
      fields.addOne(f.name -> checkField(f))
    }
    info.ty = e;
    associateImpl(e, cls)

    selfRef = ss2; selfImplRef = ss
    e
  }

  def checkField(f: VField) = {
    f match {
      // todo: unchecked var field is dangerous
      case e: VarField =>
        e.item.checked
        e
      case e: EDefField  => DefField(checkDecl(e.item, checkDef(e.item)))
      case e: EEnumField => EnumField(checkDecl(e.item, checkClass(e.item)))
      case _             => ???
    }
  }

  def associateImpl(impl: Impl, cls: Type): Unit = {
    if (canonicalTy(cls).isBuilitin) {
      return associateImpl(impl, classRepr(cls).get)
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

  def storeTy(ty: Type)(implicit exprRec: Term => String): String = {
    debugln(s"storeTy $ty")
    ty match {
      case IntegerTy(size, isUnsigned) =>
        s"${if (isUnsigned) "u" else ""}int${size}_t"
      case FloatTy(size) => s"float${size}_t"
      case UnitTy        => "void"
      case BoolTy        => "bool"
      case StrTy         => "::str"
      case SelfTy        => "self_t"
      case TopTy         => "auto"
      case BottomTy      => "void"
      // todo: int32_t
      // case ty: Int32     => "int32_t"
      case ty: Int64   => "int64_t"
      case ty: Float32 => "float32_t"
      case ty: Float64 => "float64_t"
      case ty: Str     => "::str"
      // "decltype((*this).internal.begin())"
      case ty: Hole        => throw new Exception(s"hole $ty cannot be stored")
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
      // todo: this is not good
      case ins: ClassInstance            => storeTy(ins.con)
      case v: Ref if v.value.isEmpty     => v.id.defName(stem = false)
      case v: VarExpr if v.initE.isEmpty => v.id.defName(stem = false)
      case v: DefExpr                    => v.id.defName(stem = false)
      // case Ref(id, Some(_))        => id.defName(stem = false)
      case Ref(_, Some(v)) => storeTy(v)
      case RefItem(lhs, isMut) =>
        s"${if (isMut) "" else "const "}${storeTy(lhs)}&"
      case Apply(Opaque(Some("decltype"), None), rhs) =>
        assert(rhs.length == 1, s"decltype $rhs")
        s"decltype(${exprRec(rhs.head)})"
      case Apply(lhs, rhs) => {
        val lhsTy = storeTy(lhs)
        val rhsTy = rhs.map(storeTy).mkString(", ")
        s"$lhsTy<$rhsTy>"
      }
      case ty => "auto"
    }
  }

  // sol match {
  //   case CppInsType(target, arguments) => CppInsType(target, arguments.map(e))
  //   case r @ Ref(id, value) if r.isTypeLevel    => e(items(id.id))
  //   case r @ VarExpr(id, _, _) if r.isTypeLevel => e(items(id.id))
  //   // init.map(valTerm).getOrElse(item)
  //   case BinInst(BinInstOp.Int(t, op), lhs, rhs) =>
  //     val l = e(lhs).asInstanceOf[Int64].value
  //     val r = e(rhs).asInstanceOf[Int64].value
  //     op match {
  //     }
  //   case Apply(lhs, rhs) => $apply(lhs, rhs)(UniverseTy)
  // }

  def run(sol: Term): Term = {
    debugln(s"run $sol")
    term(sol)(UniverseTy)
  }

  def compile(info: ExprInfo): (v: Env) => Term = {
    if info.jit != null then return info.jit
    info.jit = info.sol match {
      case _ => _.run(info.sol)
    }
    info.jit
  }

  def eval(item: Term): Term = {
    debugln(s"eval $item")
    item.eval(this)
  }
}
