package cosmo

import scala.annotation.tailrec

import ir._
import syntax as s

trait TypeEnv { self: Env =>

  // : Computing Part

  enum PatShape {
    /// An undefined reference
    case Hole(id: Defo);
    /// Items that can be destruct/construct
    case Cons(v: Option[Item], ty: Class, isAdt: Boolean);
    /// Not interested by tele
    case Atom(v: Item, ty: Type);

    def getTy: Type = this match {
      case Cons(_, ty, _) => ty
      case Atom(_, ty)    => ty
      case _              => ???
    }
  }

  type ExtractedPat =
    NoneKind | Ref | Bool | BinOp | EnumDestruct | ClassDestruct

  final def matchPat(lhs: PatShape, rhs: PatShape): ExtractedPat = {
    import PatShape._;
    debugln(s"matchPat $lhs by $rhs")
    (lhs, rhs) match {
      case (lhs, Hole(id)) =>
        id.ty = lhs.getTy; // todo: id.ty assignment has side effect
        val t = Ref(id, (lhs.getTy.level - 1).max(0), None)
        items += (id.id -> t)
        t
      case (Hole(_), _) => err("cannot destruct hole")
      case (Atom(lhsTy, UniverseTy), Atom(rhsTy, UniverseTy)) =>
        return Bool(isSubtype(lhsTy, rhsTy))
      case (Cons(_, lCls, _), Atom(rhsTy, UniverseTy)) =>
        return Bool(isSubtype(lCls, rhsTy))
      case (_, Atom(rv, rhsTy)) =>
        return BinOp("==", patHolder, rv)
      case (lhs: Atom, rhs: Cons) => // todo: can cons primitives
        err(s"cannot destruct type ($lhs) by class ($rhs)")
      case (Cons(lv, lCls, lAdt), Cons(rv, rCls, true)) =>
        val related =
          rCls.variantOf.map(isSubtype(_, lCls)).getOrElse(false) ||
            isSubtype(rCls, lCls);
        if (!related) {
          return err(s"cannot destruct class $lCls by enum class $rCls")
        }

        val rhsBase = rCls.variantOf.get.asInstanceOf[Class];

        debugln(s"cls level $lCls, $rCls, ${rhsBase}")
        debugln(s"args level $lv, nothing")

        val args = rv match {
          case None     => classBinds(rCls)
          case Some(rv) => classArgs(rv, rCls)
        }
        val lhsParams = classParams(lv.get);
        val params = if (lhsParams.length < args.length) {
          lhsParams ::: classParams(rCls).drop(lhsParams.length)
        } else {
          lhsParams
        }

        val matched = matchSeq(params, args);
        EnumDestruct(patHolder, rCls, matched)
      case (Cons(lv, lCls, lAdt), Cons(rv, rCls, false)) =>
        if (rCls.id.isTrait) then return err("trait cannot be destructed")
        if ((!isClass(rCls, lCls))) then
          return err(s"class mismatch lhs: $lCls rhs: $rCls")

        rv match {
          case Some(rv) if eqClass(lCls, rCls) => {
            val args = classArgs(rv, rCls)
            val lhsParams = classParams(lv.get);
            val params = if (lhsParams.length < args.length) {
              lhsParams ::: classParams(rCls).drop(lhsParams.length)
            } else {
              lhsParams
            }

            val matched = matchSeq(params, args);
            ClassDestruct(patHolder, rCls, matched)
          }
          case None     => BinOp("<:", patHolder, rCls)
          case Some(rv) => BinOp("==", patHolder, rv)
        }
    }
  }

  def curryTerm(exp: Expr): PatShape = curryTermView(valTerm(exp))
  def curryTermView(lhs: Item): PatShape = {
    val lhsTy = tyOf(lhs) match
      case None     => throw Exception("cannot match a untyped value");
      case Some(ty) => ty;
    debugln(s"curryTermView $lhs % $lhsTy")
    curryView(lhs, lhsTy)
  }

  def curryExpr(v: Expr): PatShape = {
    debugln(s"curryExpr $v")
    v match {
      case Apply(lhs, rhs) =>
        val lhsTy = lhs match {
          case e: Expr => canonicalTy(valTerm(e))
          case e       => canonicalTy(e)
        }

        debugln(s"curryApplyE $lhs by $rhs")

        lhsTy match {
          case norm: Class =>
            val adt = enumShape(norm)
            val isAdt = adt.isDefined
            val cls = adt.getOrElse(norm)
            val args = TupleLit(rhs.toArray)
            PatShape.Cons(Some(args), cls, isAdt)
          case _ =>
            curryTermView(valTerm(v))
        }
      case Hole(id) => PatShape.Hole(id)
      case v        => curryTermView(valTerm(v))
    }
  }

  def curryView(v: Item, ty: Type): PatShape = {
    debugln(s"curryView $v $ty")
    if ty.isInstanceOf[ClassInstance] then throw Exception("what's you?")

    canonicalTy(v) match {
      case norm: Class =>
        val adt = enumShape(norm)
        val isAdt = adt.isDefined
        val cls = adt.getOrElse(norm)
        PatShape.Cons(None, cls, isAdt)
      case _ =>
    }

    canonicalTy(ty) match {
      case norm: Class =>
        val adt = enumShape(norm)
        val isAdt = adt.isDefined
        val cls = adt.getOrElse(norm)
        PatShape.Cons(Some(v), cls, isAdt)
      case _ => PatShape.Atom(v, ty)
    }
  }

  def matchSeq(lhs: List[Item], rhs: List[Item]): List[Item] = {
    debugln(s"matchSeq $lhs by $rhs")
    if (lhs.length != rhs.length) {
      err(s"matchSeq length mismatch $lhs by $rhs")
    }
    lhs.zip(rhs).map { case (l, r) => matchOne(l, r) }
  }

  def matchOne(lhs: Item, rhs: Item): Item = {
    debugln(s"matchOne $lhs by $rhs [${rhs.isInstanceOf[Expr]}]")
    val lhsView = curryTermView(lhs)
    val rhsView = rhs match
      case v: Expr if rhs.isInstanceOf[Expr] => curryExpr(v)
      case _                                 => curryView(rhs, tyOf(rhs).get)
    matchPat(lhsView, rhsView)
  }

  def matchParams[T](
      eParams: Array[Param],
      eArgs: List[Item],
      f: (Param, Option[Item]) => Option[T],
  ): List[T] = {
    if (eParams.length != eArgs.length) {
      errors =
        s"Invalid number of params v.s. arguments (${eParams.length} v.s. ${eArgs.length}) ${eParams.toList} v.s. $eArgs" :: errors
    }

    val named = eArgs.flatMap {
      case KeyedArg(ItemE(Str(s)), value) => Some((s, value))
      case KeyedArg(Str(s), value)        => Some((s, value))
      case v                              => None
    }.toMap
    var positions = eArgs.flatMap {
      case v: KeyedArg => None
      case v           => Some(v)
    }.iterator
    val allowNamedAsPositional = named.isEmpty

    eParams.iterator.flatMap { param =>
      if (param.of.name == "self") None // todo: skip self
      else if (param.named) {
        val name = param.of.name
        val value = named.get(name).orElse {
          if (allowNamedAsPositional) {
            positions.nextOption
          } else {
            None
          }
        }
        f(param, value)
      } else {
        f(param, positions.nextOption)
      }
    }.toList
  }

  def classBinds(cls: Class): List[Item] = {
    // take args and rest params
    val params = cls.params
    val args = cls.args.getOrElse(List())
    val variantBind = cls.variantOf
      .map {
        case v: Class => classBinds(v)
        case _        => List()
      }
      .getOrElse(List())
    variantBind ::: args ::: params.drop(args.length).toList
  }

  @tailrec
  final def classParams(
      v: Item,
      // cls: Class,
      // variant: Option[Class],
  ): List[Item] = {
    debugln(s"classParams $v")
    v match {
      case Ref(_, _, Some(v))           => classParams(v)
      case Var(id, _, _)                => classParams(id.ty)
      case Param(of, _)                 => classParams(of)
      case SelfVal if selfRef.isDefined => classParams(selfRef.get)
      case v: ClassInstance =>
        val params = classBinds(v.con);
        params.dropRight(v.args.length) ::: v.args
      case v: Class => classBinds(v)
      case v =>
        err(s"cannot extract class params $v")
        List()
    }
  }

  def classArgs(v: Item, cls: Class): List[Item] = {
    debugln(s"classArgs $v by $cls")
    v match {
      case v: ClassInstance =>
        if (eqClass(v.con, cls)) {
          return v.args
        }
        err(s"cannot destruct case 4 $v by $cls")
        v.args
      case TupleLit(items) =>
        var reorded =
          matchParams(cls.varsParams, items.toList, (param, value) => value)
        // todo: dropped all vars params
        val preArgs = classBinds(cls).dropRight(cls.varsParams.length);
        preArgs ::: reorded
      case v =>
        err(s"cannot destruct case 3 $v by $cls")
        List()
    }
  }

  def castArgs(
      eParams: Array[Param],
      eArgs: List[Item],
  ): List[Item] = {

    debugln(s"castArgs ${eParams.toList} $eArgs")

    matchParams(
      eParams,
      eArgs,
      (param, value) => {
        val res =
          if (param.of.name == "self") None
          else if (param.named) {
            Some(value.getOrElse {
              errors = s"Missing named argument ${param.of.name}" :: errors
              Opaque.expr(
                s"\n#error \"Missing named argument ${param.of.name}\"\n",
              )
            })
          } else {
            Some(value.getOrElse {
              errors = s"Missing positional argument" :: errors
              Opaque.expr(s"\n#error \"Missing positional argument\"\n")
            })
          }
        val info = param.id
        val casted = res.map(castTo(_, info.ty))
        casted.map { v =>
          items += (info.id -> v)
        }

        casted
      },
    )
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
              checkedMut(item, rhsIsMut);
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
              case tr: Class if tr.id.isTrait => {
                As(item.e, implClass(lty, tr).get.e)
              }
              case _ =>
                As(item.e, nty.e)
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

  // : Normalization Part

  def eval(item: Item)(implicit level: Int): Item = {
    debugln(s"eval $item $level")
    val e = eval;
    item match {
      case CppInsType(target, arguments) => CppInsType(target, arguments.map(e))
      case Ref(id, lvl, value) if level <= lvl => items(id.id)
      case _                                   => item
    }
  }

  def normalize(body: Item): Item = {
    debugln(s"normalize $body")
    body match {
      case _ => body
    }
  }

  // : Type Checker Part

  def checkedMut(item: Item, isMut: Boolean): Unit = {
    debugln(s"checkedMut $item $isMut")
    val lhsIsMut = item match {
      case RefItem(lhs, rhsIsMut) => return checkedMut(lhs, rhsIsMut)
      case SelfTy                 => return
      case v: Ref                 => v.id.isMut
      case v: Var                 => v.id.isMut
      case _                      => return
    }
    if (!lhsIsMut && isMut) {
      errors = s"Cannot cast to mut ref" :: errors
    }
  }

  def isDependent(body: Item): Boolean = {
    body match {
      case _: (CIdent | CppInsType) =>
        false
      case _ => true
    }
  }

  def isClass(child: Class, parent: Class): Boolean = {
    if (eqClass(child, parent)) {
      return true
    }
    if (child.variantOf.isDefined) {
      return eqClass(child.variantOf.get.asInstanceOf[Class], parent)
    }
    if (parent.variantOf.isDefined) {
      return eqClass(child, parent.variantOf.get.asInstanceOf[Class])
    }
    false
  }

  def eqClass(lhs: Class, rhs: Class): Boolean = {
    lhs.id.id.id == rhs.id.id.id
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
      case SelfVal                              => SelfTy
      case Ref(_, level, Some(v)) if level == 1 => canonicalTy(v)
      case v: Ref                               => canonicalTy(v.id.ty)
      case v: Var                               => canonicalTy(v.id.ty)
      case BoundField(_, by, _, EnumField(v))   => v.copy(variantOf = Some(by))
      case RefItem(lhs, isMut) => RefItem(canonicalTy(lhs), isMut)
      case ItemE(item)         => canonicalTy(item)
      // todo: canonical hkt types
      case _ => rhs
    }
  }

  def isSubtype(lhs: Item, rhs: Item): Boolean = {
    debugln(s"isSubtype $lhs $rhs")
    rhs match {
      case Ref(_, _, Some(v))  => isSubtype(lhs, v)
      case RefItem(rhs, isMut) => isSubtype(lhs, rhs)
      // todo: same level
      case cls: Class         => implClass(lhs, cls).isDefined
      case TopTy | UniverseTy => true
      case BottomTy           => false
      case IntegerTy(_, _) | StrTy | BoolTy if isBuiltin(lhs, rhs) => true
      case _ => {
        lhs match {
          case Ref(_, _, Some(v))  => isSubtype(v, rhs)
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
      case Ref(_, _, Some(v)) => isBuiltin(v, rhs)
      case TopTy | UniverseTy => true
      case BottomTy           => true
      case Integer(_)         => lhs == rhs || rhs == IntegerTy(32, false)
      case Bool(_)            => lhs == rhs || rhs == BoolTy
      case Str(_)             => lhs == rhs || rhs == StrTy
      case _                  => lhs == rhs
    }
  }

  final def weakClassRepr(lhs: Type): Either[Type, Class] = {
    val cls = classRepr(lhs)
    if (cls.id.isBuiltin) {
      Left(cls.resolvedAs.get)
    } else {
      Right(cls)
    }
  }

  @tailrec
  final def classRepr(lhs: Type): Class = {
    lhs match {
      case ClassInstance(con, _) => con
      case v: Class              => v
      case Ref(_, _, Some(v))    => classRepr(v)
      case RefItem(lhs, isMut)   => classRepr(lhs)
      case Var(id, _, _)         => classRepr(id.ty)
      case Param(of, _)          => classRepr(of)
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
      s"implClass? $goal(${goal.id.isTrait}) for $lhs ($cls) impls ${cls.id.impls} ",
    )
    if (cls.id == goal.id) {
      return Some(cls)
    }
    if (!goal.id.isTrait) {
      return None;
    }
    cls.id.impls.find { i => isSubtype(i.iface, goal) }
  }

  @tailrec
  final def enumShape(ty: Item): Option[Class] = {
    debugln(s"enumShape $ty")
    ty match {
      case v: Ref if v.value.isEmpty =>
        enumShape(items.getOrElse(v.id.id, NoneItem))
      case Ref(_, _, Some(v)) => enumShape(v)
      case BoundField(_, by, _, EnumField(v)) =>
        Some(v.copy(variantOf = Some(by)))
      case ClassInstance(con, _)             => enumShape(con)
      case v: Class if v.variantOf.isDefined => Some(v)
      case ty                                => None
    }
  }

  def tyOf(lhs: Item): Option[Type] = {
    debugln(s"tyOf $lhs (level ${lhs.level})")
    lhs match {
      case _: Integer => Some(IntegerTy(32, false))
      case _: Rune    => Some(IntegerTy(32, false))
      case _: Str     => Some(StrTy)
      case NoneItem   => Some(TopTy)
      case _: (Apply | Opaque | Select | Unresolved)  => Some(TopTy)
      case _: (While | Loop | For | Break | Continue) => Some(UnitTy)
      case Unreachable                                => Some(BottomTy)
      case _: (CIdent | TopKind | NoneKind | Class | CppInsType) =>
        Some(UniverseTy)
      case v if v.level == 1                 => Some(UniverseTy)
      case _: (CModule | NativeModule)       => Some(UniverseTy)
      case BoundField(_, _, _, EnumField(v)) => Some(UniverseTy)
      case RefItem(lhs, isMut)               => tyOf(lhs).map(RefItem(_, isMut))
      case v: ClassInstance                  => Some(v.con)
      case BoundField(_, _, _, VarField(v, _)) => Some(v.id.ty)
      case b: BinOp                        => coerce(tyOf(b.lhs), tyOf(b.rhs))
      case If(_, x, y)                     => coerce(tyOf(x), y.flatMap(tyOf))
      case SelfVal                         => Some(SelfTy)
      case Ref(id, _, Some(v))             => tyOf(v)
      case Ref(id, level, _) if level == 0 => Some(id.ty)
      case v: Var if v.level > 0           => tyOf(items(v.id.id))
      case v: Var                          => Some(v.id.ty)
      case v: Param                        => tyOf(v.of)
      case TodoLit                         => Some(BottomTy)
      case reg: Region if reg.semi         => Some(UnitTy)
      case reg: Region                     => reg.stmts.lastOption.flatMap(tyOf)
      case TypeMatch(_, _, cases, d) => {
        val types = (cases.map(_._2) :+ d).map(tyOf).flatten
        debugln(s"coerce enumMatch $types")
        types.lastOption
      }
      case ValueMatch(_, _, cases, d) => {
        val types = (cases.map(_._2).map(tyOf) :+ d.flatMap(tyOf)).flatten
        debugln(s"coerce valueMatch $types")
        types.lastOption
      }
      case _ =>
        throw new Exception(
          s"program is not well typed, because of $lhs (${lhs.getClass().getName()}).",
        )
    }
  }

  def lift(item: Item): Type = {
    debugln(s"lift $item")
    item match {
      case item: CIdent => CIdent(item.name, item.ns, 1)
      case item: CppInsType =>
        CppInsType(
          lift(item.target).asInstanceOf[CIdent],
          item.arguments.map(lift),
        )
      case SelfVal => SelfTy
      case _       => item
    }
  }

  def coerce(lhs: Option[Type], rhs: Option[Type]): Option[Type] = {
    // todo: corece correctly
    lhs.orElse(rhs)
  }
}
