package cosmo

import scala.annotation.tailrec
import scala.collection.immutable.LongMap

import ir._
import ir.typed.*
import syntax as s

trait TypeEnv { self: Env =>

  /// Creates Infer Variable
  def createInfer(info: DefInfo, lvl: Int) = InferVar(info, level = lvl)

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
    NoneKind | Var | Bool | BinOp | EnumDestruct | ClassDestruct

  final def matchPat(lhs: PatShape, rhs: PatShape): ExtractedPat = {
    import PatShape._;
    debugln(s"matchPat $lhs by $rhs")
    (lhs, rhs) match {
      case (lhs, Hole(id)) =>
        id.ty = lhs.getTy; // todo: id.ty assignment has side effect
        val t = Var(id, None, (id.ty.level - 1).max(0))
        items += (id.id -> t)
        t
      case (Hole(_), _) => err("cannot destruct hole")
      case (Atom(lhsTy, UniverseTy), Atom(rhsTy, UniverseTy)) =>
        val v =
          isSubtype(canonicalTy(tyTerm(lhsTy)), canonicalTy(tyTerm(rhsTy)));
        if !v then err(s"cannot match type $lhsTy by $rhsTy")
        return Bool(v)
      case (Cons(_, lCls, _), Atom(rhsTy, UniverseTy)) =>
        val v = isSubtype(lCls, canonicalTy(tyTerm(rhsTy)));
        if !v then err(s"cannot match class $lCls by $rhsTy")
        return Bool(v)
      case (_, Atom(rv, rhsTy)) =>
        return BinOp("==", patHolder, valTerm(rv))
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
        debugln(s"args level $lv, $rv")

        val args = rv match {
          case None     => classBinds(rCls)
          case Some(rv) => classArgs(rv, rCls)
        }
        val lhsParams = classParams(valTerm(lv.get));
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
            val lhsParams = classParams(lv.map(valTerm).getOrElse(lCls));
            val params = if (lhsParams.length < args.length) {
              lhsParams ::: classParams(rCls).drop(lhsParams.length)
            } else {
              lhsParams
            }

            val matched = matchSeq(params, args);
            ClassDestruct(patHolder, rCls, matched)
          }
          case None =>
            val v = isSubtype(lCls, rCls);
            if !v then err(s"cannot match class $lCls by $rCls")
            return Bool(v)
          case Some(rv) => BinOp("==", patHolder, valTerm(rv))
        }
    }
  }

  def curryTerm(exp: Expr): PatShape = curryTermView(valTerm(exp))
  def curryTermView(lhs: Term): PatShape = {
    val lhsTy = tyOf(lhs);
    debugln(s"curryTermView $lhs % $lhsTy")
    curryView(lhs, lhsTy)
  }

  def curryExpr(v: Expr): PatShape = {
    debugln(s"curryExpr $v")
    v match {
      case untyp.Apply(lhs, rhs) =>
        val lhsTy = canonicalTy(valTerm(lhs))

        debugln(s"curryApplyE $lhs by $rhs")

        lhsTy match {
          case norm: Class =>
            val adt = enumShape(norm)
            val isAdt = adt.isDefined
            val cls = adt.getOrElse(norm)
            val args = untyp.TupleLit(rhs.toArray)
            PatShape.Cons(Some(args), cls, isAdt)
          case _ =>
            curryTermView(valTerm(v))
        }
      case untyp.Hole(id) => PatShape.Hole(id)
      case v              => curryTermView(valTerm(v))
    }
  }

  def curryView(v: Term, ty: Type): PatShape = {
    debugln(s"curryView $v $ty")

    if v.level > 0 then {
      weakClassRepr(canonicalTy(v)) match {
        case Right(norm: Class) =>
          val adt = enumShape(norm)
          val isAdt = adt.isDefined
          val cls = adt.getOrElse(norm)
          return PatShape.Cons(None, cls, isAdt)
        case Left(_) =>
      }
    }

    weakClassRepr(canonicalTy(ty)) match {
      case Right(norm: Class) =>
        val adt = enumShape(norm)
        val isAdt = adt.isDefined
        val cls = adt.getOrElse(norm)
        PatShape.Cons(Some(v), cls, isAdt)
      case Left(_) => PatShape.Atom(v, ty)
    }
  }

  def matchSeq(lhs: List[Term], rhs: List[Item]): List[Term] = {
    debugln(s"matchSeq $lhs by $rhs")
    if (lhs.length != rhs.length) {
      err(s"matchSeq length mismatch $lhs by $rhs")
    }
    lhs.zip(rhs).map { case (l, r) => matchOne(l, r) }
  }

  def matchOne(lhs: Term, rhs: Item): Term = {
    debugln(s"matchOne $lhs by $rhs [${rhs.isInstanceOf[Expr]}]")
    val lhsView = curryTermView(lhs)
    val rhsView = rhs match
      case v: Expr => curryExpr(v)
      case v: Term => curryView(v, tyOf(v))
    matchPat(lhsView, rhsView)
  }

  def matchParams[T](
      eParams: Array[Param],
      eArgs: List[Item],
      f: (Param, Option[Item]) => Option[T],
  ): List[T] = {

    val named = eArgs.flatMap {
      // todo: a bit dirty
      case untyp.KeyedArg(untyp.ItemE(Str(s)), value) =>
        Some((s, valTerm(value)))
      case KeyedArg(Str(s), value) => Some((s, value))
      case v                       => None
    }.toMap
    var positions = eArgs.flatMap {
      case v: KeyedArg       => None
      case v: untyp.KeyedArg => None
      case v                 => Some(v)
    }.iterator
    val allowNamedAsPositional = named.isEmpty

    val res = eParams.iterator.flatMap { param =>
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

    if (eParams.count(_.of.name != "self") != res.length) {
      errors =
        s"Invalid number of params v.s. arguments (${eParams.length} v.s. ${eArgs.length}) ${eParams.toList} v.s. $eArgs as $res" :: errors
    }

    res
  }

  def classBinds(cls: Class): List[Term] = {
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
      v: Term,
      // cls: Class,
      // variant: Option[Class],
  ): List[Term] = {
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
          return classBinds(v.con).dropRight(v.con.varsParams.length) ::: v.args
        }
        err(s"cannot destruct case 4 $v by $cls")
        classBinds(cls).dropRight(cls.varsParams.length) ::: v.args
      case untyp.TupleLit(items) =>
        var reorded =
          matchParams(
            cls.varsParams,
            items.toList,
            (param, value) => value,
          )
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
      eArgs: List[Term],
  ): List[Term] = {
    debugln(s"castArgs ${eParams.toList} $eArgs")

    val resolved = matchParams(
      eParams,
      eArgs,
      (param, value) => {
        val res =
          if (param.of.name == "self") None
          else if (param.named) {
            Some(value.getOrElse {
              if (!param.of.id.inClass) {
                errors = s"Missing named argument ${param.of.name}" :: errors
                Opaque.expr(
                  s"\n#error \"Missing named argument ${param.of.name}\"\n",
                )
              } else {
                Opaque.expr(
                  "kDefault",
                ) // todo: way of representing field default
              }
            })
          } else {
            Some(value.getOrElse {
              errors = s"Missing positional argument ${param.of.name}" :: errors
              Opaque.expr(
                s"\n#error \"Missing positional argument ${param.of.name}\"\n",
              )
            })
          }
        val info = param.id
        val casted = res.map(i => castTo(valTerm(i), info.ty))
        // casted.map { v =>
        //   items += (info.id -> v)
        // }

        casted match {
          case Some(Hole(id)) => Some(createInfer(id, param.level))
          case casted         => casted
        }
      },
    );

    resolved
  }

  def castTo(item: Term, nty: Type): Term = {
    val ty = canonicalTy(nty)

    debugln(s"castTo $item to $nty ($ty)")
    ty match {
      case TopTy | UniverseTy => item
      case RefItem(rtyNf, rhsIsMut) =>
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
            val lty = canonicalTy(tyOf(item))
            val rty = canonicalTy(rtyNf)
            val lhsIsInferVar = lty match {
              case _: InferVar             => true
              case RefItem(_: InferVar, _) => true
              case _                       => false
            }
            val lIsR = isSubtype(lty, rty);
            val rIsL = isSubtype(rty, lty);
            debugln(s"castTo $item to $rty ($lty) $lIsR $rIsL $lhsIsInferVar")
            if (lIsR && rIsL) {
              debugln(s"$item is exact $rty")
              return item
            }
            if (!lIsR && !lhsIsInferVar) {
              errors = s"No way to cast $item ($lty) to $nty ($rty)" :: errors
              return Opaque.expr(
                s"\n#error \"No way to cast\"\n /* ref $item */",
              )
            }

            rty match {
              case tr: Class if tr.id.isTrait => {
                As(item, implClass(lty, tr).get)
              }
              case _ =>
                As(item, nty)
            }
          }
          case l: Str if isSubtype(rtyNf, StrTy) => RefItem(l, false)
          case l if !rhsIsMut => castTo(RefItem(l, false), nty)
          case _ =>
            errors = s"Must ref item $item" :: errors;
            Opaque.expr(s"\n#error \"Must ref item\"\n /* ref $item */")
        }
      case _ => return item
    }
  }
  def hktRef(
      f: Option[Fn],
      args: List[Term],
      value: Term,
  ): Term = {
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

  def hktTranspose(syntax: Term, res: Term): Term = {
    debugln(s"hktTranspose $syntax $res")
    res match {
      case ClassInstance(con, args) =>
        val hktCon = con.copy(resolvedAs = Some(syntax))
        ClassInstance(hktCon, args)
      case _ if res.level == 0 => res
      case _                   => ???
    }
  }

  // : Normalization Part

  def normalize(body: Term): Term = {
    debugln(s"normalize $body")
    body match {
      case _ => body
    }
  }

  // : Type Checker Part

  def checkedMut(item: Term, isMut: Boolean): Unit = {
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

  def isDependent(body: Term): Boolean = {
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

  def eqType(lhs: Term, rhs: Term): Boolean = {
    val lty = canonicalTy(lhs)
    val rty = canonicalTy(rhs)
    isSubtype(lty, rty) && isSubtype(rty, lty)
  }

  def canonicalTy(rhs: Term): Term = {
    debugln(s"canonicalTy $rhs")
    rhs match {
      case SelfVal if selfImplRef.isDefined =>
        selfRef.map(canonicalTy).getOrElse(TopTy)
      case SelfVal                              => SelfTy
      case Ref(_, level, Some(v)) if level == 1 => canonicalTy(v)
      // case v: Ref                               => canonicalTy(v.id.ty)
      // case v: Var                               => canonicalTy(v.id.ty)
      case BoundField(_, by, _, EnumField(v)) => v.copy(variantOf = Some(by))
      case RefItem(lhs, isMut) => RefItem(canonicalTy(lhs), isMut)
      // todo: canonical hkt types
      case _ => rhs
    }
  }

  @tailrec
  final def dequalifyTy(rhs: Term): Term = {
    debugln(s"dequalifyTy $rhs")
    rhs match {
      case Ref(_, _, Some(v))      => dequalifyTy(v)
      case RefItem(lhs, isMut)     => dequalifyTy(lhs)
      case p: Param                => dequalifyTy(p.of)
      case HKTInstance(ty, syntax) => dequalifyTy(ty)
      case _                       => rhs
    }
  }

  def isSubtype(lhs: Term, rhs: Term): Boolean = {
    isSubtype_(dequalifyTy(lhs), dequalifyTy(rhs))
  }

  @tailrec
  final def isSubtype_(lhs: Term, rhs: Term): Boolean = {
    debugln(s"isSubtype $lhs $rhs")
    rhs match {
      case SelfTy => isSubtype_(lhs, selfRef.getOrElse(BottomTy))
      // todo: same level
      case cls: Class         => implClass(lhs, cls).isDefined
      case TopTy | UniverseTy => true
      case BottomTy           => false
      case IntegerTy(_, _) | StrTy | BoolTy if isBuiltin(lhs, rhs) => true
      case v: Var if v.level == 1 =>
        lhs match {
          case SelfTy        => isSubtype_(selfRef.getOrElse(TopTy), rhs)
          case Var(id, _, _) => id.id == v.id.id
          case BottomTy      => true
          case _             => false
        }
      case _ => {
        lhs match {
          case SelfTy         => isSubtype_(selfRef.getOrElse(TopTy), rhs)
          case BottomTy       => true
          case StrTy | BoolTy => isBuiltin(lhs, rhs)
          case _              => false
        }
      }
    }
  }

  def isBuiltin(lhs: Term, rhs: Term): Boolean = {
    debugln(s"isBuiltin $lhs $rhs")
    lhs match {
      case Ref(_, _, Some(v)) => isBuiltin(v, rhs)
      case TopTy | UniverseTy => true
      case BottomTy           => true
      case Int64(_)           => lhs == rhs || rhs == IntegerTy(64, false)
      case Float32(_)         => lhs == rhs || rhs == FloatTy(32)
      case Float64(_)         => lhs == rhs || rhs == FloatTy(64)
      case Bool(_)            => lhs == rhs || rhs == BoolTy
      case Str(_)             => lhs == rhs || rhs == StrTy
      case _                  => lhs == rhs
    }
  }

  final def weakClassRepr(lhs: Type): Either[Type, Class] = {
    classRepr(lhs) match {
      case Some(cls) =>
        if (cls.id.isBuiltin) {
          Left(cls.resolvedAs.get)
        } else {
          Right(cls)
        }
      case None => Left(lhs)
    }
  }

  @tailrec
  final def classRepr(lhs: Type): Option[Class] = {
    Some(lhs match {
      case ClassInstance(con, _)    => con
      case v: Class                 => v
      case _: (CIdent | CppInsType) => return None
      case Ref(_, _, Some(v))       => return classRepr(v)
      case Ref(_, _, None)          => return None
      case RefItem(lhs, isMut)      => return classRepr(lhs)
      case Var(id, _, _)            => return classRepr(id.ty)
      case Param(of, _)             => return classRepr(of)
      case SelfTy              => return classRepr(selfRef.getOrElse(NoneItem))
      case _ if lhs.isBuilitin => builtinClasses(lhs)
      case l @ (Bool(_))       => builtinClasses(l.ty)
      case l @ (Str(_))        => builtinClasses(l.ty)
      case l @ (Int64(_))      => builtinClasses(l.ty)
      case l @ (Float32(_))    => builtinClasses(l.ty)
      case l @ (Float64(_))    => builtinClasses(l.ty)
      case Unresolved(_) | NoneKind(_) => return None
      // todo: handle infer var
      case _: InferVar => return None
      case _           => throw new Exception(s"cannot get class $lhs")
    })
  }

  def implClass(lhs: Term, goal: Class): Option[Term] = {
    val cls = classRepr(lhs) match {
      case Some(cls) => cls
      case None      => return None
    }
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
  final def enumShape(ty: Term): Option[Class] = {
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

  def tyOf(lhs: Term): Type = {
    debugln(s"tyOf $lhs (level ${lhs.level})")
    lhs match {
      case l: Int64                                   => l.ty
      case l: Float32                                 => l.ty
      case l: Float64                                 => l.ty
      case l: Bool                                    => l.ty
      case _: Rune                                    => IntegerTy(32, false)
      case _: Str                                     => StrTy
      case NoneItem                                   => TopTy
      case _: (Apply | Opaque | Select | Unresolved)  => TopTy
      case _: (While | Loop | For | Break | Continue) => UnitTy
      case Unreachable                                => BottomTy
      case _: (CIdent | TopKind | NoneKind | Class | CppInsType) => UniverseTy
      case v if v.level == 1                                     => UniverseTy
      case _: (CModule | NativeModule)                           => UniverseTy
      case BoundField(_, _, _, EnumField(v))                     => UniverseTy
      case RefItem(lhs, isMut)                 => RefItem(tyOf(lhs), isMut)
      case v: ClassInstance                    => v.con
      case BoundField(_, _, _, VarField(v, _)) => v.id.ty
      case b: BinOp            => coerce(tyOf(b.lhs), tyOf(b.rhs))
      case b: BinInst          => b.op.ty
      case If(_, x, y)         => coerce(tyOf(x), y.map(tyOf).getOrElse(UnitTy))
      case Return(value)       => tyOf(value)
      case SelfVal             => SelfTy
      case Ref(id, _, Some(v)) => tyOf(v)
      case Ref(id, level, _) if level == 0 => id.ty
      case v: Var if v.level > 0           => tyOf(items(v.id.id))
      case v: Var                          => v.id.ty
      case v: Param                        => tyOf(v.of)
      case TodoLit                         => BottomTy
      case reg: Region if reg.semi         => UnitTy
      case reg: Region => reg.stmts.lastOption.map(tyOf).getOrElse(UnitTy)
      case TypeMatch(_, _, cases, d) => {
        val types = (cases.map(_._2) :+ d).map(tyOf)
        debugln(s"coerce enumMatch $types")
        types.lastOption.getOrElse(UnitTy)
      }
      case ValueMatch(_, _, cases, d) => {
        val types = (cases.map(_._2).map(tyOf) :+ tyOf(d))
        debugln(s"coerce valueMatch $types")
        types.lastOption.getOrElse(UnitTy)
      }
      case _ =>
        throw new Exception(
          s"program is not well typed, because of $lhs (${lhs.getClass().getName()}).",
        )
    }
  }

  def lift(item: Term): Type = {
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

  def coerce(lhs: Type, rhs: Type): Type = {
    // todo: corece correctly
    // lhs.orElse(rhs)
    lhs
  }
}
