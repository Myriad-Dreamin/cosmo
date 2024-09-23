package cosmo

import ir._
import syntax as s

trait TypeEnv { self: Env =>
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

  def defByName(info: DefInfo): String = info.defName(stem = false)

  def varByRef(vv: Term): String = {
    val ir.Term(id, level, v) = vv
    v.map {
      case v: CppInsType => Some(storeTy(v))
      case v: CIdent     => Some(v.repr)
      case _             => None
    }.flatten
      .getOrElse(defByName(id))
  }

  def storeTy(ty: Type): String = {
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

  def checkedMut(item: Item, isMut: Boolean): Unit = {
    debugln(s"checkedMut $item $isMut")
    val lhsIsMut = item match {
      case RefItem(lhs, rhsIsMut) => return checkedMut(lhs, rhsIsMut)
      case SelfTy                 => return
      case v: Term                => v.id.isMut
      case v: Var                 => v.id.isMut
      case _                      => return
    }
    if (!lhsIsMut && isMut) {
      errors = s"Cannot cast to mut ref" :: errors
    }
  }

  // : Normalization Part

  def eval(item: Item)(implicit level: Int): Item = {
    debugln(s"eval $item $level")
    val e = eval;
    item match {
      case CppInsType(target, arguments) => CppInsType(target, arguments.map(e))
      case Term(id, lvl, value) if level <= lvl => items(id.id)
      case _                                    => item
    }
  }

  def normalize(body: Item): Item = {
    debugln(s"normalize $body")
    body match {
      case _ => body
    }
  }

  // : Type Checker Part

  def isDependent(body: Item): Boolean = {
    body match {
      case _: (CIdent | CppInsType) =>
        false
      case _ => true
    }
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

  enum DestructShape {
    case Ty(ty: Type);
  }

  def checkedDestructed(shape: DestructShape) = {
    shape match {
      case DestructShape.Ty(ty) =>
        ty match
          case BottomKind(_) =>
          case ty =>
            err(s"required destructed type, but got $ty")
    }
  }

  def curryView(ty: Type): DestructShape = {
    // DestructShape.Ty(ty)
    ???
  }

  def enumShape(ty: Item): Option[Class] = {
    ty match {
      case v: Term if v.value.isEmpty =>
        enumShape(items.getOrElse(v.id.id, NoneItem))
      case Term(_, _, Some(v)) => enumShape(v)
      case BoundField(_, by, _, EnumField(v)) =>
        Some(v.copy(variantOf = Some(by)))
      case ClassInstance(con, _)             => enumShape(con)
      case v: Class if v.variantOf.isDefined => Some(v)
      case ty                                => None
    }
  }

  def tyOf(lhs: Item): Option[Type] = {
    debugln(s"tyOf $lhs")
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
      case _: (CModule | NativeModule)      => Some(UniverseTy)
      case RefItem(lhs, isMut)              => tyOf(lhs).map(RefItem(_, isMut))
      case v: ClassInstance                 => Some(v.con)
      case BoundField(_, _, _, VarField(v)) => Some(v.id.ty)
      case b: BinOp                         => coerce(tyOf(b.lhs), tyOf(b.rhs))
      case If(_, x, y)                      => coerce(tyOf(x), y.flatMap(tyOf))
      case SelfVal                          => Some(SelfTy)
      case Term(id, _, Some(v))             => tyOf(v)
      case Term(id, level, _) if level == 0 => Some(id.ty)
      case v: Var =>
        debugln(s"tyOf(Var) ${v.id.ty}")
        Some(v.id.ty)
      case TodoLit                 => Some(BottomTy)
      case reg: Region if reg.semi => Some(UnitTy)
      case reg: Region             => reg.stmts.lastOption.flatMap(tyOf)
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
