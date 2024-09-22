package cosmo.ir

import scala.collection.mutable.{Map as MutMap};

import cosmo.{DefInfo, FileId}

import cosmo.ir.Value
import cosmo.{DefId, DefInfo, Doc, Env}
import cosmo.syntax.Ident
import cosmo.service.LangObject

val DEF_ALLOC_START = 16
val CLASS_EMPTY = 0
val CODE_FUNC = 1

private[cosmo] type Defo = DefInfo;
private[cosmo] type Ni = Option[Item];
private[cosmo] type FieldMap = MutMap[String, VField];

sealed abstract class Item {
  val level: Int = 0;

  def instantiate(implicit lvl: Int): Item = {
    // this match {
    //   case Term(info, lvl, v) =>
    //     val ty = info.upperBounds.find {
    //       case Term(_, info, lvl, v) => false
    //       case _                         => true
    //     }
    //     val ty2 = ty.map { ty =>
    //       if ty.level > lvl then ty.instantiate
    //       else ty
    //     }
    //     ty2.getOrElse(TopKind(lvl))
    //   case TopKind(level)    => TopKind((level - 1).min(lvl))
    //   case BottomKind(level) => BottomKind((level - 1).min(lvl))
    //   case SelfKind(level)   => SelfKind((level - 1).min(lvl))
    //   case ty                => ty
    // }
    ???
  }
  val isBuilitin: Boolean = false

  def langObj: LangObject = LangObject(this)
  def e: Expr = ItemE(this)
  def toDoc: Doc = Doc.buildItem(this)
}
// todo: don't inherit item?
sealed abstract class Expr extends Item {
  def isWildcard: Boolean = this match {
    case d: DeclExpr if d.id.name == "_" => true; case _ => false
  }
}
sealed abstract class DeclExpr extends Expr with DeclLike {
  val id: DefInfo
}

/// Expressions

final case class ItemE(item: Item) extends Expr {}
final case class Opaque(expr: Option[String], stmt: Option[String])
    extends Expr {}
object Opaque {
  val empty = Opaque(None, None)
  def expr(expr: String) = Opaque(Some(expr), None)
  def stmt(stmt: String) = Opaque(None, Some(stmt))
}
final case class Region(stmts: List[Item]) extends Expr {
  override def toString: String = stmts.mkString("Region{ ", "; ", " }")
}
final case class Loop(body: Item) extends Expr {}
final case class While(cond: Item, body: Item) extends Expr {}
final case class For(name: Item, iter: Item, body: Item) extends Expr {}
final case class Break() extends Expr {}
final case class Continue() extends Expr {}
final case class Return(value: Item) extends Expr {}
final case class If(cond: Item, cont_bb: Item, else_bb: Option[Item])
    extends Expr {}
final case class As(lhs: Item, rhs: Item) extends Expr {}
final case class UnOp(op: String, lhs: Item) extends Expr {}
final case class BinOp(op: String, lhs: Item, rhs: Item) extends Expr {}
final case class KeyedArg(key: Item, value: Item) extends Expr {}
final case class Apply(lhs: Item, rhs: List[Item]) extends Expr {
  override def toString: String = s"$lhs(${rhs.mkString(", ")})"
}
final case class SelectExpr(lhs: Expr, rhs: String) extends Expr {}
final case class Name(val id: DefInfo, val of: Option[Expr] = None)
    extends DeclExpr {
  override def toString: String = s"${id.defName(false)}@${id.id.id}"
}
final case class Hole(id: DefInfo) extends DeclExpr {
  override def toString: String = s"hole(${id.defName(false)})"
}
final case class VarExpr(id: DefInfo, ty: Option[Type], init: Option[Expr])
    extends DeclExpr {
  override def toString: String = s"var(${id.defName(false)})"
}
sealed abstract class ParamExpr extends DeclExpr {
  val id: DefInfo
  val params: Option[List[VarExpr]]
  val constraints: List[Expr]
}
final case class DefExpr(
    id: DefInfo,
    params: Option[List[VarExpr]],
    constraints: List[Expr],
    ret_ty: Option[Type],
    body: Option[Item],
) extends ParamExpr {
  override def toString: String = s"fn(${id.defName(false)})"
}
final case class ClassExpr(
    id: DefInfo,
    params: Option[List[VarExpr]],
    constraints: List[Expr],
    fields: FieldMap,
) extends ParamExpr {}
final case class ImplExpr(
    id: DefInfo,
    params: Option[List[VarExpr]],
    constraints: List[Expr],
    iface: Option[Type],
    cls: Type,
    fields: FieldMap,
) extends ParamExpr {}
final case class CaseRegion(cases: List[(Expr, Option[Expr])]) extends Expr {}
final case class MatchExpr(lhs: Expr, body: CaseRegion) extends Expr {}

/// Types

type Type = Item
case class TopKind(
    override val level: Int,
) extends Item {
  override val isBuilitin: Boolean = true
}
case class BottomKind(
    override val level: Int,
) extends Item {
  override val isBuilitin: Boolean = true
}
case class SelfKind(
    override val level: Int,
) extends Item {
  override val isBuilitin: Boolean = true
}
case class NoneKind(
    override val level: Int,
) extends Item {
  override val isBuilitin: Boolean = true
}
case object Unreachable extends Item
final case class Unresolved(id: DefInfo) extends Item {}

val NoneItem = NoneKind(0)

// final case class Apply(lhs: Item, rhs: List[Item]) extends Item {
//   override def toString: String = s"$lhs(${rhs.mkString(", ")})"
// }
final case class BoundField(lhs: Item, by: Type, casted: Boolean, rhs: VField)
    extends Item {
  override def toString: String =
    if casted then s"($lhs as $by).$rhs" else s"$lhs.$rhs"
}
final case class RefItem(lhs: Item, isMut: Boolean) extends Item {}
final case class Select(lhs: Item, rhs: String) extends Item {
  override def toString: String = s"$lhs.$rhs"
}
final case class ValueMatch(
    lhs: Item,
    by: Type,
    cases: List[(Item, Item)],
    orElse: Option[Item],
) extends Item {}
final case class TypeMatch(
    lhs: Item,
    by: Type,
    cases: List[(Class, Item)],
    orElse: Item,
) extends Item {}
sealed abstract trait DeclLike {
  val id: DefInfo
  def name = id.name
}
abstract class DeclItem extends Item with DeclLike {}
case class DeclRef(item: DeclItem) extends DeclItem {
  val id = item.id
  override def toString: String = s"decl(${id.defName(false)})"
}

final case class Param(id: DefInfo, override val level: Int) extends DeclItem {
  def pretty(implicit rec: Item => String = _.toString): String =
    s"${id.defName(false)}: ${rec(id.ty)}"
}
final case class Var(
    id: DefInfo,
    init: Option[Item],
    override val level: Int,
) extends DeclItem {
  override def toString: String =
    val mod = if !id.isMut then "val" else "var"
    s"($mod ${id.defName(false)}:${id.id.id} = ${init.getOrElse(NoneItem)})"

  def pretty(implicit rec: Item => String = _.toString): String =
    val mod = if !id.isMut then "val" else "var"
    val initStr = init.map(rec).getOrElse("None")
    s"$mod ${id.defName(false)}"
}
final case class Fn(
    id: DefInfo,
    sig: Sig,
    override val level: Int,
) extends DeclItem {
  override def toString: String = s"fn(${id.defName(false)})"

  def pretty(implicit rec: Item => String = _.toString): String =
    val params = sig.params.map(_.map(rec).mkString(", ")).getOrElse("")
    val ret = rec(sig.ret_ty)
    s"def ${id.defName(false)}($params): $ret"
}
final case class Sig(
    params: Option[List[Param]],
    ret_ty: Type,
    body: Option[Item],
) extends Item {
  def resolveLevel = (ret_ty.level - 1).max(0)
  def selfParam: Option[Param] =
    params.iterator.flatten.find(_.id.name == "self")
  def selfIsMut: Option[Boolean] = selfParam.map(_.id.ty).map {
    case RefItem(lhs, isMut) => isMut
    case _                   => false
  }
}

final case class Term(
    val id: DefInfo,
    override val level: Int,
    val value: Option[Item] = None,
) extends DeclItem {
  override def toString: String = s"term(${id.defName(false)})"
}
final case class CModule(id: DefInfo, kind: CModuleKind, path: String)
    extends DeclItem {
  def pretty(implicit rec: Item => String = _.toString): String =
    s"module ${id.defName(false)} including \"$path\""
}
enum CModuleKind {
  case Builtin, Error, Source
}
final case class NativeModule(id: DefInfo, env: Env) extends DeclItem {
  def pretty(implicit rec: Item => String = _.toString): String =
    s"module ${id.defName(false)} in ${env.fid.map(_.toString).getOrElse("")}"
}
final case class HKTInstance(ty: Type, syntax: Item) extends Item {
  override val level: Int = 1
  override def toString(): String = s"(hkt($syntax)::type as $ty)"
  def repr(rec: Type => String): String = syntax match {
    case t: (Term | Fn) => rec(t)
    case Apply(lhs, rhs) =>
      s"${rec(HKTInstance(ty, lhs))}<${rhs.map(rec).mkString(", ")}>::type"
    case Select(lhs, rhs) => s"${rec(HKTInstance(ty, lhs))}::$rhs"
    case _                => syntax.toString
  }
}
final case class ClassInstance(
    con: Class,
    args: List[Item],
) extends Item {
  override def toString: String =
    val conAs =
      if con.resolvedAs.isDefined then s"${con.resolvedAs.get} as " else ""
    s"(${conAs}${con})(${args.mkString(", ")})"
}
final case class EnumVariant(
    id: DefInfo,
    base: Class,
) extends DeclItem {
  override val level: Int = 1
}
final case class EnumDestruct(
    item: Item,
    variant: Class,
    bindings: List[Term | EnumDestruct],
) extends Item {}
final case class Class(
    id: DefInfo,
    params: Option[List[Param]],
    fields: FieldMap,
    args: Option[List[Item]] = None,
    variantOf: Option[Type] = None,
    resolvedAs: Option[Type] = None,
) extends DeclItem {
  override val level: Int = 1
  override def toString: String = s"class(${repr()})"
  def isPhantomClass: Boolean = id.isPhantom
  def justInit: Boolean = !id.isTrait && params.isEmpty && isPhantomClass

  def repr(implicit rec: Item => String = _.toString): String =
    val argList = args.map(_.map(rec).mkString("<", ", ", ">")).getOrElse("")
    id.defName(false) + argList

  def pretty(implicit rec: Item => String = _.toString): String = ???
}
object Class {
  def empty(env: Env, isAbstract: Boolean) =
    val id = DefInfo.just(CLASS_EMPTY, env)
    Class(id, None, MutMap())
}
final case class Impl(
    id: DefInfo,
    params: Option[List[Param]],
    iface: Type,
    cls: Type,
    fields: FieldMap,
) extends DeclItem {
  override val level: Int = 1

  def pretty(implicit rec: Item => String = _.toString): String = ???
}

// TopTy
val TopTy = TopKind(1)
val BottomTy = BottomKind(1)
val SelfTy = SelfKind(1)
val SelfVal = SelfKind(0)
val UniverseTy = TopKind(2)
case object CEnumTy extends Type {
  override val level = 1
  override val isBuilitin: Boolean = true;
}
case object BoolTy extends Type {
  override val level = 1
  override val isBuilitin: Boolean = true;
}
case object StrTy extends Type {
  override val level = 1
  override val isBuilitin: Boolean = true;
  override def toString: String = "str"
}
case object UnitTy extends Type {
  override val level = 1
  override val isBuilitin: Boolean = true;
}
final case class RefTy(val isRef: Boolean, val isMut: Boolean) extends Type {
  override val level = 1
}
final case class IntegerTy(val width: Int, val isUnsigned: Boolean)
    extends Type {
  override val level = 1
  override val isBuilitin: Boolean = true;
  override def toString: String = s"${if (isUnsigned) "u" else "i"}$width"
}
object IntegerTy {
  def parse(s: String): Option[IntegerTy] = {
    val unsigned = s.startsWith("u")
    val width = s.stripPrefix("u").stripPrefix("i").toInt
    Some(new IntegerTy(width, unsigned))
  }
}
final case class FloatTy(val width: Int) extends Type {
  override val level = 1
  override def toString: String = s"f$width"
  override val isBuilitin: Boolean = true
}
object FloatTy {
  def parse(s: String): Option[FloatTy] = {
    Some(new FloatTy(s.stripPrefix("f").toInt))
  }
}
final case class InferVar(
    var info: DefInfo,
    var upperBounds: List[Type] = List(),
    var lowerBounds: List[Type] = List(),
    override val level: Int,
) extends Type {
  override def toString: String = s"infer(${info.name}:${info.id.id})"
}
final case class ValueTy(val value: Value) extends Type {
  override val level = 1
  override def toString: String = value.toString
}
final case class CIdent(
    val name: String,
    val ns: List[String],
    override val level: Int,
) extends Item {
  override def toString: String = s"cpp($repr)"
  def repr: String = (ns :+ name).mkString("::")
}
final case class CppInsType(val target: CIdent, val arguments: List[Type])
    extends Type {
  override val level = target.level
  override def toString: String = s"cpp(${repr(_.toString)})"
  def repr(rec: Type => String): String =
    target.repr + "<" + arguments
      .map {
        case Term(defId, level, None) => defId.name
        case Term(id, level, Some(v)) => rec(v)
        case ty                       => rec(ty)
      }
      .mkString(", ") + ">"
}

sealed abstract class Value extends Item
case object TodoLit extends Value {}
final case class Bool(value: Boolean) extends Value {
  val ty = BoolTy
}
final case class Integer(value: Int) extends Value {
  val ty = IntegerTy(32, false)
}
final case class Str(value: String) extends Value {
  val ty = StrTy
}
final case class Bytes(value: Array[Byte]) extends Value {}
final case class Rune(value: Int) extends Value {}
final case class DictLit(value: Map[String, Item]) extends Value {}
final case class TupleLit(elems: Array[Item]) extends Value {
  override def toString: String = elems.mkString("tup(", ", ", ")")
}

sealed abstract class VField {
  val item: DeclLike
  def name = item.name

  def pretty(implicit rec: Item => String = _.toString): String = ???
}
final case class EVarField(item: VarExpr, index: Int) extends VField
final case class EDefField(item: DefExpr) extends VField
final case class EEnumField(item: ClassExpr, index: Int) extends VField
final case class VarField(item: Var) extends VField
final case class DefField(item: Fn) extends VField
final case class EnumField(item: Class) extends VField
