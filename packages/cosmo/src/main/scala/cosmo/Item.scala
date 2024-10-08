package cosmo.ir

import scala.compiletime.uninitialized
import scala.collection.mutable.{LinkedHashMap as MutMap};
import scala.annotation.tailrec

import cosmo.{DefInfo, ExprInfo, FileId}

import cosmo.ir.Value
import cosmo.{DefId, DefInfo, Doc, Env}
import cosmo.syntax.Ident
import cosmo.service.LangObject
import cosmo.{debugln, logln}
import cosmo.ir.typed.Region

val DEF_ALLOC_START = 16
val CLASS_EMPTY = 0
val CODE_FUNC = 1

private[cosmo] type Defo = DefInfo;
private[cosmo] type Ni = Option[Term | Expr];
private[cosmo] type FieldMap = MutMap[String, VField];

/// Relationship
///
/// Expr, Term, Type, Value are Item
/// Item is used for Semantic Analysis
/// Value and (Concrete) Expr has Type
/// Term is Type with operations
/// Op is not Item, Op is (optionally) converted from Expr, thus has type
///
/// Source Code -> Expr -> Instrs with Types -> Output

abstract trait DeclLike {
  val id: DefInfo
  def name = id.name
}
sealed trait ObjectLike(
    val fields: FieldMap,
) extends DeclTerm {
  @inline
  def vars =
    fields.values.filter(_.isInstanceOf[VarField]).asInstanceOf[List[VarField]]
  def defs =
    fields.values.filter(_.isInstanceOf[DefField]).asInstanceOf[List[DefField]]
  def variants =
    fields.values
      .filter(_.isInstanceOf[EnumField])
      .asInstanceOf[List[EnumField]]
}
sealed trait FuncLike(
    val synParams: Option[List[VarExpr]],
    val constraints: List[Expr],
) extends DeclTerm {
  var rawParams: Option[List[Param]] = uninitialized
  lazy val params: Array[Param] = rawParams.map(_.toArray).getOrElse(Array())
  lazy val callByName: Boolean = rawParams.isEmpty;
  def selfParam: Option[Param] = params.find(_.id.name == "self")
  def selfIsMut: Option[Boolean] = selfParam.map(_.id.ty).map {
    case RefItem(lhs, isMut) => isMut
    case _                   => false
  }
  def doCheckParams(p: Option[List[Param]]) = rawParams = p
}

sealed abstract trait CallableTerm(raw: Option[List[Param]]) {}

/// Expressions & Terms

trait ItemExt {
  def toDoc: Doc
}

sealed abstract class Expr extends ItemExt {
  val info: ExprInfo = ExprInfo.empty;
  def toDoc: Doc = Doc.buildItem(this)
}
sealed abstract class Term extends ItemExt {
  def sol: Term = this
  def ty: Type
  @tailrec
  final def isTypeLevel: Boolean =
    debugln(s"isTypeLevel $this")
    this match {
      case ExprTy     => false
      case UniverseTy => true
      // todo: process is type var
      case v: VarExpr => v.id.isTypeVar || v.ty.isTypeLevel
      case f: DefExpr => f.retTy == UniverseTy
      case _          => ty == UniverseTy
    }
  def isValLevel: Boolean = !isTypeLevel

  val isBuilitin: Boolean = false

  def e: Expr = untyp.ItemE(this)
  def langObj: LangObject = LangObject(this)
  def toDoc: Doc = Doc.buildItem(this)
  def eval(env: Env): Term = this
}
sealed abstract class ExprTerm extends Term {
  var info: ExprInfo = uninitialized
  override lazy val ty: Type = info.sol match {
    case Region(stmts, true)                   => UnitTy
    case Region(stmts, false) if stmts.isEmpty => UnitTy
    case Region(stmts, false)                  => stmts.last.ty
    case v: ExprTerm                           => TopTy
    case v                                     => v
  }
  override def eval(env: Env): Term = env.compile(info)(env)
}

sealed case class DeclExpr(term: DeclTerm) extends Expr {}
sealed abstract class DeclTerm extends DeclItem with DeclLike {
  type ThisTerm <: Term

  def checkDecl: ThisTerm;
  protected var checkCache: ThisTerm = uninitialized
  def checked: ThisTerm =
    if checkCache eq null then checkCache = id.env.checkDecl(this, checkDecl)
    checkCache
}
final case class Name(val id: DefInfo, val of: Option[DefInfo] = None)
    extends DeclTerm {
  type ThisTerm <: Term

  override def toString: String = s"${id.defName(false)}@${id.id.id}"
  override def checkDecl: ThisTerm = id.env.valTerm(this).asInstanceOf[ThisTerm]
}
final case class Hole(id: DefInfo) extends DeclTerm {
  type ThisTerm <: Term

  override def toString: String = s"hole(${id.defName(false)})"
  override def checkDecl: ThisTerm = id.env.valTerm(this).asInstanceOf[ThisTerm]
}
final case class VarExpr(id: DefInfo, annoTy: Option[Expr], initE: Option[Expr])
    extends DeclTerm {
  type ThisTerm <: VarExpr
  // override def toString: String =
  //   val mod = if !id.isMut then "val" else "var"
  //   s"($mod ${id.defName(false)}:${id.id.id} = ${init.getOrElse(NoneItem)})"
  override def toString: String = s"var(${id.defName(false)})"
  override def checkDecl: ThisTerm =
    id.env.checkVar(this).asInstanceOf[ThisTerm]
  var init: Option[Term] = uninitialized
  def doCheck(ty: Type, i: Option[Term]) =
    id.ty = ty
    init = i
    i.foreach { i =>
      if isTypeLevel then id.env.items += id.id -> i
    }
    // if isTypeLevel then id.env.items += id.id -> i.getOrElse(this)
    // else id.env.items += id.id -> this
    checkCache = this.asInstanceOf[ThisTerm]
}
object VarExpr {
  def generate(id: DefInfo, ty: Type, init: Option[Term]) =
    val res = VarExpr(id, None, None)
    res.doCheck(ty, init)
    res
}

final case class DefExpr(
    id: DefInfo,
    retTyExp: Option[Expr],
    bodyExp: Option[Expr],
)(
    params: Option[List[VarExpr]],
    constraints: List[Expr],
) extends FuncLike(params, constraints) {
  type ThisTerm <: DefExpr
  var retTy: Type = uninitialized
  var body: Option[Term] = uninitialized

  override def toString: String = s"fn(${id.defName(false)})"
  def checkDecl = id.env.checkDef(this).asInstanceOf[ThisTerm]
  def doCheckRetTy(ty: Type) =
    retTy = ty
  def doCheckBody(ty: Type, b: Option[Term]) =
    retTy = ty
    body = b
    checkCache = this.asInstanceOf[ThisTerm]
}
final case class ClassExpr(
    id: DefInfo,
)(
    params: Option[List[VarExpr]],
    constraints: List[Expr],
)(fields: FieldMap)
    extends FuncLike(params, constraints)
    with ObjectLike(fields) {
  type ThisTerm <: Class

  def checkDecl = id.env.checkClass(this).asInstanceOf[ThisTerm]
}
final case class Impl(
    id: DefInfo,
    ifaceExpr: Option[Expr],
    clsExpr: Expr,
)(
    params: Option[List[VarExpr]],
    constraints: List[Expr],
)(fields: FieldMap)
    extends FuncLike(params, constraints)
    with ObjectLike(fields) {
  type ThisTerm <: Impl
  var iface: Type = TopTy
  var cls: Type = TopTy

  override def checkDecl: ThisTerm =
    id.env.checkImpl(this).asInstanceOf[ThisTerm]
}

enum BinInstIntOp {
  case Add, Sub, Mul, Div, Rem, And, Or, Xor, Shl, Shr, Sar, Eq, Ne, Lt, Le,
    Gt,
    Ge;

  def repr: String = this match {
    case Add => "+"
    case Sub => "-"
    case Mul => "*"
    case Div => "/"
    case Rem => "%"
    case And => "&"
    case Or  => "|"
    case Xor => "^"
    case Shl => "<<"
    case Shr => ">>"
    case Sar => ">>>"
    case Eq  => "=="
    case Ne  => "!="
    case Lt  => "<"
    case Le  => "<="
    case Gt  => ">"
    case Ge  => ">="
  }
}
enum BinInstOp {
  case Int(t: IntegerTy, op: BinInstIntOp);

  def ty: Type = this match {
    case Int(t, op) => t
  }
}

// region: Exprs
object untyp {
  type T = Expr;
  type E = Expr;
  final case class ItemE(item: Term) extends Expr {}

  final case class Opaque(expr: Option[String], stmt: Option[String])
      extends E {}
  object Opaque {
    val empty = Opaque(None, None)
    def expr(expr: String) = Opaque(Some(expr), None)
    def stmt(stmt: String) = Opaque(None, Some(stmt))
  }
  final case class Region(stmts: List[T], semi: Boolean) extends E {
    override def toString: String = stmts.mkString("Region{ ", "; ", " }")
  }
  final case class Loop(body: T) extends E {}
  final case class While(cond: T, body: T) extends E {}
  final case class For(name: T, iter: T, body: T) extends E {}
  final case class Break() extends E {}
  final case class Continue() extends E {}
  final case class Return(value: T) extends E {}
  final case class If(cond: T, cont_bb: T, else_bb: Option[T]) extends E {}
  final case class As(lhs: T, rhs: T) extends E {}
  final case class UnOp(op: String, lhs: T) extends E {}
  final case class BinOp(op: String, lhs: T, rhs: T) extends E {}
  final case class BinInst(op: BinInstOp, lhs: T, rhs: T) extends E {}
  final case class KeyedArg(key: T, value: T) extends E {}
  final case class Apply(lhs: T, rhs: List[T]) extends E {
    override def toString: String = s"$lhs(${rhs.mkString(", ")})"
  }
  final case class TmplApply(
      lhs: T,
      strings: List[String],
      rhs: List[(T, Option[String])],
  ) extends E {
    override def toString: String =
      s"$lhs(${strings.mkString(", ")})(${rhs.mkString(", ")})"
  }
  final case class SelectExpr(lhs: T, rhs: String) extends E {
    override def toString: String = s"$lhs.$rhs"
  }
  final case class DestructExpr(dst: T, src: T) extends E {
    override def toString: String = s"$dst = $src"
  }
  final case class CaseExpr(cond: T, body: Option[T]) extends E {}
  final case class CaseRegion(cases: List[CaseExpr]) extends E {}
  final case class MatchExpr(lhs: T, body: CaseRegion) extends E {}
  final case class TupleLit(elems: Array[T]) extends E {
    override def toString: String = elems.mkString("tup(", ", ", ")")
  }

}
object typed {
  type T = Term;
  type E = ExprTerm;

  final case class Opaque(expr: Option[String], stmt: Option[String])
      extends E {}
  object Opaque {
    val empty = Opaque(None, None)
    def expr(expr: String) = Opaque(Some(expr), None)
    def stmt(stmt: String) = Opaque(None, Some(stmt))
  }
  final case class Region(stmts: List[T], semi: Boolean) extends E {
    override def toString: String = stmts.mkString("Region{ ", "; ", " }")
  }
  final case class Loop(body: T) extends E {}
  final case class While(cond: T, body: T) extends E {}
  final case class For(name: T, iter: T, body: T) extends E {}
  final case class Break() extends E {}
  final case class Continue() extends E {}
  final case class Return(value: T) extends E {}
  final case class If(cond: T, cont_bb: T, else_bb: Option[T]) extends E {}
  final case class As(lhs: T, rhs: T) extends E {}
  final case class UnOp(op: String, lhs: T) extends E {}
  final case class BinOp(op: String, lhs: T, rhs: T) extends E {}
  final case class BinInst(op: BinInstOp, lhs: T, rhs: T) extends E {}
  final case class KeyedArg(key: T, value: T) extends E {}
  final case class Apply(lhs: T, rhs: List[T]) extends E {
    override def toString: String = s"$lhs(${rhs.mkString(", ")})"
  }
  final case class TmplApply(
      lhs: T,
      strings: List[String],
      rhs: List[(T, Option[String])],
  ) extends E {
    override def toString: String =
      s"$lhs(${strings.mkString(", ")})(${rhs.mkString(", ")})"
  }
  final case class SelectExpr(lhs: T, rhs: String) extends E {
    override def toString: String = s"$lhs.$rhs"
  }
  final case class DestructExpr(dst: T, src: T) extends E {
    override def toString: String = s"$dst = $src"
  }
  final case class CaseExpr(cond: T, body: Option[T]) extends E {}
  final case class CaseRegion(cases: List[CaseExpr]) extends E {}
  final case class MatchExpr(lhs: T, body: CaseRegion) extends E {}
  final case class TupleLit(elems: Array[T]) extends E {
    override def toString: String = elems.mkString("tup(", ", ", ")")
  }

}
// endregion: Exprs

import typed.*;

/// Types

type Type = Term

sealed abstract class SimpleType extends Type {}

val Unreachable = BottomKind(0)
val NoneItem = NoneKind(0)

case class TopKind(val level: Int) extends SimpleType {
  override def ty = if level == 1 then UniverseTy else this.copy(level + 1)
  override val isBuilitin: Boolean = true
}
case class BottomKind(val level: Int) extends SimpleType {
  override def ty = if level == 1 then UniverseTy else this.copy(level + 1)
  override val isBuilitin: Boolean = true
}
case class SelfKind(val level: Int) extends SimpleType {
  override def ty = if level == 1 then UniverseTy else this.copy(level + 1)
  override val isBuilitin: Boolean = true
}
case class NoneKind(val level: Int) extends SimpleType {
  override def ty = if level == 1 then UniverseTy else this.copy(level + 1)
  override val isBuilitin: Boolean = true
}
final case class Unresolved(id: DefInfo) extends SimpleType {
  override def ty = id.ty
}

// TopTy
val TopTy = TopKind(1)
val BottomTy = BottomKind(1)
val SelfTy = SelfKind(1)
val SelfVal = SelfKind(0)
val UniverseTy = TopKind(2)
val NoneTy = TopKind(1)
/// Expr type is not a type
case object ExprTy extends SimpleType {
  override def ty = ???
  override val isBuilitin: Boolean = true;
}
case object CEnumTy extends SimpleType {
  override def ty = UniverseTy
  override val isBuilitin: Boolean = true;
}
case object BoolTy extends SimpleType {
  override def ty = UniverseTy
  override val isBuilitin: Boolean = true;
}
case object StrTy extends SimpleType {
  override def ty = UniverseTy
  override val isBuilitin: Boolean = true;
  override def toString: String = "str"
}
case object UnitTy extends SimpleType {
  override def ty = UniverseTy
  override val isBuilitin: Boolean = true;
}
final case class RefTy(val isRef: Boolean, val isMut: Boolean)
    extends SimpleType {
  override def ty = UniverseTy
}
final case class IntegerTy(val width: Int, val isUnsigned: Boolean)
    extends SimpleType {
  override def ty = UniverseTy
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
final case class FloatTy(val width: Int) extends SimpleType {
  override def ty = UniverseTy
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
    val t: Term = UniverseTy,
) extends Type {
  override def ty = t
  override def toString: String = s"infer(${info.name}:${info.id.id})"
}
final case class ValueTy(val value: Value) extends Type {
  override def ty = UniverseTy
  override def toString: String = value.toString
}
final case class CIdent(
    val name: String,
    val ns: List[String],
    val t: Term = UniverseTy,
) extends Term {
  override def ty = t
  override def toString: String = s"cpp($repr)"
  def repr: String = (ns :+ name).mkString("::")
}
final case class CppInsType(val target: CIdent, val arguments: List[Type])
    extends Type {
  // todo: this is not correct
  override def ty = target.ty
  override def toString: String = s"cpp(${repr(_.toString)})"
  def repr(rec: Type => String): String =
    target.repr + "<" + arguments
      .map {
        case Ref(defId, None) => defId.name
        case Ref(id, Some(v)) => rec(v)
        case ty               => rec(ty)
      }
      .mkString(", ") + ">"
}

/// Decls

final case class Param(of: VarExpr, named: Boolean) extends DeclItem {
  override val id: DefInfo = of.id

  def pretty(implicit rec: Term | Expr => String = _.toString): String =
    s"${id.defName(false)}: ${rec(id.ty)}"

  override def toString: String = s"param(${id.defName(false)})"
}

final case class Ref(
    val id: DefInfo,
    val value: Option[Term] = None,
) extends DeclItem {
  override def toString: String = s"${id.defName(false)}@${id.id.id}"
}
final case class CModule(id: DefInfo, kind: CModuleKind, path: String)
    extends DeclItem {
  def pretty(implicit rec: Term | Expr => String = _.toString): String =
    s"module ${id.defName(false)} including \"$path\""
}
enum CModuleKind {
  case Builtin, Error, Source
}
final case class NativeModule(id: DefInfo, env: Env) extends DeclItem {
  def pretty(implicit rec: Term | Expr => String = _.toString): String =
    s"module ${id.defName(false)} in ${env.fid}"
}

final case class Class(
    id: DefInfo,
    rawParams: Option[List[Param]],
    fields: FieldMap,
    args: Option[List[Term]] = None,
    variantOf: Option[Type] = None,
    resolvedAs: Option[Type] = None,
) extends DeclItem {
  lazy val varsParams = vars.map(v => Param(v.item, true)).toArray
  lazy val params: Array[Param] = {
    (rawParams.getOrElse(List()) ::: varsParams.toList).toArray
  }
  lazy val callByName: Boolean = rawParams.isEmpty;

  override def toString: String = s"class(${repr()})"
  def isPhantomClass: Boolean = id.isPhantom
  def justInit: Boolean = !id.isTrait && params.isEmpty && isPhantomClass
  def isBadInferred: Boolean = args.iterator.flatten.exists {
    case Hole(_) => true; case _ => false
  }
  def vars = fields.values.collect { case a: VarField => a }.toList
  def defs = fields.values.collect { case a: DefField => a }.toList
  def variants = fields.values.collect { case a: EnumField => a }.toList

  def repr(implicit rec: Term => String = _.toString): String =
    val argList = args.map(_.map(rec).mkString("<", ", ", ">")).getOrElse("")
    id.defName(false) + argList

  def pretty(implicit rec: Term | Expr => String = _.toString): String = ???
}
object Class {
  def empty(env: Env, isAbstract: Boolean) =
    val id = DefInfo.just(CLASS_EMPTY, env)
    Class(id, None, MutMap())
}

/// Operations

final case class BoundField(lhs: Term, by: Type, casted: Boolean, rhs: VField)
    extends Term {
  override def ty = ???
  override def toString: String =
    if casted then s"($lhs as $by).(field ${rhs.name})"
    else s"$lhs.(field ${rhs.name})"
}
final case class RefItem(lhs: Term, isMut: Boolean) extends Term {
  override def ty = ???
}
final case class Select(lhs: Term, rhs: String) extends Term {
  override def ty = ???
  override def toString: String = s"$lhs.$rhs"
}
final case class ValueMatch(
    lhs: Term,
    by: Type,
    cases: List[(Term, Term)],
    orElse: Term,
) extends Term {
  override def ty = ???
}
final case class TypeMatch(
    lhs: Term,
    by: Type,
    cases: List[(Class, Term)],
    orElse: Term,
) extends Term {
  override def ty = ???
}
sealed abstract class DeclItem extends Term with DeclLike {
  override def ty = id.ty
}
final case class HKTInstance(ty: Type, syntax: Term) extends Term {
  // override def ty = UniverseTy
  override def toString(): String = s"(hkt($syntax)::type as $ty)"
  def repr(rec: Type => String): String = syntax match {
    case t: (Ref | DefExpr) => rec(t)
    case Apply(lhs, rhs) =>
      s"${rec(HKTInstance(ty, lhs))}<${rhs.map(rec).mkString(", ")}>::type"
    case Select(lhs, rhs) => s"${rec(HKTInstance(ty, lhs))}::$rhs"
    case _                => syntax.toString
  }
}
final case class ClassInstance(
    con: Class,
    args: List[Term],
) extends Term {
  override def ty = con
  override def toString: String =
    val conAs =
      if con.resolvedAs.isDefined then s"${con.resolvedAs.get} as " else ""
    s"ins (${conAs}${con})(${args.mkString(", ")})"
}
final case class ClassDestruct(
    item: Term,
    cls: Class,
    bindings: List[Term],
) extends Term {
  override def ty = ???
}
final case class EnumDestruct(
    item: Term,
    variant: Class,
    bindings: List[Term],
) extends Term {
  override def ty = ???
}

sealed abstract class Value extends Term
case object TodoLit extends Value {
  override def ty = BottomTy
}
final case class Bool(value: Boolean) extends Value {
  val ty = BoolTy
}
final case class Int64(value: Long) extends Value {
  val ty = IntegerTy(64, false)
}
final case class Float32(value: Float) extends Value {
  val ty = FloatTy(32)
}
final case class Float64(value: Double) extends Value {
  val ty = FloatTy(64)
}
final case class Str(value: String) extends Value {
  val ty = StrTy
}
final case class Bytes(value: Array[Byte]) extends Value {
  override def ty = ???
}
final case class Rune(value: Int) extends Value {
  override def ty = ???
}
final case class DictLit(value: Map[String, Term]) extends Value {
  override def ty = ???
}

sealed abstract class VField {
  val item: DeclLike
  def name = item.name

  def pretty(implicit rec: Term | Expr => String = _.toString): String = ???
}
final case class EDefField(item: DefExpr) extends VField
final case class EEnumField(item: ClassExpr, index: Int) extends VField
final case class VarField(item: VarExpr, index: Int) extends VField
final case class DefField(item: DefExpr) extends VField
final case class EnumField(item: Class) extends VField
