package cosmo.inst

import cosmo.{Env, DefInfo as Defo}
import cosmo.ir
import cosmo.ir.Term
import scala.collection.mutable.ListBuffer

private type Str = String
private type Insts = List[Inst]
private type Bool = Boolean

class InstModule {
  var body = List[Inst]()
}

sealed class Inst {}

case object Unreachable extends Inst {}
final case class Block(body: Insts, v: Bool) extends Inst {}
final case class Loop(body: Insts) extends Inst {}
final case class While(cond: Inst, body: Insts) extends Inst {}
final case class For(name: Inst, iter: Inst, body: Insts) extends Inst {}
case object Break extends Inst {}
case object Continue extends Inst {}
final case class Return(value: Inst) extends Inst {}
final case class If(cond: Inst, yes: Insts, no: Insts, v: Bool) extends Inst {}
final case class Switch(cond: Inst, cases: List[(Inst, Inst)]) extends Inst {}

final case class Opaque(v: Str) extends Inst {}
final case class Ref(lhs: Defo) extends Inst {}
final case class Const(ref: Term) extends Inst {}
final case class UnInst(op: Str, lhs: Inst) extends Inst {}
final case class BinInst(op: Str, lhs: Inst, rhs: Inst) extends Inst {}
final case class Call(conv: CallCC, args: Insts) extends Inst {}

final case class ImportC(ref: ir.CModule) extends Inst {}
final case class ImportNative(ref: ir.NativeModule) extends Inst {}
final case class Var(ref: ir.Var, init: Option[Inst]) extends Inst {}
final case class Typ(ref: ir.Var, init: ir.Term) extends Inst {}
final case class Destruct(ref: List[ir.Var], init: Inst) extends Inst {}
final case class Def(ref: ir.Fn) extends ParamInst {}
final case class Class(ref: ir.Class) extends ParamInst {}
final case class Impl(ref: ir.Impl) extends ParamInst {}

sealed abstract class ParamInst extends Inst {
  var tmplParams = List[Inst]()
  var params = List[Inst]()
}

sealed abstract class CallCC {}
final case class CallFn(ref: ir.Fn) extends CallCC {}
final case class CallClass(ref: ir.Class) extends CallCC {}
final case class CallMethod(self: Inst) extends CallCC {}
final case class CallImpl(ref: ir.Impl) extends CallCC {}
final case class CallInst(ref: Inst) extends CallCC {}

final class InstModuleBuilder {
  private var current = ListBuffer[Inst]()
  private var blockStack = ListBuffer[ListBuffer[Inst]]()

  def emit(inst: Inst) = current = current :+ inst

  def block(inst: => Unit): List[Inst] = {
    blockStack += current
    current = ListBuffer()
    inst
    val res = current
    current = blockStack.head
    blockStack.remove(0)
    res.toList
  }
}

trait InstEnv { self: Env =>
  private val builder = InstModuleBuilder()

  import builder.emit

  def emitModule(body: ir.Term) = {
    val module = new InstModule

    module
  }

  def emitBlock(body: ir.Term): Insts = builder.block(emitTerm(body))
  def emitTerm(item: ir.Term): Inst = {
    // if !item.isInstanceOf[Expr] then return item
    item match {
      // control flow, todo: duplicate patterns
      case _: ir.Break    => Break
      case _: ir.Continue => Continue
      case _: ir.Opaque   => Opaque(item.toString)

      case ir.Return(v) => Return(emitTerm(v))
      case ir.If(cond, x, y) =>
        val c = emitTerm(cond)
        val region = emitBlock(x)
        val region2 = y.map(emitBlock).getOrElse(List())
        If(c, region, region2, false)
      // If(emitTerm(cond), emitTerm(x), y.map(term))
      case ir.Loop(body) => Loop(emitBlock(body))
      case ir.For(name, iter, body) =>
        val n = emitTerm(name)
        val i = emitTerm(iter)
        val b = emitBlock(body)
        For(n, i, b)
      case ir.Region(stmts, semi) =>
        val block = builder.block(stmts.map(emitTerm))
        Block(block, semi)
      case e => ???
    }
  }
}
