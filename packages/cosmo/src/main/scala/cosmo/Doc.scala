package cosmo

import scala.annotation.tailrec
import java.lang.String
import cosmo.ir.NoneKind

enum Doc {
  case NewLine
  case Str(v: String)
  case Item(v: Any)
  case Indent(i: Int, d: Doc)
  case Concat(is: Array[Doc], sep: Doc = Doc.empty)
  case Region(name: String, Attrs: Option[Doc], children: Doc)

  def pretty(implicit showDef: Boolean = false): String = {
    var sb = new StringBuilder
    Doc.prettyImpl(sb, this);
    sb.toString
  }
}

object Doc {
  def indent(i: Int, d: Doc): Doc = Doc.Indent(i, d)
  def concat(is: Array[Doc]): Doc = Doc.Concat(is)
  def join(d: Doc*): Doc = Doc.Concat(d.toArray)
  def item(v: Any): Doc = Doc.Item(v)
  def paren(d: Doc): Doc = Doc.Concat(Array(Doc.Str("("), d, Doc.Str(")")))
  def brack(d: Doc): Doc = Doc.Concat(Array(Doc.Str("["), d, Doc.Str("]")))
  def block(name: String, d: Doc): Doc = Doc.Region(name, None, d)

  val empty = Doc.Str("")
  val lbrace = Doc.Str("{")
  val rbrace = Doc.Str("}")
  val smallIndents = (0 to 30).map { " " * _ }.toArray

  private def prettyImpl(sb: StringBuilder, doc: Doc)(implicit
      showDef: Boolean = false,
  ): Unit = {
    val stack = collection.mutable.Stack[(Int, Doc)]()
    stack.push((0, doc))
    while (stack.nonEmpty) {
      val (indent, doc) = stack.pop()
      doc match {
        case Doc.NewLine =>
          sb.append("\n")
          sb.append(smallIndents(indent))
        case Doc.Str(v) =>
          sb.append(v)
        case Doc.Item(v: ir.Name) if showDef =>
          v.of match {
            case Some(v) => sb.append(s"$v")
            case None    => sb.append(s"$v!")
          }
        case Doc.Item(v) =>
          sb.append(v.toString())
        case Doc.Indent(i, d) =>
          stack.push((indent + i, d))
        case Doc.Concat(is, sep) =>
          // is.reverseIterator.foreach { d => stack.push((indent, d)) }
          is.reverseIterator.zipWithIndex.foreach { (d, i) =>
            if i > 0 then stack.push((indent, sep))
            stack.push((indent, d))
          }
        case Doc.Region(name, attrs, children) =>
          stack.push((indent, Doc.Str(s"}")))
          stack.push((indent, Doc.NewLine))
          stack.push((indent + 1, children))
          stack.push((indent + 1, Doc.NewLine))
          if attrs.isEmpty then stack.push((indent, Doc.Str(s"$name {")))
          else stack.push((indent, Doc.Str(s"$name ($attrs) {")))
      }
    }
  }

  implicit class ConcatArrayOps(val arr: Array[Doc]) extends AnyVal {
    def d(implicit sep: Doc = Doc.empty): Doc = Doc.Concat(arr, sep)
  }
  implicit class ConcatSeqOps(val seq: Seq[Doc]) extends AnyVal {
    def d(implicit sep: Doc = Doc.empty): Doc = Doc.Concat(seq.toArray, sep)
  }
  implicit class ConcatItemOps(val seq: Seq[ir.Item]) extends AnyVal {
    def d(implicit sep: Doc = Doc.empty): Doc =
      Doc.Concat(seq.map(Doc.buildItem).toArray, sep)
  }
  implicit class ConcatItem2Ops(val seq: Array[ir.Item]) extends AnyVal {
    def d(implicit sep: Doc = Doc.empty): Doc =
      Doc.Concat(seq.map(Doc.buildItem).toArray, sep)
  }
  implicit class ItemOptionOps(val o: Option[ir.Item]) extends AnyVal {
    def d: Option[Doc] = o.map(Doc.buildItem)
  }
  implicit class ItemOps(val o: ir.Item) extends AnyVal {
    def d: Doc = Doc.buildItem(o)
  }
  implicit class StringOps(val o: String) extends AnyVal {
    def d: Doc = Doc.Str(o)
  }
  implicit class IdOps(val i: ir.Defo) extends AnyVal {
    def d: Doc = Doc.Str(s"${i.name}@${i.id.id}")
  }

  def paramDecl(
      kind: String,
      pe: ir.ParamExpr,
      ret_ty: Option[ir.Type],
      body: => Doc,
  ): Doc = {
    val n = s"${pe.id.name}@${pe.id.id.id}"
    val p = pe.params.map(ps => Doc.paren(ps.d(", ".d))).getOrElse(empty)
    val cs =
      if pe.constraints.isEmpty then empty
      else Array(" where [".d, pe.constraints.d(", ".d), "]".d).d
    val r = ret_ty.d.getOrElse("_".d)
    Array(kind.d, n.d, p, ": ".d, r, cs, " = ".d, body).d
  }
  def fieldDecl(f: ir.VField): Doc = {
    if f.item.isInstanceOf[ir.DeclItem] then
      val item = f.item.asInstanceOf[ir.DeclItem]
      item.d
    else f.item.asInstanceOf[ir.DeclExpr].d
  }
  def fieldDecls(fields: ir.FieldMap): Doc = {
    val fs = fields.values.map(fieldDecl)
    Doc.block("block", fs.toSeq.d(NewLine))
  }
  def buildItem(item: ir.Item): Doc = item match {
    case b: ir.Region => Doc.block("block", b.stmts.d(NewLine))
    case f: ir.DefExpr =>
      paramDecl("def ", f, f.ret_ty, f.body.d.getOrElse(empty))
    case c: ir.ClassExpr if !c.id.isTrait =>
      paramDecl("class ", c, None, fieldDecls(c.fields))
    case c: ir.ClassExpr =>
      paramDecl("trait ", c, None, fieldDecls(c.fields))
    case impl: ir.ImplExpr =>
      // iface, " for ".d
      val iface = impl.iface.d.map(i => Array(i, " for ".d).d).getOrElse(empty)
      val cls = impl.cls.d
      val p = impl.params.map(_.d(", ".d)).getOrElse(empty)
      val f = fieldDecls(impl.fields)
      Array("impl".d, Doc.paren(p), " ".d, iface, cls, " = ".d, f).d
    case i: ir.VarExpr =>
      val r = i.ty.d.getOrElse("_".d)
      val b = i.init.d.getOrElse("_".d)
      Array(i.id.mod.d, i.id.d, ": ".d, r, " = ".d, b).d
    case i: ir.Hole =>
      Array("hole ".d, i.id.d).d
    case i: ir.Apply        => Array(i.lhs.d, Doc.paren(i.rhs.d(", ".d))).d
    case i: ir.Name         => Doc.item(i)
    case ir.ItemE(item)     => item.d
    case ir.TupleLit(items) => Doc.paren(items.d(", ".d))
    case ir.KeyedArg(k, v) =>
      Doc.Concat(Array(k.d, v.d), ": ".d)
    case ir.SelfVal    => Doc.Str("self")
    case ir.SelfTy     => Doc.Str("Self")
    case ir.Integer(v) => Doc.item(v)
    case ir.Bool(v)    => Doc.item(v)
    case ir.Str(v)     => Doc.item(s"\"${escapeStr(v)}\"")
    case ir.Rune(v)    => Doc.item(v)
    case v: ir.Opaque  => Doc.item(v)
    case ir.Bytes(v)   => Doc.item(bytesRepr(v))
    case ir.UnOp(op, lhs) =>
      Array("\"".d, op.d, "\"".d, Doc.paren(lhs.d)).d
    case ir.BinOp(op, lhs, rhs) =>
      val args = Doc.paren(Seq(lhs, rhs).d(", ".d))
      Array("\"".d, op.d, "\"".d, args).d
    case ir.SelectExpr(lhs, rhs) =>
      Array(lhs.d, ".".d, rhs.d).d
    case ir.For(name, iter, body) =>
      Array("for (".d, name.d, " in ".d, iter.d, ") ".d, body.d).d
    case ir.While(cond, body) =>
      Array("while (".d, cond.d, ") ".d, body.d).d
    case ir.Loop(body) =>
      Array("loop ".d, body.d).d
    case ir.If(cond, thenp, elsep) =>
      val el = elsep.d.map { e => Array(" else ".d, e).d }.getOrElse(empty)
      Array("if (".d, cond.d, ") ".d, thenp.d, el).d
    case ir.MatchExpr(cond, cb) =>
      val cs = cb.cases.map { c =>
        Array("case (".d, c._1.d, ") => ".d, c._2.d.getOrElse("_".d)).d
      }
      val body = (") {".d +: cs).d(NewLine)
      Array("match (".d, cond.d, indent(1, body), NewLine, "}".d).d
    case ir.As(v, ty)  => Array(v.d, " as ".d, ty.d).d
    case ir.Break()    => Doc.Str("break")
    case ir.Continue() => Doc.Str("continue")
    case ir.Return(v)  => Array("return ".d, v.d).d
    case ir.Class(id, params, fields, _, _, _) =>
      val p = params.map(_.d(", ".d)).getOrElse(empty)
      val f = fieldDecls(fields)
      Array("class ".d, id.d, Doc.paren(p), " = ".d, f).d
    case ir.Var(id, init, _) =>
      val ty = id.ty.d
      val i = init.d.getOrElse("_".d)
      Array(id.mod.d, id.d, ": ".d, ty, " = ".d, i).d
    case f: ir.Fn =>
      val p = f.rawParams.map(_.d(", ".d)).getOrElse(empty)
      val r = f.ret_ty.d
      val b = f.body.d.getOrElse("_".d)
      Array("def ".d, f.id.d, Doc.paren(p), ": ".d, r, " = ".d, b).d
    // case c: ir.Class => c.repr(c.id.env.storeTy).d
    // case v: ir.Var   => Array(v.id.mod.d, v.id.d).d
    // case f: ir.Fn    => Array("def ".d, f.id.d).d
    case i: ir.Impl => Array("impl ".d, i.id.d).d
    case ir.Param(of) =>
      Array(of.id.d, ": ".d, of.id.ty.d).d
    case ir.Ref(_, _, Some(v)) => buildItem(v)
    case ir.Ref(_, _, None)    => Doc.item(item)
    case i: ir.ClassInstance =>
      Array("instance ".d, i.con.d, Doc.paren(i.args.d(", ".d))).d
    case ir.NoneKind(_)                               => Doc.Str("none")
    case _: (ir.IntegerTy | ir.InferVar | ir.TopKind) => Doc.item(item)
    case ir.BoundField(lhs, by, casted, rhs) =>
      if casted then Array(lhs.d, " as ".d, by.d, ".".d, fieldDecl(rhs)).d
      else Array(lhs.d, ".".d, fieldDecl(rhs)).d
    case _ => Doc.item(item)
  }
}
