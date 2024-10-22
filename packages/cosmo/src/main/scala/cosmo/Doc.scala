package cosmo

import scala.annotation.tailrec
import java.lang.String
import cosmo.ir.NoneKind

import ir.{untyp, typed}

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

  implicit class ConcatArrayOps(arr: Array[Doc]) extends AnyVal {
    def d(implicit sep: Doc = Doc.empty): Doc = Doc.Concat(arr, sep)
  }
  implicit class ConcatSeqOps(seq: Seq[Doc]) extends AnyVal {
    def d(implicit sep: Doc = Doc.empty): Doc = Doc.Concat(seq.toArray, sep)
  }
  implicit class ConcatItemOps(seq: Seq[ir.Term | ir.Expr]) extends AnyVal {
    def d(implicit sep: Doc = Doc.empty): Doc =
      Doc.Concat(seq.map(Doc.buildItem).toArray, sep)
  }
  implicit class ConcatItem2Ops(seq: Array[ir.Term | ir.Expr]) extends AnyVal {
    def d(implicit sep: Doc = Doc.empty): Doc =
      Doc.Concat(seq.map(Doc.buildItem).toArray, sep)
  }
  implicit class ConcatTerm2Ops(seq: Array[ir.Term]) extends AnyVal {
    def d(implicit sep: Doc = Doc.empty): Doc =
      Doc.Concat(seq.map(Doc.buildItem).toArray, sep)
  }
  implicit class ItemOptionOps(o: Option[ir.Term | ir.Expr]) extends AnyVal {
    def d: Option[Doc] = o.map(Doc.buildItem)
  }
  implicit class ItemOps(o: ir.Term | ir.Expr) extends AnyVal {
    def d: Doc = Doc.buildItem(o)
  }
  implicit class StringOps(o: String) extends AnyVal {
    def d: Doc = Doc.Str(o)
  }
  implicit class IdOps(i: ir.Defo) extends AnyVal {
    def d: Doc = Doc.Str(s"${i.defName(false)}@${i.id.id}")
  }

  def paramDecl(
      kind: String,
      pe: ir.FuncLike,
      ret_ty: Option[ir.Expr | ir.Term],
      body: => Doc,
  ): Doc = {
    val n = s"${pe.id.name}@${pe.id.id.id}"
    val p = pe.rawParams.map(ps => Doc.paren(ps.d(", ".d))).getOrElse(empty)
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
  def buildItem(item: ir.Term | ir.Expr): Doc = item match {
    case b: typed.Region => Doc.block("block", b.stmts.d(NewLine))
    case f: ir.Def =>
      paramDecl("def ", f, f.retTyExp, f.body.d.getOrElse(empty))
    case c: ir.ClassExpr if !c.id.isTrait =>
      paramDecl("class ", c, None, fieldDecls(c.fields))
    case c: ir.ClassExpr =>
      paramDecl("trait ", c, None, fieldDecls(c.fields))
    case impl: ir.Impl =>
      // iface, " for ".d
      val iface =
        impl.ifaceExpr.d.map(i => Array(i, " for ".d).d).getOrElse(empty)
      val cls = impl.clsExpr.d
      val p = impl.synParams.map(_.d(", ".d)).getOrElse(empty)
      val f = fieldDecls(impl.fields)
      Array("impl".d, Doc.paren(p), " ".d, iface, cls, " = ".d, f).d
    case i: ir.Var =>
      // todo: expr stage
      val r = i.id.ty.d
      val bb = if i.init == null then i.initE.d else i.init.d
      val b = bb.getOrElse("_".d)
      Array(i.id.mod.d, i.id.d, ": ".d, r, " = ".d, b).d
    case i: ir.Hole =>
      Array("hole ".d, i.id.d).d
    case typed.Apply(lhs: ir.Def, rhs) =>
      Array(lhs.id.d, Doc.paren(rhs.d(", ".d))).d
    case i: typed.Apply => Array(i.lhs.d, Doc.paren(i.rhs.d(", ".d))).d
    case i: ir.Name     => Doc.item(i)
    case typed.KeyedArg(k, v) =>
      Doc.Concat(Array(k.d, v.d), ": ".d)
    case typed.TupleLit(items) => Doc.paren(items.d(", ".d))
    case ir.SelfVal            => Doc.Str("self")
    case ir.SelfTy             => Doc.Str("Self")
    case ir.Int64(v)           => Doc.item(v)
    case ir.Float32(v)         => Doc.item(v)
    case ir.Float64(v)         => Doc.item(v)
    case ir.Bool(v)            => Doc.item(v)
    case ir.Str(v)             => Doc.item(s"\"${escapeStr(v)}\"")
    case ir.Rune(v)            => Doc.item(v)
    case ir.Bytes(v)           => Doc.item(bytesRepr(v))
    case v: typed.Opaque       => Doc.item(v)
    case typed.UnOp(op, lhs) =>
      Array("\"".d, op.d, "\"".d, Doc.paren(lhs.d)).d
    case typed.BinOp(op, lhs, rhs) =>
      val args = Doc.paren(Seq(lhs, rhs).d(", ".d))
      Array("\"".d, op.d, "\"".d, args).d
    case typed.SelectExpr(lhs, rhs) =>
      Array(lhs.d, ".".d, rhs.d).d
    case typed.For(name, iter, body) =>
      Array("for (".d, name.d, " in ".d, iter.d, ") ".d, body.d).d
    case typed.While(cond, body) =>
      Array("while (".d, cond.d, ") ".d, body.d).d
    case typed.Loop(body) =>
      Array("loop ".d, body.d).d
    case typed.If(cond, thenp, elsep) =>
      val el = elsep.d.map { e => Array(" else ".d, e).d }.getOrElse(empty)
      Array("if (".d, cond.d, ") ".d, thenp.d, el).d
    case typed.MatchExpr(cond, cb) =>
      val cs = cb.cases.map { c =>
        Array("case (".d, c._1.d, ") => ".d, c._2.d.getOrElse("_".d)).d
      }
      val body = (") {".d +: cs).d(NewLine)
      Array("match (".d, cond.d, indent(1, body), NewLine, "}".d).d
    case ir.ValueMatch(lhs, by, cases, orElse) =>
      val cs = cases.map { c =>
        Array("case ".d, c._1.d, " => ".d, c._2.d).d
      }
      val body = (") {".d +: cs).d(NewLine)
      val o = Array(" else ".d, orElse.d).d
      Array(
        "valueMatch (".d,
        lhs.d,
        " by ".d,
        by.d,
        indent(1, body),
        NewLine,
        "}".d,
        o,
      ).d
    case ir.TypeMatch(lhs, by, cases, orElse) =>
      val cs = cases.map { c =>
        Array("case ".d, c._1.d, " => ".d, c._2.d).d
      }
      val body = (") {".d +: cs).d(NewLine)
      val o = Array(" else ".d, orElse.d).d
      Array(
        "typeMatch (".d,
        lhs.d,
        " by ".d,
        by.d,
        indent(1, body),
        NewLine,
        "}".d,
        o,
      ).d
    case typed.As(v, ty)  => Array(v.d, " as ".d, ty.d).d
    case typed.Break()    => Doc.Str("break")
    case typed.Continue() => Doc.Str("continue")
    case typed.Return(v)  => Array("return ".d, v.d).d
    case ir.Class(id, params, fields, _, _, _) =>
      val p = params.map(_.d(", ".d)).getOrElse(empty)
      val f = fieldDecls(fields)
      Array("class ".d, id.d, Doc.paren(p), " = ".d, f).d
    // case f: ir.Def =>
    //   val p = f.rawParams.map(_.d(", ".d)).getOrElse(empty)
    //   val r = f.retTyExp.d.getOrElse("_".d)
    //   val b = f.body.d.getOrElse("_".d)
    // Array("def ".d, f.id.d, Doc.paren(p), ": ".d, r, " = ".d, b).d
    // case c: ir.Class => c.repr(c.id.env.storeTy(_)(_.toString)).d
    // case v: ir.Var   => Array(v.id.mod.d, v.id.d).d
    // case f: ir.Def   => Array("def ".d, f.id.d).d
    // case i: ir.Impl => Array("impl ".d, i.id.d).d
    case ir.Param(of, _) =>
      Array(of.id.d, ": ".d, of.id.ty.d).d
    case i: ir.Ref => Doc.item(i)
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
