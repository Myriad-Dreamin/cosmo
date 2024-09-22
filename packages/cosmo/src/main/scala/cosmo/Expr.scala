package cosmo

import scala.collection.mutable.{ListBuffer, Map as MutMap};

import ir._
import syntax as s
import syntax.{Ident, No}
import scala.collection.mutable.ArrayBuffer

type SParam = s.Param;
type SParams = List[SParam];

trait ExprEnv { self: Env =>

  def errE(e: String) = err(e).e

  def resolve(name: String) = scopes.get(name).map(Name(_, None))
  def nameOrProduce(name: Ident, strict: Boolean) =
    scopes.get(name.name) match {
      case Some(x)        => Name(ct(name, true), Some(Name(x, None)))
      case None if strict => Name(ct(name, true), None)
      case None           => hole(ct(name))
    }
  def byName(name: Ident) = nameOrProduce(name, true)

  def withNs[T](ns: Defo, ast: s.Node)(f: => T): T = {
    this.ns = ns.name :: this.ns; val res = f; this.ns = this.ns.tail
    res
  }

  def withParams[T](params: Option[SParams])(f: => T) = {
    debugln(s"withParams $params")
    scopes.withScope {
      var vars = ListBuffer[VarExpr]()
      var constraints = ListBuffer[Expr]()

      for (p <- params.getOrElse(List())) p match {
        case s.Param(Ident("-"), Some(ty @ s.BinOp(op, x: Ident, y)), _, _) =>
          op match {
            case "<:" | ":" | ">:" if !vars.contains(x.name) =>
              vars.addOne($var(ct(x), Some(UniverseTy), None, false, true))
            case _ =>
          }
          constraints.addOne(expr(ty))
        case s.Param(Ident("-"), ty, init, c) =>
          constraints.addOne(expr(ty.get))
        case s.Param(name, ty, init, c) =>
          val info = ct(name); info.isTypeVar = c
          vars.addOne($var(info, ty.map(expr), init, false, false))
      }

      (params.map(_ => vars.toList), constraints.toList, f)
    }
  }

  def expr(node: s.Node): Expr = {
    node match {
      case s.Err(msg) => errE(msg)
      // literals
      case s.TodoLit        => TodoLit.e
      case s.BoolLit(value) => Bool(value).e
      case s.IntLit(value) =>
        if (value.isValidInt) Integer(value.toInt).e
        else Opaque.expr(value.toString)
      case s.FloatLit(value) => Opaque.expr(value.toString)
      case s.StrLit(value)   => Str(value).e
      case s.Ident("self")   => SelfVal.e
      case s.Ident("Self")   => SelfTy.e
      case i: s.Ident        => byName(i)
      case s.ArgsLit(values) => argsLit(values)
      // control flow
      case b: s.Block       => block(b)
      case b: s.CaseBlock   => CaseRegion(caseBlock(b))
      case l: s.Loop        => Loop(expr(l.body))
      case w: s.While       => While(expr(w.cond), expr(w.body))
      case f: s.For         => For(expr(f.name), expr(f.iter), expr(f.body))
      case s.Break()        => Break()
      case s.Continue()     => Continue()
      case s.Return(value)  => Return(expr(value))
      case s.If(cond, x, y) => If(expr(cond), expr(x), y.map(expr))
      // operations
      case s.UnOp(op, lhs)       => UnOp(op, expr(lhs))
      case s.BinOp(op, lhs, rhs) => BinOp(op, expr(lhs), expr(rhs))
      case s.As(lhs, rhs)        => As(expr(lhs), expr(rhs))
      case s.KeyedArg(k, v)      => KeyedArg(keyExpr(k), expr(v))
      // todo: check is compile time
      case s.Select(lhs, rhs, _) => SelectExpr(expr(lhs), rhs.name)
      case b: s.Match            => $match(b)
      // todo: check is compile time
      case s.Apply(lhs, rhs, _)         => Apply(expr(lhs), rhs.map(expr))
      case s.TmplApply(Ident("a"), rhs) => Rune(rhs.head._1.head.toInt).e
      case s.TmplApply(Ident("c"), rhs) => Rune(rhs.head._1.head.toInt).e
      case s.TmplApply(Ident("b"), rhs) => Bytes(rhs.head._1.getBytes()).e
      case s.TmplApply(lhs, rhs) => Opaque.expr(s"0/* tmpl: ${lhs} ${rhs} */")
      case s.Lambda(lhs, rhs)    => Opaque.expr(s"0/* lambda: ${lhs} ${rhs} */")
      case s.Semi(None)          => ir.NoneKind(0).e
      case s.Semi(Some(value))   => expr(value)
      // todo: decorator
      case s.Decorate(s.Apply(Ident("noCore"), _, _), _) =>
        noCore = true
        NoneItem.e
      case s.Decorate(s.Apply(Ident("syntaxOnly"), _, _), _) =>
        syntaxOnly = true
        NoneItem.e
      case s.Decorate(lhs, rhs) => expr(rhs)
      // declarations
      case s.Import(p, dest) => $import(p, dest).e
      case s.Val(x, ty, y)   => $var(ct(x), ty.map(expr), y, false, false)
      case s.Typ(x, ty, y)   => $var(ct(x), ty.map(expr), y, false, true)
      case s.Var(x, ty, y)   => $var(ct(x), ty.map(expr), y, true, false)
      case d: s.Def          => $def(d, ct(d.name))
      case c: s.Class        => $class(c, ct(c.name))
      case i: s.Impl         => impl(i, ct("$impl", hidden = true))
      // syntax errors
      case SParam(name, _, _, _) => Opaque.expr(s"panic(\"param: $name\")")
      case b: s.ParamsLit =>
        errE(s"params lit without body")
        Opaque.expr(s"0/* error: case block without body */")
      case s.Case(cond, body) =>
        errE(s"case clause without match")
        Opaque.expr(s"0/* error: case clause without match */")
    }
  }

  def argsLit(values: List[s.Node]): Expr = {
    var arr = ArrayBuffer[Expr]();
    for (v <- values) {
      arr = arr :+ expr(v)
    }
    return TupleLit(arr.toArray).e
  }

  def keyExpr(n: s.Node): Expr = n match {
    case i: Ident => Str(i.name).e
    case _        => expr(n)
  }

  def hole(di: Defo): Expr = {
    di.isVar = true;
    Hole(di)
  }

  def block(ast: s.Block) = Region(scopes.withScope(ast.stmts.map(expr)))

  def caseBlock(b: s.CaseBlock) = b.stmts.map { c =>
    scopes.withScope {
      val cond = destruct(c.cond);
      val body = c.body.map(expr);
      (cond, body)
    }
  }

  def destruct(ast: s.Node): Expr = ast match {
    case i: Ident => nameOrProduce(i, false)
    // todo: check is compile time
    case s.Apply(l, r, ct) => Apply(expr(l), r.map(destruct))
    case s.KeyedArg(l, r)  => KeyedArg(keyExpr(l), destruct(r))
    case _                 => expr(ast)
  }

  def $match(b: s.Match): Expr = {
    var lhs = expr(b.lhs)
    val cases = b.rhs match {
      case b: s.CaseBlock                  => caseBlock(b)
      case b: s.Block if (b.stmts.isEmpty) => List()
      case b: s.Block => return errE(s"match body contains non-cases $b")
      case _          => return errE("match body must be a case block")
    }
    MatchExpr(lhs, CaseRegion(cases))
  }

  def $var(info: Defo, ty: Ni, init: No, mut: Boolean, ct: Boolean): VarExpr = {
    info.isMut = mut; info.isTypeVar = ct; info.isVar = true;
    VarExpr(info, ty, init.map(expr))
  }

  def $def(ast: s.Def, info: Defo): Expr = {
    val s.Def(_, params, ret_ty, rhs) = ast
    val (ps, cs, (ty, body)) =
      withParams(params)((ret_ty.map(expr), rhs.map(expr)))
    DefExpr(info, ps, cs, ty, body)
  }

  def $class(ast: s.Class, info: Defo): ClassExpr = {
    val s.Class(_, params, body, isAbstract) = ast
    val fields = MutMap[String, VField]()
    val (ps, cs, _) = withNs(info, ast)(withParams(params)(body match {
      case body: s.Block     => baseClass(body, fields, isAbstract)
      case body: s.CaseBlock => enumClass(body, fields, isAbstract)
      case body              => err(s"trait/class body is invalid kind: $body")
    }))
    ClassExpr(info, ps, cs, fields, isAbstract)
  }

  def baseClass(body: s.Block, fields: FieldMap, isAbstract: Boolean) = {
    var index = 0;
    for (stmt <- body.stmts.iterator.map(expr)) stmt match {
      case v: VarExpr =>
        if (isAbstract) then err(s"abstract class cannot have fields")
        addField(EVarField(v, index), fields); index += 1;
      case d: DefExpr =>
        addField(EDefField(d), fields); d.id.isVirtual = isAbstract;
      case node => err(s"Invalid class field $node")
    }
  }

  def enumClass(body: s.CaseBlock, fields: FieldMap, isAbstract: Boolean) = {
    if (isAbstract) then err(s"abstract trait cannot have cases")
    for ((stmt, index) <- body.stmts.zipWithIndex) {
      val variant = scopes.withScope(enumVariant(stmt, fields))
      addField(EEnumField(variant, index), fields)
    }
  }

  def enumVariant(node: s.Case, fields: FieldMap) = {
    val (subName, params) = node.cond match {
      case name: s.Ident                     => (name, List())
      case s.Apply(name: s.Ident, params, _) => (name, params)
      case _                                 => (s.Ident("invalid"), List())
    }
    val vars = params.zipWithIndex.map {
      case (s.KeyedArg(k: s.Ident, v), index) => s.Var(k, Some(v), None)
      case (n, index) => s.Var(Ident(s"_${index}"), Some(n), None)
    }
    val body = (node.body, vars) match {
      case (body, Nil)               => body.getOrElse(s.Block(List()))
      case (Some(s.Block(bc)), vars) => s.Block(vars ::: bc)
      case (Some(n), vars)           => s.Block(vars :+ n)
      case _                         => s.Block(vars)
    }
    $class(s.Class(subName, None, body, false), ct(subName))
  }

  def addField(f: VField, fields: FieldMap) = {
    if (fields.contains(f.name)) then err(s"conflict field ${f.name}")
    f.item.id.inClass = true;
    fields.addOne(f.name -> f)
  }

  def impl(ast: s.Impl, info: Defo): Expr = {
    val s.Impl(rhs, lhs, params, body) = ast
    val (cls, iface) = (expr(rhs), lhs.map(expr))
    val (ps, cs, init) = withParams(params)(expr(body))
    ImplExpr(info, ps, cs, iface, cls, init)
  }
}
