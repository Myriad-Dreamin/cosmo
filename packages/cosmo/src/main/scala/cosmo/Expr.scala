package cosmo

import ir._
import syntax as s
import syntax.{Ident, No}

class ExprEnv(val env: Env) {
  import env.{ct, scopes}

  var module: Expr = Opaque.expr("")

  def err(e: String) = env.err(e).e

  def resolve(name: String) = scopes.get(name).map(Name(_, None))
  def byName(name: Ident) = Name(ct(name, true), resolve(name.name))

  def withNs[T](ns: Defo, ast: syntax.Node)(f: => T): T = {
    env.ns = ns.name :: env.ns; val res = f; env.ns = env.ns.tail
    res
  }

  def withParams[T](params: Option[SParams])(f: => T) = {
    debugln(s"withParams $params")
    scopes.withScope {
      var vars = scala.collection.mutable.ListBuffer[VarExpr]()
      var constraints = scala.collection.mutable.ListBuffer[Expr]()

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
      case s.Err(msg) => err(msg)
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
      case s.KeyedArg(k, v)      => KeyedArg(expr(k), expr(v))
      // todo: check is compile time
      case s.Select(lhs, rhs, _) => SelectExpr(expr(lhs), rhs.name)
      case b: s.Match            => $match(b)
      // todo: check is compile time
      case s.Apply(lhs, rhs, _)         => ApplyExpr(expr(lhs), rhs.map(expr))
      case s.TmplApply(Ident("a"), rhs) => Rune(rhs.head._1.head.toInt).e
      case s.TmplApply(Ident("c"), rhs) => Rune(rhs.head._1.head.toInt).e
      case s.TmplApply(Ident("b"), rhs) => Bytes(rhs.head._1.getBytes()).e
      case s.TmplApply(lhs, rhs) => Opaque.expr(s"0/* tmpl: ${lhs} ${rhs} */")
      case s.Lambda(lhs, rhs)    => Opaque.expr(s"0/* lambda: ${lhs} ${rhs} */")
      case s.Semi(None)          => ir.NoneKind(0).e
      case s.Semi(Some(value))   => expr(value)
      // todo: decorator
      case s.Decorate(s.Apply(Ident("noCore"), _, _), _) =>
        env.noCore = true
        NoneItem.e
      case s.Decorate(lhs, rhs) => expr(rhs)
      // declarations
      case s.Import(p, dest) => env.$import(p, dest).e
      case s.Val(x, ty, y)   => $var(ct(x), ty.map(expr), y, false, false)
      case s.Typ(x, ty, y)   => $var(ct(x), ty.map(expr), y, false, true)
      case s.Var(x, ty, y)   => $var(ct(x), ty.map(expr), y, true, false)
      case d: s.Def          => $def(d, ct(d.name))
      case c: s.Class        => $class(c, ct(c.name))
      case i: s.Impl         => impl(i, ct("$impl", hidden = true))
      // syntax errors
      case SParam(name, _, _, _) => Opaque.expr(s"panic(\"param: $name\")")
      case b: s.ParamsLit =>
        err(s"params lit without body")
        Opaque.expr(s"0/* error: case block without body */")
      case b: s.CaseBlock =>
        err(s"case block without match")
        Opaque.expr(s"0/* error: case block without match */")
      case s.Case(cond, body) =>
        err(s"case clause without match")
        Opaque.expr(s"0/* error: case clause without match */")
    }
  }

  def argsLit(values: List[syntax.Node]): Expr = {
    var arr = List[Expr]();
    for (v <- values) {
      arr = arr :+ expr(v)
    }
    return TupleLit(arr).e
  }

  def block(ast: syntax.Block) = Region(scopes.withScope(ast.stmts.map(expr)))

  def $match(b: syntax.Match): Expr = {
    var lhs = expr(b.lhs)
    val cases = b.rhs match {
      case b: syntax.CaseBlock =>
        b.stmts.map { c => (expr(c.cond), c.body.map(expr)) }
      case b: syntax.Block if (b.stmts.isEmpty) => List()
      case b: syntax.Block => return err(s"match body contains non-cases $b")
      case _               => return err("match body must be a case block")
    }
    MatchExpr(lhs, cases)
  }

  def $var(di: Defo, ty: Ni, init: No, mut: Boolean, ct: Boolean): VarExpr = {
    di.isMut = mut; di.isTypeVar = ct; di.isVar = true;
    VarExpr(di, ty, init.map(expr))
  }

  def $def(ast: syntax.Def, defInfo: Defo): Expr = {
    val syntax.Def(_, params, ret_ty, rhs) = ast
    val (ps, cs, (ty, init)) =
      withParams(params)((ret_ty.map(expr), rhs.map(expr)))
    DefExpr(defInfo, ps, cs, ty, init)
  }

  def $class(ast: syntax.Class, defInfo: Defo): Expr = {
    val syntax.Class(_, params, body, isAbstract) = ast
    val (ps, cs, init) = withNs(defInfo, ast)(withParams(params)(expr(body)))
    ClassExpr(defInfo, ps, cs, init, isAbstract)
  }

  def impl(ast: syntax.Impl, defInfo: Defo): Expr = {
    val syntax.Impl(rhs, lhs, params, body) = ast
    val (cls, iface) = (expr(rhs), lhs.map(expr))
    val (ps, cs, init) = withParams(params)(expr(body))
    ImplExpr(defInfo, ps, cs, iface, cls, init)
  }
}
