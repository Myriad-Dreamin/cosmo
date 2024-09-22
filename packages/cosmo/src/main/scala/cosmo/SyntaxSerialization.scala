//! This file is automatically generated by copilot.

package cosmo.syntax

def toJson(node: Node): String = {
  var buf = new StringBuilder
  j(node, buf)
  buf.toString
}

type NodeKinds = Node | No | Ident | Str | Boolean | BigInt | BigDecimal |
  List[Node]
private def jpol(node: Pol, buf: StringBuilder): Unit = {
  if node.isEmpty then buf.append("null")
  else j(node.get, buf)
}
private def j(node: NodeKinds, buf: StringBuilder): Unit = {
  node match {
    case n: Node =>
      buf.append(s"{\"pos\":[${n.offset},${n.end}], \"kind\":")
    case _ =>
  }
  node match {
    case n: No      => if n.isEmpty then buf.append("null") else j(n.get, buf)
    case s: Str     => buf.append(s""""$s"""")
    case b: Boolean => buf.append(b)
    case i: BigInt  => buf.append(i.toString)
    case d: BigDecimal => buf.append(d.toString)
    case l: List[Node] => {
      buf.append("[")
      var first = true
      l.foreach { node =>
        if !first then buf.append(","); first = false
        j(node, buf)
      }
      buf.append("]")
    }

    case TodoLit    => buf.append(""" "todo"}""")
    case Semi(None) => buf.append(""" "semi"}""")
    case Semi(Some(node)) =>
      buf.append(""" "semi", "value": """)
      j(node, buf)
      buf.append("}")
    case Decorate(lhs, rhs) =>
      buf.append(""" "decorate", "lhs": """)
      j(lhs, buf)
      buf.append(""", "rhs": """)
      j(rhs, buf)
      buf.append("}")
    case Ident(name)    => buf.append(s""" "ident", "name": "$name"}""")
    case Err(msg)       => buf.append(s""" "err", "msg": "$msg"}""")
    case BoolLit(value) => buf.append(s""" "bool", "value": $value}""")
    case IntLit(value)  => buf.append(s""" "int", "value": "$value"}""")
    case FloatLit(value) =>
      buf.append(s""" "float", "value": "$value"}""")
    case StrLit(value) =>
      buf.append(s""" "string", "value": "$value"}""")
    case ParamsLit(values) =>
      buf.append(""" "params", "values": [""")
      j(values, buf)
      buf.append("]}")
    case ArgsLit(values) =>
      buf.append(""" "args", "values": [""")
      j(values, buf)
      buf.append("]}")
    case Block(stmts) =>
      buf.append(""" "block", "stmts": """)
      j(stmts, buf)
      buf.append("}")
    case Val(name, ty, init) =>
      buf.append(""" "val", "name": """)
      j(name, buf)
      buf.append(""", "ty": """)
      j(ty, buf)
      buf.append(""", "init": """)
      j(init, buf)
      buf.append("}")
    case Var(name, ty, init) =>
      buf.append(""" "var", "name": """)
      j(name, buf)
      buf.append(""", "ty": """)
      j(ty, buf)
      buf.append(""", "init": """)
      j(init, buf)
      buf.append("}")
    case Typ(name, ty, init) =>
      buf.append(""" "typ", "name": """)
      j(name, buf)
      buf.append(""", "ty": """)
      j(ty, buf)
      buf.append(""", "init": """)
      j(init, buf)
      buf.append("}")
    case Class(name, params, body, isAbstracted) =>
      buf.append(""" "class", "name": """)
      j(name, buf)
      buf.append(""", "params": """)
      jpol(params, buf)
      buf.append(""", "body": """)
      j(body, buf)
      buf.append(""", "abstracted": """)
      j(isAbstracted, buf)
      buf.append("}")
    case Impl(item, tr, params, body) =>
      buf.append(""" "impl", "item": """)
      j(item, buf)
      buf.append(""", "tr": """)
      j(tr, buf)
      buf.append(""", "params": """)
      jpol(params, buf)
      buf.append(""", "body": """)
      j(body, buf)
      buf.append("}")
    case Param(name, ty, init, isCompileTime) =>
      buf.append(""" "param", "name": """)
      j(name, buf)
      buf.append(""", "ty": """)
      j(ty, buf)
      buf.append(""", "init": """)
      j(init, buf)
      buf.append(""", "compileTime": """)
      j(isCompileTime, buf)
      buf.append("}")
    case Def(name, params, ret, rhs) =>
      buf.append(""" "def", "name": """)
      j(name, buf)
      buf.append(""", "params": """)
      jpol(params, buf)
      buf.append(""", "ret": """)
      j(ret, buf)
      buf.append(""", "rhs": """)
      j(rhs, buf)
      buf.append("}")
    case Import(path, dest) =>
      buf.append(""" "import", "path": """)
      j(path, buf)
      buf.append(""", "dest": """)
      j(dest, buf)
      buf.append("}")
    case Loop(body) =>
      buf.append(""" "loop", "body": """)
      j(body, buf)
      buf.append("}")
    case While(cond, body) =>
      buf.append(""" "while", "cond": """)
      j(cond, buf)
      buf.append(""", "body": """)
      j(body, buf)
      buf.append("}")
    case For(name, iter, body) =>
      buf.append(""" "for", "name": """)
      j(name, buf)
      buf.append(""", "iter": """)
      j(iter, buf)
      buf.append(""", "body": """)
      j(body, buf)
      buf.append("}")
    case Break()    => buf.append(""" "break"}""")
    case Continue() => buf.append(""" "continue"}""")
    case If(cond, cont_bb, else_bb) =>
      buf.append(""" "if", "cond": """)
      j(cond, buf)
      buf.append(""", "cont_bb": """)
      j(cont_bb, buf)
      buf.append(""", "else_bb": """)
      j(else_bb, buf)
      buf.append("}")
    case UnOp(op, lhs) =>
      buf.append(""" "unop", "op": """)
      j(op, buf)
      buf.append(""", "lhs": """)
      j(lhs, buf)
      buf.append("}")
    case BinOp(op, lhs, rhs) =>
      buf.append(""" "binop", "op": """)
      j(op, buf)
      buf.append(""", "lhs": """)
      j(lhs, buf)
      buf.append(""", "rhs": """)
      j(rhs, buf)
      buf.append("}")
    case Match(lhs, rhs) =>
      buf.append(""" "match", "lhs": """)
      j(lhs, buf)
      buf.append(""", "rhs": """)
      j(rhs, buf)
      buf.append("}")
    case As(lhs, rhs) =>
      buf.append(""" "as", "lhs": """)
      j(lhs, buf)
      buf.append(""", "rhs": """)
      j(rhs, buf)
      buf.append("}")
    case Select(lhs, rhs, isCompileTime) =>
      buf.append(""" "select", "lhs": """)
      j(lhs, buf)
      buf.append(""", "rhs": """)
      j(rhs, buf)
      buf.append(""", "compileTime": """)
      j(isCompileTime, buf)
      buf.append("}")
    case Apply(lhs, rhs, _) =>
      buf.append(""" "apply", "lhs": """)
      j(lhs, buf)
      buf.append(""", "rhs": [""")
      j(rhs, buf)
      buf.append("]}")
    case Lambda(lhs, rhs) =>
      buf.append(""" "lambda", "lhs": """)
      j(lhs, buf)
      buf.append(""", "rhs": """)
      j(rhs, buf)
      buf.append("}")
    case TmplApply(lhs, rhs) =>
      buf.append(""" "tmplapply", "lhs": """)
      j(lhs, buf)
      buf.append(""", "rhs": [""")
      var first = true
      rhs.foreach { arg =>
        if !first then buf.append(",")
        first = false
        buf.append("""{"name": """)
        j(arg._1, buf)
        buf.append(""", "value": """)

        if arg._2.isEmpty then buf.append("null")
        else
          for (v <- arg._2) {
            buf.append("""{"kind": "tmplapply", "lhs": """)
            j(v._1, buf)
            buf.append(""", "rhs": [""")
            var first = true
            v._2.foreach { arg =>
              if !first then buf.append(",")
              j(arg, buf)
            }
            buf.append("]}")
          }

        buf.append("}")
      }
      buf.append("]}")
    case KeyedArg(key, value) =>
      buf.append(""" "keyedarg", "key": """)
      j(key, buf)
      buf.append(""", "value": """)
      j(value, buf)
      buf.append("}")
    case Return(value) =>
      buf.append(""" "return", "value": """)
      j(value, buf)
      buf.append("}")
    case CaseBlock(stmts) =>
      buf.append(""" "caseblock", "stmts": [""")
      j(stmts, buf)
      buf.append("]}")
    case Case(cond, body) =>
      buf.append(""" "case", "cond": """)
      j(cond, buf)
      buf.append(""", "body": """)
      j(body, buf)
      buf.append("}")
  }
}
