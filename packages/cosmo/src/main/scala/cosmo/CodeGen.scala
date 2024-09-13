package cosmo

import scala.compiletime.ops.boolean
import ir._

class CodeGen(implicit val env: Env) {
  val prelude = """
// NOLINTBEGIN(readability-identifier-naming,llvm-else-after-return)

"""

  val postlude = """
// NOLINTEND(readability-identifier-naming,llvm-else-after-return)
"""

  // Generate Cxx code from the env
  def gen(): String = {
    val itemCode = env.module.stmts.map(genDef(_, true)).mkString("\n")
    val errorCode = env.errors.mkString("\n")
    s"$prelude$itemCode\n#if 0\n$errorCode\n#endif$postlude"
  }

  def genDef(ast: ir.Item, tl: Boolean = false): String = {
    mayGenDef(ast, tl).getOrElse(expr(ast))
  }

  def mayGenDef(ast: ir.Item, tl: Boolean): Option[String] = {
    implicit val topLevel = tl;
    val res: String = ast match {
      case ir.NoneItem    => ""
      case ir.Semi(value) => genDef(value, tl)
      case Fn(defInfo, Sig(None, ret_ty, body), _) =>
        val name = defInfo.defName(stem = true)
        s"/* cosmo function $name */"
      case Fn(defInfo, Sig(_, Some(UniverseTy), body), _) =>
        val name = defInfo.defName(stem = true)
        s"/* cosmo function $name */"
      case Fn(defInfo, Sig(_, _, body), level) if level > 0 =>
        val name = defInfo.defName(stem = true)
        s"/* cosmo function $name */"
      case Fn(defInfo, Sig(Some(params), ret_ty, body), _) =>
        val name = defInfo.defName(stem = true)
        val hasSelf = params.exists(_.name == "self")
        val typeParams = params
          .filter(_.name != "self")
          .filter(e => e.ty.level > 1)
          .map(param => s"typename ${param.name}")
          .mkString(", ")
        val templateCode =
          if typeParams.isEmpty then "" else s"template <$typeParams> "
        val paramCode = params
          .filter(_.name != "self")
          .filter(e => e.ty.level <= 1)
          .map(param => s"${paramTy(param.ty)} ${param.name}")
          .mkString(", ")

        val bodyCode = body match {
          case Some(body) => blockizeExpr(body, ValRecv.Return)
          case None       => ";"
        }
        if (name == "main") s"int main(int argc, char **argv) { $bodyCode }"
        else
          val rt = ret_ty.map(returnTy).getOrElse("auto")
          debugln(s"$ret_ty => $rt")
          val staticModifier =
            if (defInfo.inClass && !hasSelf) "static " else ""
          s"${templateCode}inline $staticModifier$rt $name($paramCode) $bodyCode"
      // case ir.TypeAlias(ret_ty) =>
      //   s"using $name = ${returnTy(ret_ty)};"
      case ir.NativeModule(info, env, fid) =>
        val name = info.defName(stem = true)
        // fid.path (.cos -> .h)
        // todo: unreliable path conversion
        val path = fid.path.slice(0, fid.path.length - 4) + ".h"
        s"#include <${fid.pkg.namespace}/${fid.pkg.name}/src${path}>"
      case ir.CModule(id, kind, path) =>
        kind match {
          case ir.CModuleKind.Builtin =>
            s"#include <$path> // IWYU pragma: keep"
          case ir.CModuleKind.Error =>
            s"""#error "cosmo error module $path""""
          case ir.CModuleKind.Source =>
            s"""#include "$path" // IWYU pragma: keep"""
        }
      case ir.Class(defInfo, params, vars, restFields) =>
        val variants = restFields.filter(_.isInstanceOf[ir.TypeField])
        val defs = restFields.filter(_.isInstanceOf[ir.DefField])
        val item = env.items(defInfo.id)
        val name = defInfo.defName(stem = true)
        val templateCode = params
          .map { ps =>
            s"template <${ps.map(p => s"typename ${p.name}").mkString(", ")}>"
          }
          .getOrElse("")
        val emptyConstructable = vars.forall(!_.item.init.isEmpty)
        val varsCode = vars.map(p => genDef(p.item)).mkString("", ";\n", ";")
        val defsCode = defs.map(p => genDef(p.item)).mkString("\n")
        val consPref = if (vars.isEmpty) "" else s":"
        val consCode =
          s"$name(${vars.map(p => genVarParam(p.item)).mkString(", ")})$consPref${vars
              .map(p => genVarCons(p.item))
              .mkString(", ")} {}"
        val emptyConsCode =
          if variants.isEmpty && emptyConstructable && !vars.isEmpty then
            s"$name() = default;"
          else ""
        val variantNames = variants.map(_.item.id.defName(stem = true))
        val variantBases = variants.map(e =>
          e.item match
            case EnumVariant(_, _, base: ir.Class) => Some(base)
            case _                                 => None,
        )
        val variantVars =
          variantBases.map(e => e.map(_.vars).getOrElse(List.empty))
        val bodyCode =
          variantBases.flatMap(id => id.map(genDef(_))).mkString(";\n")
        val enumIdxCode = variantNames.zipWithIndex
          .map { case (name, idx) =>
            s"kIdx$name = $idx"
          }
          .mkString(", ")
        val dataCode = variantNames.mkString(", ")
        val variantCons =
          variantNames
            .map(vn => s"$name($vn &&v): data(std::move(v)) {}")
            .mkString("\n  ")
        val variantCons2 = variantNames
          .map(vn =>
            s"""template <typename... Args> static $name ${vn}_cons(Args &&...args) {
    return {${vn}(std::forward<Args>(args)...)};
  }""",
          )
          .mkString("\n  ")
        val variantFields =
          variantNames
            .zip(variantVars)
            .zipWithIndex
            .flatMap { case ((vn, vars), index) =>
              vars.map(v =>
                val defInfo = v.item.id
                val name = defInfo.nameStem(defInfo.id.id)
                val ty = defInfo.upperBounds.headOption.getOrElse(TopTy)
                val mayDeref = if ty == SelfTy then "*" else ""
                s"""const ${returnTy(
                    ty,
                  )} &${vn}${name}() const { return $mayDeref(std::get<$index>(data)).${name};};""",
              )
            }
            .mkString("\n  ")
        val variantClones: String =
          variantNames
            .zip(variantVars)
            .zipWithIndex
            .map { case ((vn, vars), index) =>
              val clones = vars
                .map(v =>
                  val defInfo = v.item.id
                  val name = defInfo.nameStem(defInfo.id.id)
                  val ty = defInfo.upperBounds.headOption.getOrElse(TopTy)
                  val mayClone = if ty == SelfTy then "->clone()" else ""
                  s"std::get<$index>(data).${name}$mayClone",
                )
                .mkString(",")
              s"""case ${index}: return ${vn}_cons($clones);"""
            }
            .mkString("\n    ")
        if (variants.isEmpty) {
          s"$templateCode struct $name {$varsCode$emptyConsCode$consCode$defsCode};"
        } else {
          s"""$templateCode struct $name {using Self = ${name};using self_t = std::unique_ptr<Self>; $bodyCode;std::variant<${dataCode}> data;

  enum { $enumIdxCode };

  $name($name &&n) : data(std::move(n.data)) {}
  $name &operator=($name &&n) {
    data = std::move(n.data);
    return *this;
  }

  $variantCons

  $variantCons2

  $variantFields

  Self clone() const { // NOLINT(misc-no-recursion)
    switch (data.index()) {
    $variantClones
    default:
      unreachable();
    }
  }

  $defsCode
};"""
        }
      case v: ir.Var => genVarStore(v)
      case a         => return None
    }

    Some(res)
  }

  def genVarParam(node: ir.Var) = {
    val ir.Var(defInfo, init, isContant, _) = node
    val name = defInfo.nameStem(defInfo.id.id)
    val ty = paramTy(defInfo.upperBounds.headOption.getOrElse(TopTy))
    var constantStr = if isContant then "const " else ""
    s"${constantStr}${ty} ${name}_p"
  }

  def genVarCons(node: ir.Var) = {
    val ir.Var(defInfo, init, isContant, _) = node
    val name = defInfo.nameStem(defInfo.id.id)
    val ty = defInfo.upperBounds.headOption.getOrElse(TopTy)
    val p = s"std::move(${name}_p)"
    if ty == SelfTy then s"$name(std::make_unique<Self>($p))"
    else s"$name($p)"
  }

  def genVarStore(node: ir.Var) = {
    val ir.Var(defInfo, init, isContant, _) = node
    val name = defInfo.nameStem(defInfo.id.id)
    val ty = storeTy(defInfo.upperBounds.headOption.getOrElse(TopTy))
    var constantStr = if isContant then "const " else ""
    val kInit =
      if (defInfo.inClass) then
        val initStr = init match {
          case Some(value) => s"${expr(value)}"
          case None        => "{}"
        }
        s"static inline $ty k${name.capitalize}Default = ${initStr};"
      else ""
    val initStr =
      if (init.isEmpty) then ""
      else if (defInfo.inClass) then s" = k${name.capitalize}Default"
      else s" = ${expr(init.getOrElse(NoneItem))}"
    s"$kInit$constantStr$ty $name$initStr"
  }

  def paramTy(ty: Type): String = {
    ty match {
      case SelfTy => "Self&&"
      case ty     => returnTy(ty)
    }
  }

  def returnTy(ty: Type): String = {
    ty match {
      case SelfTy => "Self"
      case ty     => storeTy(ty)
    }
  }

  def storeTy(ty: Type): String = env.storeTy(ty)(false)
  // // 2 args <-> dict
  // println((a: 2, b: 4,))
  // // 2 args <-> dict with string key
  // println((a: 2, ":": 4,))
  // // 1 arg <-> nested array
  // println((a: (1, 2)));
  //   println(std::move(std::map<std::string, int>{{"a", 2}, {"b", 4}}));
  //   println(std::move(std::map<std::string, int>{{"a", 2}, {":", 4}}));
  //   println(std::move(
  //       std::map<std::string, std::tuple<int, int>>{{"a", std::tuple{1, 2}}}));

  def solveDict(items: Map[String, ir.Item]): String = {

    val (keyTy, valueTy) = items.headOption match {
      case Some((k, v)) => ("std::string", storeTy(v))
      case _            => ("std::string", "int")
    }
    s"std::map<$keyTy, $valueTy>"
  }

  def bytesRepr(s: ir.Bytes): String = {
    var sb = new StringBuilder()
    // \xbb
    for (b <- s.value) {
      sb.append(f"\\x${b}%02x")
    }

    sb.toString()
  }

  // todo: analysis it concretely
  def mayClone(v: ir.VarField, value: String): String = {
    v.item match {
      case ir.Var(info, _, _, _) =>
        if (info.upperBounds.headOption.getOrElse(TopTy) == SelfTy) {
          // todo: detect rvalue correctly
          if (value.endsWith(")")) {
            s"std::move($value)"
          } else {
            s"${value}.clone()"
          }
        } else {
          value
        }
    }
  }

  def blockizeExpr(ast: ir.Item, recv: ValRecv): String = {
    ast match {
      case s: ir.Region => exprWith(s, recv)
      case a            => s"{${exprWith(a, recv)};}"
    }
  }

  def moveExpr(ast: ir.Item): String = {
    debugln(s"moveExpr: $ast")
    ast match {
      case RefItem(lhs) => expr(lhs)
      case ast          => s"std::move(${expr(ast)})"
    }
  }

  def expr(ast: ir.Item): String = {
    exprWith(ast, ValRecv.None)
  }

  def exprWith(ast: ir.Item, recv: ValRecv): String = {
    val res = ast match {
      case Integer(value)        => value.toString
      case Opaque(_, Some(stmt)) => stmt
      case Opaque(Some(expr), _) => expr
      case Region(stmts) => {
        val (rests, last) = stmts.length match {
          case 0 => return "{}"
          case 1 => (List.empty, stmts.head)
          case _ => (stmts.dropRight(1), stmts.last)
        }

        return (rests.map(genDef(_, false)) :+ mayGenDef(last, false).getOrElse(
          exprWith(last, recv),
        ))
          .mkString("{", ";\n", ";}")
      }
      case ir.Return(value) => {
        recv match {
          case ValRecv.Return => exprWith(value, ValRecv.Return)
          case recv           => s"return ${exprWith(value, recv)}"
        }
      }
      case v: Variable => v.id.env.varByRef(v)(false)
      case v: Fn       => v.id.defName()(false)
      case ir.Loop(body) =>
        return s"for(;;) ${blockizeExpr(body, ValRecv.None)}"
      case ir.While(cond, body) =>
        return s"while(${expr(cond)}) ${blockizeExpr(body, ValRecv.None)}"
      case ir.For(name, iter, body) =>
        return s"for(auto $name : ${expr(iter)}) ${blockizeExpr(body, ValRecv.None)}"
      case ir.Break()    => return "break"
      case ir.Continue() => return "continue"
      case ir.TodoLit    => return "unimplemented();"
      case ir.If(cond, cont_bb, else_bb) =>
        return s"if(${expr(cond)}) ${blockizeExpr(cont_bb, recv)}${else_bb
            .map(e => s" else ${blockizeExpr(e, recv)}")
            .getOrElse("")}"
      case ir.BinOp("..", lhs, rhs) =>
        s"Range(${expr(lhs)}, ${expr(rhs)})"
      case ir.BinOp(op, lhs, rhs) =>
        s"${expr(lhs)} $op ${expr(rhs)}"
      case ir.Apply(lhs, rhs) =>
        val rhsAnyTy = rhs.exists(_.level > 0)
        if (rhsAnyTy) {
          s"${expr(lhs)}<${rhs.map(storeTy).mkString(", ")}>"
        } else {
          s"${expr(lhs)}(${rhs.map(moveExpr).mkString(", ")})"
        }
      case ir.Select(SelfVal, rhs) => rhs
      case ir.Select(lhs, rhs) =>
        if (lhsIsType(lhs)) {
          // todo: this is not well modeled
          s"${expr(lhs)}::${rhs}"
        } else {
          s"${expr(lhs)}.${rhs}"
        }
      case SelfVal     => "(*this)"
      case Unreachable => return "unreachable();"
      case ir.Str(s)   => s"""::std::string("${escapeStr(s)}")"""
      case s: ir.Bytes => s"""Bytes("${bytesRepr(s)}")"""
      case ir.Rune(s) if s < 0x80 && !s.toChar.isControl =>
        s"Rune('${s.toChar}')"
      case ir.Rune(s) => s"Rune($s)"
      case ir.TupleLit(items) =>
        s"std::tuple{${items.map(expr).mkString(", ")}}"
      case ir.DictLit(items) =>
        s"${solveDict(items)}{${items
            .map { case (k, v) => s"""{"$k", ${expr(v)}}""" }
            .mkString(", ")}}"
      case ir.Semi(value)      => return exprWith(value, ValRecv.None)
      case v: ir.NativeType    => storeTy(v)
      case v: ir.CIdent        => storeTy(v)
      case v: ir.NativeInsType => storeTy(v)
      case v: ir.CppInsType    => storeTy(v)
      case v: ir.Var           => v.id.defName(stem = true)(false)
      case v: ir.ClassInstance => {
        val args = v.args.flatMap {
          case v: KeyedArg => Some((v.key, v.value))
          case v           => None
        }.toMap
        val positions = v.args.iterator.flatMap {
          case v: KeyedArg => None
          case v           => Some(v)
        }.iterator
        val ty = storeTy(v.iface.ty)
        val vars = v.iface.fields.iterator.flatMap {
          case (s, v: VarField) => {
            val value = args.get(s).orElse(positions.nextOption)
            Some(value.map(expr).getOrElse(s"$ty::k${s.capitalize}Default"))
          };
          case _ => None
        }
        val initArgs = vars.mkString(", ")

        s"$ty($initArgs)"
      }
      case v: ir.EnumInstance => {
        val args = v.args.flatMap {
          case v: KeyedArg => Some((v.key, v.value))
          case v           => None
        }.toMap
        val positions = v.args.iterator.flatMap {
          case v: KeyedArg => None
          case v           => Some(v)
        }.iterator
        val ty = storeTy(v.iface.ty)
        val vars = v.iface.fields.iterator.flatMap {
          case (s, v: VarField) => {
            val value = args.get(s).orElse(positions.nextOption)
            Some(
              mayClone(
                v,
                value.map(expr).getOrElse(s"$ty::k${s.capitalize}Default"),
              ),
            )
          };
          case _ => None
        }
        val initArgs = vars.mkString(", ")

        val base = storeTy(v.base.variantOf.ty);
        s"$base::${ty}_cons($initArgs)"
      }
      // case v: ir.Sig()
      case v: ir.EnumDestruct if v.bindings.isEmpty => {
        s""
      }
      case v: ir.EnumDestruct => {

        // const auto [nn] = std::get<Nat::kIdxSucc>(std::move((*this).data));
        // auto n = std::move(*nn);

        val base = storeTy(v.variant.variantOf.ty);
        val namelist = v.bindings
          .map {
            case "_" => ""
            case s   => s"_destructed_${s}"
          }
          .mkString(", ")
        val rebind = v.bindings
          .zip(
            v.variant.base match { case c: ir.Class => c.vars },
          )
          .map {
            case ("_", _) => ""
            case (s, v) => {
              val defInfo = v.item.id
              val name = defInfo.nameStem(defInfo.id.id)
              val ty = defInfo.upperBounds.headOption.getOrElse(TopTy)
              val mayDeref = if ty == SelfTy then "*" else ""
              s"auto $s = std::move(${mayDeref}_destructed_${s});"
            }
          }
          .mkString("\n")
        val vname = v.variant.id.defName(stem = true)(false)
        s"auto [$namelist] = std::get<${base}::kIdx$vname>(std::move(${expr(v.item)}.data));$rebind"
      }
      case v: ir.EnumMatch => {
        val clsName = storeTy(v.meta.ty)
        val cases = v.cases.map { case (variant, body) =>
          val name = variant.id.defName(stem = true)(false)
          s"case $clsName::kIdx$name: ${blockizeExpr(body, recv)}; break;"
        }
        val orElse = v.orElse match {
          case ir.NoneItem => ""
          case e           => s"default: ${blockizeExpr(e, recv)}"
        }
        // todo: value receiver
        return s"switch(${expr(v.lhs)}.data.index()) {${cases.mkString("\n")}\n$orElse}"
      }
      case v: ir.CEnumMatch =>
        val cases = v.cases.map { case (cond, body) =>
          val name = expr(cond)
          s"case $name: ${blockizeExpr(body, recv)}; break;"
        }
        val orElse =
          v.orElse.map(e => s"default: ${blockizeExpr(e, recv)}").getOrElse("")
        return s"switch(${expr(v.lhs)}) {${cases.mkString("\n")}\n$orElse}"
      case a => {
        println(s"unhandled expr: $a")
        a.toString()
      }
    }

    recv match {
      case ValRecv.None      => res
      case ValRecv.Return    => s"return ($res)"
      case ValRecv.Var(name) => s"$name = ($res)"
    }
  }
}

def lhsIsType(lhs: ir.Item): Boolean = {
  lhs match {
    case ir.Variable(_, _, _, _) if lhs.level == 1 => true
    case ir.Select(lhs, _)                         => lhsIsType(lhs)
    case _                                         => false
  }
}

enum ValRecv {
  case None, Return
  case Var(name: String)
}
