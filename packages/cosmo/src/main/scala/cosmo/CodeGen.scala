package cosmo

import scala.compiletime.ops.boolean
import ir._

class CodeGen(implicit val env: Env) {
  val fns = env.fid.map(f => s"${f.ns.mkString("::")}")
  val nsb_ = fns.map(n => s"\nnamespace ${n} {\n").getOrElse("")
  val prelude = s"""
// NOLINTBEGIN(readability-identifier-naming,llvm-else-after-return)
"""
  val nse_ = fns.map(n => s"\n} // namespace ${n}\n").getOrElse("")
  val postlude = s"""
// NOLINTEND(readability-identifier-naming,llvm-else-after-return)
"""
  var genInImpl = false

  // Generate Cxx code from the env
  def gen(): String = {
    val itemCode = env.module.stmts.map(genDef(_, true)).mkString("\n")
    val errorCode = env.errors.mkString("\n")
    s"$prelude$itemCode\n#if (${errorCode.length} > 0)\n#error \"cosmo finds some error\"\n#endif\n#if 0\n$errorCode\n#endif$postlude"
  }

  def genDef(ast: ir.Item, tl: Boolean = false): String = {
    mayGenDef(ast, tl).getOrElse(expr(ast))
  }

  def mayGenDef(ast: ir.Item, tl: Boolean): Option[String] = {
    implicit val topLevel = tl;
    val info = ast match {
      case ir.Var(info, _, _, _) => Some(info)
      case ir.Fn(info, _, _)     => Some(info)
      case ir.Class(info, _, _, _, _, _, _, _) =>
        Some(info)
      case ir.Impl(info, _, _, _, _) => Some(info)
      case _                         => None
    }
    val inClass = info.exists(_.inClass)
    val nsb = if inClass then "" else nsb_
    val nse = if inClass then "" else nse_
    val res: String = ast match {
      case ir.NoneItem    => ""
      case ir.Semi(value) => return mayGenDef(value, tl)
      case Fn(defInfo, Sig(params, retTy, body), level) if level > 0 =>
        val name = defInfo.defName(stem = true)
        val templateCode = dependentParams(params);
        val bodyCode =
          if defInfo.isDependent then s" using type =${storeTy(body.get)};"
          else ""
        s"$nsb${templateCode} struct $name {using Self = $name; $bodyCode $name() = delete;};$nse"
      case Fn(defInfo, Sig(None, ret_ty, body), _) =>
        val name = defInfo.defName(stem = true)
        s"/* cosmo function $name */"
      case Fn(defInfo, Sig(Some(params), ret_ty, body), _) =>
        val name = defInfo.defName(stem = true)
        val hasSelf = params.exists(_.id.name == "self")
        val selfIsConst = defInfo.inClass && hasSelf && {
          val self = params.find(_.id.name == "self").get
          self.id.ty match {
            case RefItem(lhs, isMut) => !isMut
            case _                   => false
          }
        }
        val typeParams = params
          .filter(_.id.name != "self")
          .filter(param => param.level > 0)
          .map(param => s"typename ${param.id.name}")
          .mkString(", ")
        val templateCode =
          if typeParams.isEmpty then "" else s"template <$typeParams> "
        val paramCode = params
          .filter(_.id.name != "self")
          .filter(param => param.level <= 0)
          .map(param => s"${paramTy(param.id.ty)} ${param.id.name}")
          .mkString(", ")

        val bodyCode = body match {
          case Some(body)                => blockizeExpr(body, ValRecv.Return)
          case None if defInfo.isVirtual => " = 0;"
          case None                      => ";"
        }
        if (name == "main") s"int main(int argc, char **argv) $bodyCode"
        else
          // val needDynAssertMut = !selfIsConst && defInfo.isOverride
          val rt = ret_ty.map(returnTy).getOrElse("auto")
          // val ret =
          //   if needDynAssertMut then s"std::enable_if_t<isMut, $rt>" else rt
          debugln(s"$ret_ty => $rt")
          val isStatic = defInfo.inClass && !hasSelf
          val staticModifier = if (isStatic) " static" else ""
          val constModifier = if selfIsConst then " const" else ""
          val virtualModifier = if defInfo.isVirtual then " virtual" else ""
          val overrideModifier = if defInfo.isOverride then " override" else ""
          val inlineModifier = if !defInfo.inClass then " inline" else ""
          s"$nsb${templateCode}$inlineModifier$staticModifier$virtualModifier $rt $name($paramCode)$constModifier$overrideModifier $bodyCode$nse"
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
      case ir.Class(defInfo, params, _, vars, restFields, true, _, _) =>
        val gii = genInImpl
        genInImpl = false
        val name = defInfo.defName(stem = true);
        val templateCode = typeParams(params).getOrElse("");
        val defsCode = restFields.map(p => genDef(p.item)).mkString("\n  ")
        genInImpl = gii
        s"""$nsb$templateCode struct ${defInfo.name} {using Self = $name;  $defsCode
  virtual ~${defInfo.name}() = default;
};$nse"""
      case ir.Class(defInfo, params, _, vars, restFields, false, _, _) =>
        val gii = genInImpl
        genInImpl = false
        val variants = restFields.filter(_.isInstanceOf[ir.TypeField])
        val defs = restFields.filter(_.isInstanceOf[ir.DefField])
        val item = env.items(defInfo.id)
        val name = defInfo.defName(stem = true)
        val templateCode = typeParams(params).getOrElse("")
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
            case EnumVariant(_, base: ir.Class) => Some(base)
            case _                              => None,
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
                val ty = defInfo.instantiateTy
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
                  val ty = defInfo.instantiateTy
                  val mayClone = if ty == SelfTy then "->clone()" else ""
                  s"std::get<$index>(data).${name}$mayClone",
                )
                .mkString(",")
              s"""case ${index}: return ${vn}_cons($clones);"""
            }
            .mkString("\n    ")
        genInImpl = gii
        if (variants.isEmpty) {
          s"$nsb$templateCode struct $name {using Self = $name;$varsCode$emptyConsCode$consCode$defsCode};$nse"
        } else {
          s"""$nsb$templateCode struct $name {using Self = ${name};using self_t = std::unique_ptr<Self>; $bodyCode;std::variant<${dataCode}> data;

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
};$nse"""
        }
      case ir.Impl(defInfo, params, iface, cls, defs) =>
        val gii = genInImpl
        genInImpl = true
        val (ifaceTy, that) = (storeTy(iface), storeTy(cls))
        val name = s"::cosmo::Impl<$that, $ifaceTy, isMut>"
        val templateCode =
          typeParams(params)
            .map(p => p.stripSuffix(">") + ", bool isMut>")
            .getOrElse("template<bool isMut>");
        val defsCode = defs.map(p => genDef(p.item)).mkString("\n")
        genInImpl = gii
        s"""${templateCode} struct $name final: public $ifaceTy {using Self = $name;
  using That = $that;
  That & self_;
  Impl(That &self_): self_(self_) {}
  template<bool isMutW = isMut> 
  Impl(const That &self_, typename std::enable_if<!isMutW, int>::type* = 0): self_(const_cast<That&>(self_)) {} ~Impl() override {}
  const That &self() const { return self_; }
  That &self() { if (!isMut) {unreachable();} return self_; } $defsCode};"""
      case v: ir.Var => genVarStore(v)
      case a         => return None
    }

    Some(res)
  }

  def dependentParams(params: Option[List[ir.Param]]): String = {
    val ps = params.getOrElse(List.empty);
    val d = if ps.isEmpty then "" else ", ";
    ps.map(p => s"typename ${p.name}")
      .mkString("template <", ", ", d + "typename Cond = void> ");
  }

  def typeParams(params: Option[List[ir.Param]]): Option[String] =
    params.map(ps =>
      s"template <${ps.map(p => s"typename ${p.name}").mkString(", ")}>",
    )

  def genVarParam(node: ir.Var) = {
    val ir.Var(defInfo, init, isContant, _) = node
    val name = defInfo.nameStem(defInfo.id.id)
    val ty = paramTy(defInfo.instantiateTy)
    var constantStr = if isContant then "const " else ""
    s"${constantStr}${ty} ${name}_p"
  }

  def genVarCons(node: ir.Var) = {
    val ir.Var(defInfo, init, isContant, _) = node
    val name = defInfo.nameStem(defInfo.id.id)
    val ty = defInfo.instantiateTy
    val p = s"std::move(${name}_p)"
    if ty == SelfTy then s"$name(std::move(std::make_unique<Self>($p)))"
    else s"$name($p)"
  }

  def genVarStore(node: ir.Var) = {
    val ir.Var(defInfo, init, isContant, _) = node
    val name = defInfo.nameStem(defInfo.id.id)
    val ty = storeTy(defInfo.instantiateTy)
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

  def solveDict(items: Map[String, ir.Item]): String = {

    val (keyTy, valueTy) = items.headOption match {
      case Some((k, v)) => ("::std::string", storeTy(v))
      case _            => ("::std::string", "int")
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
        if (info.instantiateTy == SelfTy) {
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

  var tmpRegister = 0
  def moveExpr(
      ast: ir.Item,
  )(implicit defaultMove: Boolean = true): (String, String) = {
    debugln(s"moveExpr: $ast")
    ast match {
      case RefItem(lhs, _) if isConst(lhs) => mutExpr(lhs);
      case ir.As(RefItem(lhs, _), rhs: Impl) if isConst(lhs) =>
        val (x, y) = mutExpr(lhs);
        val (z, w) = moveExpr(As(RefItem(Opaque.expr(y), true), rhs))(false);
        (x + z, w)
      case ir.As(RefItem(_, _), rhs: Impl) => ("", expr(ast));
      case RefItem(lhs, _)                 => ("", expr(lhs));
      case ir.As(lhs, rhs: Impl)           => mutExpr(Opaque.expr(expr(ast)))
      case ast if isConst(ast) || !defaultMove => ("", expr(ast))
      case ast => ("", s"std::move(${expr(ast)})")
    }
  }

  def mutExpr(rhs: ir.Item): (String, String) = {
    val name = s"tmp${tmpRegister}"
    tmpRegister += 1
    val (defCode, moveCode) = moveExpr(rhs)(false);
    (s"${defCode}auto $name = ${moveCode};", name)
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
      case v: Term => v.id.env.varByRef(v)(false)
      case v: Fn   => v.id.defName()
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
      case ir.BinOp("..", lhs, rhs) => s"Range(${expr(lhs)}, ${expr(rhs)})"
      case ir.BinOp(op, lhs, rhs)   => s"${expr(lhs)} $op ${expr(rhs)}"
      case ir.RefItem(SelfVal, _) if genInImpl => s"self()"
      case ir.RefItem(SelfVal, _)              => s"(*this)"
      case ir.RefItem(lhs, _)                  => s"::cosmo::ref(${expr(lhs)})"
      case ir.UnOp("*", SelfVal) if genInImpl  => s"self()"
      case ir.UnOp("*", SelfVal)               => s"(*this)"
      case ir.UnOp(op, lhs)                    => s"$op ${expr(lhs)}"
      case ir.As(lhs, rhs: Impl) =>
        val iface = storeTy(rhs.iface)
        val cls = storeTy(rhs.cls)
        val lhsIsMut = lhs match {
          case RefItem(lhs, isMut) => isMut
          case _                   => false
        }
        return literalCall(
          s"::cosmo::Impl<$cls, $iface, $lhsIsMut>",
          List(lhs),
          recv,
        )
      case ir.Apply(BoundField(lhs, impl: Impl, true, field), rhs) =>
        val iface = storeTy(impl.iface)
        val cls = storeTy(impl.cls)
        val lhsIsMut = lhs match {
          case RefItem(lhs, isMut) => isMut
          case _                   => false
        }
        val casted = s"::cosmo::Impl<$cls, $iface, $lhsIsMut>"
        val castedOp =
          if lhsIsType(lhs) then s"$casted::" else s"$casted(${expr(lhs)})."
        return literalCall(s"$castedOp${field.item.id.name}", rhs, recv)
      case ir.Apply(BoundField(lhs, _: Class, true, field), rhs) =>
        return literalCall(
          s"${dispatchOp(lhs, field, true)}${field.item.id.name}",
          rhs,
          recv,
        )
      case ir.Apply(BoundField(lhs, by, false, field), rhs) =>
        return literalCall(
          s"${dispatchOp(lhs, field, genInImpl)}${field.item.id.name}",
          rhs,
          recv,
        )
      case ir.Apply(lhs, rhs) => return literalCall(expr(lhs), rhs, recv)
      case BoundField(lhs, by, false, field) =>
        s"${dispatchOp(lhs, field, genInImpl)}${field.item.id.name}"
      case ir.Select(SelfVal, rhs) => rhs
      case ir.Select(lhs, rhs) =>
        if (lhsIsType(lhs)) {
          // todo: this is not well modeled
          s"${expr(lhs)}::${rhs}"
        } else {
          s"${expr(lhs)}.${rhs}"
        }
      case SelfVal if genInImpl => "self()"
      case SelfVal              => "(*this)"
      case Unreachable => return "unreachable();"
      case Bool(v)     => v.toString
      case Str(s)      => s"""::str("${escapeStr(s)}")"""
      case s: Bytes    => s"""cosmo_std::str::Bytes("${bytesRepr(s)}")"""
      case Rune(s) if s < 0x80 && !s.toChar.isControl =>
        s"Rune('${s.toChar}')"
      case Rune(s) => s"Rune($s)"
      case ir.TupleLit(items) =>
        s"std::tuple{${items.map(expr).mkString(", ")}}"
      case ir.DictLit(items) =>
        s"${solveDict(items)}{${items
            .map { case (k, v) => s"""{"$k", ${expr(v)}}""" }
            .mkString(", ")}}"
      case ir.Semi(value)   => return exprWith(value, ValRecv.None)
      case v: ir.CIdent     => storeTy(v)
      case v: ir.CppInsType => storeTy(v)
      case v: ir.Var        => v.id.defName(stem = false)
      case v: ClassInstance if v.con.variantOf.isDefined => {
        val args = v.args.flatMap {
          case v: KeyedArg => Some((v.key, v.value))
          case v           => None
        }.toMap
        val positions = v.args.iterator.flatMap {
          case v: KeyedArg => None
          case v           => Some(v)
        }.iterator
        val ty = v.con.id.defName(stem = true)
        val vars = v.con.vars.map { v =>
          {
            val name = v.item.id.name;
            val value = args.get(name).orElse(positions.nextOption)
            mayClone(
              v,
              value
                .map(expr)
                .getOrElse(s"$ty::k${name.capitalize}Default"),
            )
          }
        }
        val initArgs = vars.mkString(", ")

        val base = v.con.resolvedAs match {
          case Some(t) => storeTy(HKTInstance(v.con, t))
          case None    => s"${storeTy(v.con.variantOf.get)}::${ty}"
        }
        s"${base}_cons($initArgs)"
      }
      case v: ir.ClassInstance => {
        val args = v.args.flatMap {
          case v: KeyedArg => Some((v.key, v.value))
          case v           => None
        }.toMap
        val positions = v.args.iterator.flatMap {
          case v: KeyedArg => None
          case v           => Some(v)
        }.iterator
        val ty = storeTy(v.con)
        val vars = v.con.vars.map { v =>
          val s = v.item.id.name
          val value = args.get(s).orElse(positions.nextOption)
          value.map(expr).getOrElse(s"$ty::k${s.capitalize}Default")
        }
        val initArgs = vars.mkString(", ")

        s"$ty($initArgs)"
      }
      // case v: ir.Sig()
      case v: ir.EnumDestruct if v.bindings.isEmpty => s""
      case v: ir.EnumDestruct => {

        // const auto [nn] = std::get<Nat::kIdxSucc>(std::move((*this).data));
        // auto n = std::move(*nn);

        val base = storeTy(v.variant.variantOf.get);
        val namelist = v.bindings
          .map {
            case "_" => ""
            case s   => s"_destructed_${s}"
          }
          .mkString(", ")
        val rebind = v.bindings
          .zip(v.variant.vars)
          .map {
            case ("_", _) => ""
            case (s, v) => {
              val defInfo = v.item.id
              val name = defInfo.nameStem(defInfo.id.id)
              val ty = defInfo.instantiateTy
              val mayDeref = if ty == SelfTy then "*" else ""
              s"auto $s = std::move(${mayDeref}_destructed_${s});"
            }
          }
          .mkString("\n")
        val vname = v.variant.id.defName(stem = true)
        s"auto [$namelist] = std::get<${base}::kIdx$vname>(std::move(${expr(v.item)}.data));$rebind"
      }
      case v: ir.EnumMatch => {
        val clsName = storeTy(v.by)
        val cases = v.cases.map { case (variant, body) =>
          val name = variant.id.defName(stem = true)
          s"case $clsName::kIdx$name: ${blockizeExpr(body, recv)}; break;"
        }
        val orElse = v.orElse match {
          case ir.NoneItem => ""
          case e           => s"default: ${blockizeExpr(e, recv)}"
        }
        // todo: value receiver
        return s"switch(${expr(v.lhs)}.data.index()) {${cases.mkString("\n")}\n$orElse}"
      }
      case v: ir.ValueMatch =>
        val cases = v.cases.map { case (cond, body) =>
          val name = expr(cond)
          s"case $name: ${blockizeExpr(body, recv)}; break;"
        }
        val orElse =
          v.orElse.map(e => s"default: ${blockizeExpr(e, recv)}").getOrElse("")
        return s"switch(${expr(v.lhs)}) {${cases.mkString("\n")}\n$orElse}"
      case a => {
        logln(s"unhandled expr: $a")
        a.toString()
      }
    }

    recv match {
      case ValRecv.None      => res
      case ValRecv.Return    => s"return ($res)"
      case ValRecv.Var(name) => s"$name = ($res)"
    }
  }

  def literalCall(lhs: String, rhs: List[Item], recv: ValRecv): String = {
    val rhsAnyTy = rhs.exists(_.level > 0)
    if (rhsAnyTy) {
      s"$lhs<${rhs.map(storeTy).mkString(", ")}>"
    } else {
      val xys = rhs.map(moveExpr);
      debugln(s"literalCall: $lhs, $xys")
      val (xs, ys) = (xys.map(_._1).filter(_.nonEmpty), xys.map(_._2));
      val call = s"$lhs(${ys.mkString(", ")})";
      recv match {
        case ValRecv.None      => s"${xs.mkString(";")} $call"
        case ValRecv.Return    => s"${xs.mkString(";")} return $call"
        case ValRecv.Var(name) => s"${xs.mkString(";")} $name = $call"
      }
    }
  }

  def dispatchOp(
      lhs: ir.Item,
      field: VField,
      inImpl: Boolean = false,
  ): String = {
    val isStatic = (field, lhsIsType(lhs)) match {
      case (_, true)                             => true
      case (DefField(f), _) if isStaticMethod(f) => true
      case (TypeField(_), _)                     => true
      case _                                     => false
    }
    val lhsV = lhs match {
      case SelfVal if inImpl => "self()"
      case SelfVal           => "(*this)"
      case v if isStatic =>
        env.liftAsType(v) match {
          case RefItem(lhs, isMut) => returnTy(lhs)
          case ty                  => returnTy(ty)
        }
      case _ => expr(lhs)
    }
    if (isStatic) {
      lhsV + "::"
    } else {
      lhsV + "."
    }
  }
}

def isStaticMethod(f: ir.Item): Boolean = f match {
  case ir.Fn(_, sig, _) => isStaticMethod(sig)
  case ir.Sig(params, _, _) =>
    !params.iterator.flatten.exists(p => p.id.name == "self")
  case _ => false
}

def lhsIsType(lhs: ir.Item): Boolean = {
  lhs match {
    case ir.Term(_, _, _) if lhs.level >= 1 => true
    case ir.Select(lhs, _)                  => lhsIsType(lhs)
    case _                                  => false
  }
}

def isConst(lhs: ir.Item): Boolean = {
  lhs match {
    case Str(_) | Integer(_) | Bool(_) | Rune(_) | Bytes(_) => true
    case _                                                  => false
  }
}

enum ValRecv {
  case None, Return
  case Var(name: String)
}
