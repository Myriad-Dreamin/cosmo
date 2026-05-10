package cosmo0

import scala.collection.mutable.ListBuffer

import cosmo.syntax
import cosmo.syntax.*

object UntypedElaborator:
  val defaultStandardGenericNames: Set[String] =
    Set("Arena", "Box", "Id", "Map", "Option", "Ptr", "Ref", "RefMut", "Result", "Set", "Vec")

  def apply(): UntypedElaborator =
    new UntypedElaborator(defaultStandardGenericNames)

final class UntypedElaborator(
    standardGenericNames: Set[String],
):
  def elaborate(parsed: ParsedModule): Result[UntypedModule] =
    val state = State(parsed.source, standardGenericNames)
    val declarations = parsed.ast.stmts.flatMap(state.moduleDecl)
    val diagnostics = state.diagnostics.toList

    if diagnostics.isEmpty then
      Result.success(
        Phase.Check,
        UntypedModule(
          parsed.source,
          declarations,
          state.nodeSpan(parsed.ast),
        ),
      )
    else
      Result(
        Phase.Check,
        PhaseStatus.Unsupported,
        None,
        diagnostics,
      )

  private final class State(
      source: SourceFile,
      standardGenericNames: Set[String],
  ):
    val diagnostics: ListBuffer[Diagnostic] = ListBuffer.empty

    private val assignmentOps =
      Set("=", "+=", "-=", "*=", "/=", "%=", "&=", "|=", "^=", "<<=", ">>=")

    private val higherOrderMethodNames =
      Set("filter", "flatMap", "fold", "forEach", "foreach", "map")

    def moduleDecl(node: syntax.Node): Option[UntypedDecl] =
      unwrapSemi(node) match
        case None => None
        case Some(importNode: Import) => importDecl(importNode)
        case Some(classNode: Class)   => classDecl(classNode)
        case Some(defNode: Def)       => functionDecl(defNode)
        case Some(valueNode: Val)     => valueDecl(valueNode, UntypedValueKind.Val)
        case Some(valueNode: Var)     => valueDecl(valueNode, UntypedValueKind.Var)
        case Some(typeNode: Typ)      => typeAlias(typeNode)
        case Some(implNode: Impl) =>
          unsupported(
            implNode,
            "cosmo0.elaborate.unsupported.impl",
            "impl declarations are outside the initial cosmo0 subset",
          )
        case Some(decorated: Decorate) =>
          unsupported(
            decorated,
            "cosmo0.elaborate.unsupported.decorator",
            "decorators and staging annotations are outside the initial cosmo0 subset",
          )
        case Some(caseNode: Case) =>
          unsupported(
            caseNode,
            "cosmo0.elaborate.unsupported.top-level-case",
            "case variants are only supported inside cosmo0 classes",
          )
        case Some(other) =>
          unsupported(
            other,
            "cosmo0.elaborate.unsupported.top-level",
            s"${constructName(other)} is not a supported top-level cosmo0 declaration",
          )

    private def importDecl(node: Import): Option[UntypedImport] =
      val path = pathFromNode(node.path, Some(nodeSpan(node)))
      val dest = node.dest.map(pathFromNode(_, Some(nodeSpan(node))))
      for
        p <- path
        d <- sequence(dest.toList).map(_.headOption)
      yield UntypedImport(p, d, nodeSpan(node))

    private def classDecl(node: Class): Option[UntypedClass] =
      if node.ab then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.trait",
          "traits are outside the initial cosmo0 subset",
        )
      else if hasExplicitTypeParams(node.ps) then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.generic-class",
          "user-defined generic classes are outside the initial cosmo0 subset",
        )
      else if node.ps.exists(_.nonEmpty) then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.class-params",
          "class parameter lists are outside the initial cosmo0 subset",
        )
      else
        val members = classMembers(node.body)
        sequence(members).map(UntypedClass(node.name.name, _, nodeSpan(node)))

    private def classMembers(node: syntax.Node): List[Option[UntypedClassMember]] =
      unwrapSemi(node) match
        case None => Nil
        case Some(block: Block) =>
          block.stmts.map(classMember)
        case Some(caseBlock: CaseBlock) =>
          caseBlock.stmts.map(variantDecl)
        case Some(other) =>
          List(
            unsupported(
              other,
              "cosmo0.elaborate.unsupported.class-body",
              "cosmo0 class bodies must be blocks containing fields, methods, aliases, or case variants",
            ),
          )

    private def classMember(node: syntax.Node): Option[UntypedClassMember] =
      unwrapSemi(node) match
        case None => None
        case Some(valueNode: Val) => valueDecl(valueNode, UntypedValueKind.Val)
        case Some(valueNode: Var) => valueDecl(valueNode, UntypedValueKind.Var)
        case Some(defNode: Def)   => functionDecl(defNode)
        case Some(typeNode: Typ)  => typeAlias(typeNode)
        case Some(caseNode: Case) => variantDecl(caseNode)
        case Some(decorated: Decorate) =>
          unsupported(
            decorated,
            "cosmo0.elaborate.unsupported.decorator",
            "decorators and staging annotations are outside the initial cosmo0 subset",
          )
        case Some(implNode: Impl) =>
          unsupported(
            implNode,
            "cosmo0.elaborate.unsupported.impl",
            "impl declarations are outside the initial cosmo0 subset",
          )
        case Some(other) =>
          unsupported(
            other,
            "cosmo0.elaborate.unsupported.class-member",
            s"${constructName(other)} is not a supported cosmo0 class member",
          )

    private def functionDecl(node: Def): Option[UntypedFunction] =
      if hasExplicitTypeParams(node.params) then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.generic-function",
          "user-defined generic functions are outside the initial cosmo0 subset",
        )
      else
        val params = node.params.getOrElse(Nil).map(param)
        val returnType = node.ret.map(typeFromNode(_, Some(nodeSpan(node))))
        val body = node.rhs.map(expr)
        for
          ps <- sequence(params)
          rt <- sequence(returnType.toList).map(_.headOption)
          b <- sequence(body.toList).map(_.headOption)
        yield UntypedFunction(node.name.name, ps, rt, b, nodeSpan(node))

    private def valueDecl(
        node: Val,
        kind: UntypedValueKind,
    ): Option[UntypedValueDecl] =
      val valueType = node.ty.map(typeFromNode(_, Some(nodeSpan(node))))
      val init = node.init.map(expr)
      for
        t <- sequence(valueType.toList).map(_.headOption)
        i <- sequence(init.toList).map(_.headOption)
      yield UntypedValueDecl(kind, node.name.name, t, i, nodeSpan(node))

    private def valueDecl(
        node: Var,
        kind: UntypedValueKind,
    ): Option[UntypedValueDecl] =
      val valueType = node.ty.map(typeFromNode(_, Some(nodeSpan(node))))
      val init = node.init.map(expr)
      for
        t <- sequence(valueType.toList).map(_.headOption)
        i <- sequence(init.toList).map(_.headOption)
      yield UntypedValueDecl(kind, node.name.name, t, i, nodeSpan(node))

    private def typeAlias(node: Typ): Option[UntypedTypeAlias] =
      node.init.orElse(node.ty) match
        case Some(targetNode) =>
          typeFromNode(targetNode, Some(nodeSpan(node))).map(target =>
            UntypedTypeAlias(node.name.name, target, nodeSpan(node)),
          )
        case None =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.type-alias-target",
            "cosmo0 type aliases must name a concrete target type",
          )

    private def variantDecl(node: Case): Option[UntypedVariant] =
      if node.body.nonEmpty then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.variant-body",
          "enum-style cosmo0 case variants cannot define branch bodies",
        )
      else
        node.cond match
          case name: Ident =>
            Some(UntypedVariant(name.name, Nil, nodeSpan(node)))
          case Apply(name: Ident, args, false) =>
            val fields = args.map(variantField)
            sequence(fields).map(UntypedVariant(name.name, _, nodeSpan(node)))
          case other =>
            unsupported(
              other,
              "cosmo0.elaborate.unsupported.variant",
              "cosmo0 case variants must be a variant name with optional payload types",
            )

    private def variantField(node: syntax.Node): Option[UntypedVariantField] =
      node match
        case KeyedArg(name: Ident, valueType) =>
          typeFromNode(valueType, Some(nodeSpan(node))).map(t =>
            UntypedVariantField(Some(name.name), t, nodeSpan(node)),
          )
        case KeyedArg(_, _) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.variant-field",
            "named variant fields must use an identifier field name",
          )
        case other =>
          typeFromNode(other, Some(nodeSpan(other))).map(t =>
            UntypedVariantField(None, t, nodeSpan(other)),
          )

    private def param(node: Param): Option[UntypedParam] =
      if node.ct then
        unsupported(
          node,
          "cosmo0.elaborate.unsupported.type-param",
          "explicit compile-time parameters are outside the initial cosmo0 subset",
        )
      else
        val valueType = node.ty.map(typeFromNode(_, Some(nodeSpan(node))))
        val default = node.init.map(expr)
        for
          t <- sequence(valueType.toList).map(_.headOption)
          d <- sequence(default.toList).map(_.headOption)
        yield UntypedParam(node.name.name, t, d, nodeSpan(node))

    private def blockItem(node: syntax.Node): Option[UntypedBlockItem] =
      unwrapSemi(node) match
        case None => None
        case Some(valueNode: Val) => local(valueNode, UntypedValueKind.Val)
        case Some(valueNode: Var) => local(valueNode, UntypedValueKind.Var)
        case Some(other)          => expr(other)

    private def local(
        node: Val,
        kind: UntypedValueKind,
    ): Option[UntypedLocal] =
      val valueType = node.ty.map(typeFromNode(_, Some(nodeSpan(node))))
      val init = node.init.map(expr)
      for
        t <- sequence(valueType.toList).map(_.headOption)
        i <- sequence(init.toList).map(_.headOption)
      yield UntypedLocal(kind, node.name.name, t, i, nodeSpan(node))

    private def local(
        node: Var,
        kind: UntypedValueKind,
    ): Option[UntypedLocal] =
      val valueType = node.ty.map(typeFromNode(_, Some(nodeSpan(node))))
      val init = node.init.map(expr)
      for
        t <- sequence(valueType.toList).map(_.headOption)
        i <- sequence(init.toList).map(_.headOption)
      yield UntypedLocal(kind, node.name.name, t, i, nodeSpan(node))

    private def expr(node: syntax.Node): Option[UntypedExpr] =
      unwrapSemi(node) match
        case None => Some(UntypedUnitLiteral(nodeSpan(node)))
        case Some(block: Block) =>
          val items = block.stmts.map(blockItem)
          sequence(items).map(UntypedBlock(_, nodeSpan(block)))
        case Some(name: Ident) =>
          Some(UntypedName(UntypedPath(List(name.name), nodeSpan(name)), nodeSpan(name)))
        case Some(BoolLit(value)) =>
          Some(UntypedBoolLiteral(value, nodeSpan(node)))
        case Some(IntLit(value)) =>
          Some(UntypedIntLiteral(value, nodeSpan(node)))
        case Some(FloatLit(value)) =>
          Some(UntypedFloatLiteral(value, nodeSpan(node)))
        case Some(StrLit(value)) =>
          Some(UntypedStringLiteral(value, nodeSpan(node)))
        case Some(Select(lhs, rhs, false)) =>
          expr(lhs).map(receiver => UntypedSelect(receiver, rhs.name, nodeSpan(node)))
        case Some(Select(lhs, rhs, true)) =>
          typeFromNode(lhs, Some(nodeSpan(node))).map(owner =>
            UntypedVariantConstructor(owner, rhs.name, nodeSpan(node)),
          )
        case Some(Apply(Select(_, rhs, false), _, false))
            if higherOrderMethodNames.contains(rhs.name) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.higher-order-api",
            s"${rhs.name} is outside the initial cosmo0 higher-order API subset",
          )
        case Some(Apply(lhs, args, false)) =>
          val callee = expr(lhs)
          val callArgs = args.map(expr)
          for
            c <- callee
            as <- sequence(callArgs)
          yield UntypedCall(c, as, nodeSpan(node))
        case Some(applyNode @ Apply(_, _, true)) =>
          unsupported(
            applyNode,
            "cosmo0.elaborate.unsupported.compile-time-apply",
            "compile-time type application is only supported in cosmo0 type positions",
          )
        case Some(BinOp(op, lhs, rhs)) if assignmentOps.contains(op) =>
          for
            target <- expr(lhs)
            value <- expr(rhs)
          yield UntypedAssign(target, value, op, nodeSpan(node))
        case Some(BinOp(op, lhs, rhs)) =>
          for
            left <- expr(lhs)
            right <- expr(rhs)
          yield UntypedBinary(op, left, right, nodeSpan(node))
        case Some(UnOp("mut", _)) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.mut-expression",
            "mut is only supported in cosmo0 reference type syntax",
          )
        case Some(UnOp(op, lhs)) =>
          expr(lhs).map(value => UntypedUnary(op, value, nodeSpan(node)))
        case Some(If(cond, thenBranch, elseBranch)) =>
          for
            c <- expr(cond)
            t <- expr(thenBranch)
            e <- sequence(elseBranch.map(expr).toList).map(_.headOption)
          yield UntypedIf(c, t, e, nodeSpan(node))
        case Some(Loop(body)) =>
          expr(body).map(UntypedLoop(_, nodeSpan(node)))
        case Some(While(cond, body)) =>
          for
            c <- expr(cond)
            b <- expr(body)
          yield UntypedWhile(c, b, nodeSpan(node))
        case Some(For(name, iter, body)) =>
          for
            i <- expr(iter)
            b <- expr(body)
          yield UntypedFor(name.name, i, b, nodeSpan(node))
        case Some(Match(lhs, rhs: CaseBlock)) =>
          val arms = rhs.stmts.map(matchArm)
          for
            scrutinee <- expr(lhs)
            as <- sequence(arms)
          yield UntypedMatch(scrutinee, as, nodeSpan(node))
        case Some(Match(_, rhs)) =>
          unsupported(
            rhs,
            "cosmo0.elaborate.unsupported.match-body",
            "cosmo0 match expressions must contain only case arms",
          )
        case Some(Return(value)) =>
          expr(value).map(UntypedReturn(_, nodeSpan(node)))
        case Some(Break()) =>
          Some(UntypedBreak(nodeSpan(node)))
        case Some(Continue()) =>
          Some(UntypedContinue(nodeSpan(node)))
        case Some(lambda: Lambda) =>
          unsupported(
            lambda,
            "cosmo0.elaborate.unsupported.lambda",
            "lambdas and closures are outside the initial cosmo0 subset",
          )
        case Some(asNode: As) =>
          unsupported(
            asNode,
            "cosmo0.elaborate.unsupported.cast",
            "as-casts are outside the initial cosmo0 subset",
          )
        case Some(tmpl: TmplApply) =>
          unsupported(
            tmpl,
            "cosmo0.elaborate.unsupported.template-literal",
            "template literals are outside the initial cosmo0 subset",
          )
        case Some(args: ArgsLit) =>
          unsupported(
            args,
            "cosmo0.elaborate.unsupported.tuple",
            "tuple and named-argument literals are outside the initial cosmo0 subset",
          )
        case Some(params: ParamsLit) =>
          unsupported(
            params,
            "cosmo0.elaborate.unsupported.params-literal",
            "parameter literals are outside the initial cosmo0 subset",
          )
        case Some(keyed: KeyedArg) =>
          unsupported(
            keyed,
            "cosmo0.elaborate.unsupported.named-argument",
            "named arguments are outside the initial cosmo0 subset",
          )
        case Some(TodoLit) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.todo",
            "??? placeholders are outside the initial cosmo0 subset",
          )
        case Some(decorated: Decorate) =>
          unsupported(
            decorated,
            "cosmo0.elaborate.unsupported.decorator",
            "decorators and staging annotations are outside the initial cosmo0 subset",
          )
        case Some(_: Val | _: Var | _: Typ | _: Def | _: Class | _: Impl | _: Import | _: Case) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.declaration-expression",
            s"${constructName(node)} cannot appear as a cosmo0 expression",
          )
        case Some(caseBlock: CaseBlock) =>
          unsupported(
            caseBlock,
            "cosmo0.elaborate.unsupported.case-block",
            "case blocks are only supported as match bodies",
          )
        case Some(err: Err) =>
          unsupported(
            err,
            "cosmo0.elaborate.unsupported.parser-error",
            err.msg,
          )
        case Some(other) =>
          unsupported(
            other,
            "cosmo0.elaborate.unsupported.expression",
            s"${constructName(other)} is outside the initial cosmo0 expression subset",
          )

    private def matchArm(node: Case): Option[UntypedMatchArm] =
      val p = pattern(node.cond)
      val b = node.body.map(expr)
      for
        patternValue <- p
        bodyValue <- sequence(b.toList).map(_.headOption)
      yield UntypedMatchArm(patternValue, bodyValue, nodeSpan(node))

    private def pattern(node: syntax.Node): Option[UntypedPattern] =
      node match
        case Ident("_") =>
          Some(UntypedWildcardPattern(nodeSpan(node)))
        case Ident(name) =>
          Some(UntypedBindingPattern(name, nodeSpan(node)))
        case BoolLit(value) =>
          Some(UntypedBoolLiteral(value, nodeSpan(node)))
        case IntLit(value) =>
          Some(UntypedIntLiteral(value, nodeSpan(node)))
        case FloatLit(value) =>
          Some(UntypedFloatLiteral(value, nodeSpan(node)))
        case StrLit(value) =>
          Some(UntypedStringLiteral(value, nodeSpan(node)))
        case Apply(callee, args, false) =>
          val constructor = expr(callee)
          val argPatterns = args.map(pattern)
          for
            c <- constructor
            as <- sequence(argPatterns)
          yield UntypedVariantPattern(c, as, nodeSpan(node))
        case select: Select =>
          expr(select).map(constructor => UntypedVariantPattern(constructor, Nil, nodeSpan(node)))
        case other =>
          unsupported(
            other,
            "cosmo0.elaborate.unsupported.pattern",
            s"${constructName(other)} is not a supported cosmo0 pattern",
          )

    private def typeFromNode(
        node: syntax.Node,
        fallbackSpan: Option[SourceSpan] = None,
    ): Option[UntypedType] =
      node match
        case Ident("Type") =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.host-type",
            "host Type and type-level programming are outside the initial cosmo0 subset",
          )
        case UnOp("&", UnOp("mut", target)) =>
          typeFromNode(target, fallbackSpan).map(t =>
            UntypedRefType(t, mutable = true, nodeSpan(node, fallbackSpan)),
          )
        case UnOp("&", target) =>
          typeFromNode(target, fallbackSpan).map(t =>
            UntypedRefType(t, mutable = false, nodeSpan(node, fallbackSpan)),
          )
        case Apply(lhs, args, true) =>
          pathFromNode(lhs, fallbackSpan) match
            case Some(base) if standardGenericNames.contains(base.parts.lastOption.getOrElse("")) =>
              val typeArgs = args.map(typeFromNode(_, Some(nodeSpan(node, fallbackSpan))))
              sequence(typeArgs).map { values =>
                base.parts.lastOption match
                  case Some("Ref") if values.size == 1 =>
                    UntypedRefType(values.head, mutable = false, nodeSpan(node, fallbackSpan))
                  case Some("RefMut") if values.size == 1 =>
                    UntypedRefType(values.head, mutable = true, nodeSpan(node, fallbackSpan))
                  case _ =>
                    UntypedAppliedType(base, values, nodeSpan(node, fallbackSpan))
              }
            case Some(base) =>
              unsupported(
                node,
                "cosmo0.elaborate.unsupported.generic-type",
                s"${base.text} is not a registered cosmo0 standard generic type",
              )
            case None => None
        case Apply(_, _, false) =>
          unsupported(
            node,
            "cosmo0.elaborate.unsupported.runtime-type-apply",
            "cosmo0 type applications must use compile-time square-bracket syntax",
          )
        case pathNode =>
          pathFromNode(pathNode, fallbackSpan).map(path =>
            UntypedNamedType(path, nodeSpan(pathNode, fallbackSpan)),
          )

    private def pathFromNode(
        node: syntax.Node,
        fallbackSpan: Option[SourceSpan] = None,
    ): Option[UntypedPath] =
      node match
        case Ident(name) =>
          Some(UntypedPath(List(name), nodeSpan(node, fallbackSpan)))
        case Select(lhs, rhs, _) =>
          pathFromNode(lhs, fallbackSpan).map(path =>
            UntypedPath(path.parts :+ rhs.name, nodeSpan(node, fallbackSpan)),
          )
        case other =>
          unsupported(
            other,
            "cosmo0.elaborate.unsupported.path",
            s"${constructName(other)} cannot be used as a cosmo0 path",
          )

    def nodeSpan(node: syntax.Node): SourceSpan =
      nodeSpan(node, None)

    private def nodeSpan(
        node: syntax.Node,
        fallbackSpan: Option[SourceSpan],
    ): SourceSpan =
      if node.offset >= 0 && node.end >= node.offset then source.span(node.offset, node.end)
      else fallbackSpan.getOrElse(source.span(0, 0))

    private def unwrapSemi(node: syntax.Node): Option[syntax.Node] =
      node match
        case Semi(None)        => None
        case Semi(Some(inner)) => unwrapSemi(inner)
        case other             => Some(other)

    private def hasExplicitTypeParams(params: Option[List[Param]]): Boolean =
      params.exists(_.exists(_.ct))

    private def sequence[A](items: List[Option[A]]): Option[List[A]] =
      val values = ListBuffer.empty[A]
      var ok = true
      items.foreach {
        case Some(value) => values += value
        case None        => ok = false
      }
      if ok then Some(values.toList) else None

    private def unsupported[A](
        node: syntax.Node,
        code: String,
        message: String,
    ): Option[A] =
      diagnostics += Diagnostic(
        Phase.Check,
        DiagnosticSeverity.Error,
        code,
        message,
        Some(nodeSpan(node)),
      )
      None

    private def constructName(node: syntax.Node): String =
      node.getClass.getSimpleName.stripSuffix("$")
