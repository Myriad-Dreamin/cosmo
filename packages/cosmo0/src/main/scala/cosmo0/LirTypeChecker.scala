package cosmo0

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object LirTypeChecker:
  def apply(): LirTypeChecker =
    new LirTypeChecker(StandardGenericDescriptors.all)

final class LirTypeChecker(
    descriptors: Map[String, StandardGenericDescriptor] = StandardGenericDescriptors.all,
):
  def check(module: LirModule): Result[LirModule] =
    val state = State(module, descriptors)
    state.check()
    val diagnostics = state.diagnostics
    if diagnostics.isEmpty then Result.success(Phase.Check, module)
    else Result.failure(Phase.Check, diagnostics)

  private final case class DeclEnv(
      declarations: Map[LirDeclId, LirDeclaration],
      functions: Map[LirDeclId, LirFunction],
      globals: Map[LirDeclId, LirGlobal],
      typesByName: Map[String, LirTypeDecl],
      typesById: Map[LirDeclId, LirTypeDecl],
  )

  private final case class LocalInfo(
      id: LirLocalId,
      name: String,
      valueType: LirTypeRef,
      mutable: Boolean,
      mutationAllowed: Boolean,
      param: Boolean,
  )

  private final case class FunctionEnv(
      function: LirFunction,
      declarations: DeclEnv,
      locals: Map[LirLocalId, LocalInfo],
      blocks: Map[LirLabel, LirBlock],
      entry: LirLabel,
  )

  private final case class PlaceInfo(
      valueType: LirTypeRef,
      mutable: Boolean,
      local: Option[LirLocalId],
      description: String,
  )

  private final case class VariantShape(
      owner: LirTypeRef,
      variant: String,
      payload: List[LirTypeRef],
  )

  private final class State(
      module: LirModule,
      descriptors: Map[String, StandardGenericDescriptor],
  ):
    private val errors = ListBuffer.empty[Diagnostic]

    def diagnostics: List[Diagnostic] =
      errors.toList

    def check(): Unit =
      val declarations = buildDeclarationEnv()
      checkTypeDeclarations(declarations)
      checkGlobals(declarations)
      declarations.functions.values.toList.sortBy(_.id.value).foreach(checkFunction(_, declarations))

    private def buildDeclarationEnv(): DeclEnv =
      module.declarations
        .groupBy(_.id)
        .toList
        .sortBy(_._1.value)
        .foreach { case (id, declarations) =>
          if declarations.length > 1 then
            error(
              "cosmo0.lir.duplicate-declaration",
              s"duplicate LIR declaration id $id",
            )
        }

      val declarations = firstBy(module.declarations)(_.id)
      val functions = declarations.collect { case (id, function: LirFunction) => id -> function }
      val globals = declarations.collect { case (id, global: LirGlobal) => id -> global }
      val typeDecls = declarations.collect { case (id, ty: LirTypeDecl) => id -> ty }

      typeDecls.values
        .toList
        .groupBy(_.name)
        .toList
        .sortBy(_._1)
        .foreach { case (name, declarations) =>
          if declarations.length > 1 then
            error(
              "cosmo0.lir.duplicate-type",
              s"duplicate LIR type name $name",
            )
        }

      DeclEnv(
        declarations,
        functions,
        globals,
        typeDecls.values.map(ty => ty.name -> ty).toMap,
        typeDecls,
      )

    private def checkTypeDeclarations(declarations: DeclEnv): Unit =
      declarations.typesById.values.toList.sortBy(_.id.value).foreach { ty =>
        ty.fields
          .groupBy(_.name)
          .toList
          .sortBy(_._1)
          .foreach { case (name, fields) =>
            if fields.length > 1 then
              error(
                "cosmo0.lir.duplicate-field",
                s"${ty.id} declares duplicate field $name",
              )
          }

        ty.variants
          .groupBy(_.name)
          .toList
          .sortBy(_._1)
          .foreach { case (name, variants) =>
            if variants.length > 1 then
              error(
                "cosmo0.lir.duplicate-variant",
                s"${ty.id} declares duplicate variant $name",
              )
          }
      }

    private def checkGlobals(declarations: DeclEnv): Unit =
      declarations.globals.values.toList.sortBy(_.id.value).foreach { global =>
        global.initializer.foreach { initializer =>
          checkModuleValue(initializer, declarations)
          if !assignable(initializer.valueType, global.valueType) then
            error(
              "cosmo0.lir.assignment-mismatch",
              s"${global.id} initializer has type ${initializer.valueType.display}, expected ${global.valueType.display}",
            )
        }
      }

    private def checkFunction(function: LirFunction, declarations: DeclEnv): Unit =
      function.sourceSignature.foreach(checkSourceSignature(function, _))

      if function.externBinding.nonEmpty then
        checkExternFunction(function)
        return

      if function.blocks.isEmpty then
        error(
          "cosmo0.lir.missing-block",
          s"${function.id} has no LIR blocks",
        )
        return

      val locals = buildLocalEnv(function)
      val blocks = buildBlockEnv(function)
      val entry = blocks.get(LirLabel("entry")).map(_.label).getOrElse(blocks.keys.toList.sortBy(_.value).head)
      val env = FunctionEnv(function, declarations, locals, blocks, entry)

      checkBranchTargets(env)
      val inSets = computeDefinedInputs(env)
      inSets.keys.toList.sortBy(_.value).foreach { label =>
        blocks.get(label).foreach { block =>
          checkReachableBlock(block, env, inSets(label))
        }
      }

    private def checkSourceSignature(function: LirFunction, source: CallableSignature): Unit =
      val callableParams =
        source.receiver match
          case Some(receiver) =>
            function.params.headOption match
              case Some(param) if param.name == "self" =>
                checkReceiverParam(function, param, receiver)
                function.params.tail
              case _ =>
                error(
                  "cosmo0.lir.invalid-signature",
                  s"${function.id} source signature has a receiver but the LIR function has no self parameter",
                )
                function.params
          case None =>
            function.params
      val sourceParams = source.params.map(param => LirTypeRef(param.valueType))
      if sourceParams.length != callableParams.length then
        error(
          "cosmo0.lir.invalid-signature",
          s"${function.id} has ${callableParams.length} callable LIR parameter(s), source signature has ${sourceParams.length}",
        )
      sourceParams.zip(callableParams).zipWithIndex.foreach { case ((expected, actual), index) =>
        if !sameType(expected, actual.valueType) then
          error(
            "cosmo0.lir.invalid-signature",
            s"${function.id} parameter $index has type ${actual.valueType.display}, expected ${expected.display}",
          )
      }
      val expectedReturn = LirTypeRef(source.returnType)
      if !sameType(expectedReturn, function.returnType) then
        error(
          "cosmo0.lir.invalid-signature",
          s"${function.id} returns ${function.returnType.display}, source signature returns ${expectedReturn.display}",
        )

    private def checkReceiverParam(
        function: LirFunction,
        param: LirParam,
        receiver: CallableReceiver,
    ): Unit =
      SourceType.dealias(param.valueType.source) match
        case SourceType.Ref(target, mutable) =>
          if !sameSourceType(target, receiver.valueType) || (receiver.mutable && !mutable) then
            error(
              "cosmo0.lir.invalid-signature",
              s"${function.id} receiver parameter has type ${param.valueType.display}, expected receiver ${receiver.valueType.display}",
            )
        case other =>
          if !sameSourceType(other, receiver.valueType) then
            error(
              "cosmo0.lir.invalid-signature",
              s"${function.id} receiver parameter has type ${param.valueType.display}, expected receiver ${receiver.valueType.display}",
            )

    private def checkExternFunction(function: LirFunction): Unit =
      val binding = function.externBinding.get
      if !TrustedExternAbi.isSupportedAbiName(binding.abi) then
        error(
          "cosmo0.lir.invalid-extern-binding",
          s"${function.id} uses unsupported extern ABI ${binding.abi}",
        )
      if function.locals.nonEmpty then
        error(
          "cosmo0.lir.invalid-extern-binding",
          s"${function.id} extern binding cannot declare local storage",
        )
      if function.blocks.nonEmpty then
        error(
          "cosmo0.lir.invalid-extern-binding",
          s"${function.id} extern binding cannot also define LIR blocks",
        )
      binding.requirements
        .map(_.legacyName)
        .groupBy(identity)
        .toList
        .collect { case (requirement, values) if values.length > 1 => requirement }
        .sorted
        .foreach { requirement =>
          error(
            "cosmo0.lir.invalid-extern-binding",
            s"${function.id} extern binding repeats backend requirement $requirement",
          )
        }

    private def buildLocalEnv(function: LirFunction): Map[LirLocalId, LocalInfo] =
      val params = function.params.map { param =>
        LocalInfo(param.id, param.name, param.valueType, mutable = false, param.mutationAllowed, param = true)
      }
      val locals = function.locals.map { local =>
        LocalInfo(local.id, local.name, local.valueType, local.mutable, local.mutationAllowed, param = false)
      }
      (params ++ locals)
        .groupBy(_.id)
        .toList
        .sortBy(_._1.value)
        .foreach { case (id, values) =>
          if values.length > 1 then
            error(
              "cosmo0.lir.duplicate-local",
              s"${function.id} declares duplicate local $id",
            )
        }
      firstBy(params ++ locals)(_.id)

    private def buildBlockEnv(function: LirFunction): Map[LirLabel, LirBlock] =
      function.blocks
        .groupBy(_.label)
        .toList
        .sortBy(_._1.value)
        .foreach { case (label, blocks) =>
          if blocks.length > 1 then
            error(
              "cosmo0.lir.duplicate-label",
              s"${function.id} declares duplicate block label $label",
            )
        }
      firstBy(function.blocks)(_.label)

    private def checkBranchTargets(env: FunctionEnv): Unit =
      env.blocks.values.toList.sortBy(_.label.value).foreach { block =>
        branchTargets(block.terminator).foreach { target =>
          if !env.blocks.contains(target) then
            error(
              "cosmo0.lir.invalid-branch",
              s"${env.function.id} block ${block.label} branches to unknown target $target",
            )
        }
      }

    private def computeDefinedInputs(env: FunctionEnv): Map[LirLabel, Set[LirLocalId]] =
      val initial = env.locals.values.filter(_.param).map(_.id).toSet
      val inputs = mutable.LinkedHashMap.empty[LirLabel, Set[LirLocalId]]
      val outputs = mutable.LinkedHashMap.empty[LirLabel, Set[LirLocalId]]
      val queue = mutable.Queue.empty[LirLabel]

      inputs(env.entry) = initial
      queue.enqueue(env.entry)

      while queue.nonEmpty do
        val label = queue.dequeue()
        val block = env.blocks(label)
        val nextOut = transferDefinitions(block, inputs(label), env)
        val changedOut = outputs.get(label).forall(_ != nextOut)
        outputs(label) = nextOut
        if changedOut then
          successors(block, env.blocks.keySet).foreach { successor =>
            val merged = inputs.get(successor).fold(nextOut)(_ intersect nextOut)
            if inputs.get(successor).forall(_ != merged) then
              inputs(successor) = merged
              queue.enqueue(successor)
          }

      inputs.toMap

    private def transferDefinitions(
        block: LirBlock,
        initial: Set[LirLocalId],
        env: FunctionEnv,
    ): Set[LirLocalId] =
      block.operations.foldLeft(initial) { (defined, op) =>
        op match
          case LirAllocLocal(local, _) =>
            if env.locals.contains(local.id) then defined + local.id else defined
          case LirAssign(LirLocalPlace(id, _), _) =>
            if env.locals.contains(id) then defined + id else defined
          case LirAssign(_, _) =>
            defined
          case LirFieldGet(output, _, _, _) =>
            defineKnown(output, defined, env)
          case LirFieldSet(_, _, _) =>
            defined
          case LirDirectCall(output, _, _, _) =>
            output.fold(defined)(defineKnown(_, defined, env))
          case LirLoweredMethodCall(output, _, _, _, _) =>
            output.fold(defined)(defineKnown(_, defined, env))
          case LirDescriptorIntrinsic(output, _, _, _, _) =>
            output.fold(defined)(defineKnown(_, defined, env))
          case LirConstructType(output, _, _) =>
            defineKnown(output, defined, env)
          case LirConstructVariant(output, _, _, _) =>
            defineKnown(output, defined, env)
          case LirReadVariantTag(output, _, _) =>
            defineKnown(output, defined, env)
          case LirCheckVariantTag(output, _, _, _) =>
            defineKnown(output, defined, env)
          case LirReadVariantPayload(output, _, _, _, _) =>
            defineKnown(output, defined, env)
        }

    private def defineKnown(
        id: LirLocalId,
        defined: Set[LirLocalId],
        env: FunctionEnv,
    ): Set[LirLocalId] =
      if env.locals.contains(id) then defined + id else defined

    private def checkReachableBlock(
        block: LirBlock,
        env: FunctionEnv,
        initiallyDefined: Set[LirLocalId],
    ): Unit =
      val finalDefined = block.operations.foldLeft(initiallyDefined) { (defined, op) =>
        checkOp(op, env, defined)
      }
      checkTerminator(block, env, finalDefined)

    private def checkOp(
        op: LirOp,
        env: FunctionEnv,
        defined: Set[LirLocalId],
    ): Set[LirLocalId] =
      op match
        case LirAllocLocal(local, init) =>
          val localInfo = env.locals.get(local.id)
          localInfo match
            case Some(info) =>
              if info.param then
                error(
                  "cosmo0.lir.invalid-local",
                  s"${env.function.id} cannot allocate parameter ${local.id}",
                )
              if info.name != local.name ||
                  !sameType(info.valueType, local.valueType) ||
                  info.mutable != local.mutable ||
                  info.mutationAllowed != local.mutationAllowed
              then
                error(
                  "cosmo0.lir.invalid-local",
                  s"${env.function.id} allocates ${local.id} with metadata that does not match the function local declaration",
                )
              if defined.contains(local.id) && !info.mutable then
                error(
                  "cosmo0.lir.invalid-mutability",
                  s"${env.function.id} cannot redefine immutable local ${local.id}",
                )
            case None =>
              error(
                "cosmo0.lir.unknown-local",
                s"${env.function.id} allocates undeclared local ${local.id}",
              )

          init.foreach { value =>
            checkValue(value, env, defined)
            if !assignable(value.valueType, local.valueType) then
              error(
                "cosmo0.lir.assignment-mismatch",
                s"${env.function.id} initializes ${local.id} with ${value.valueType.display}, expected ${local.valueType.display}",
              )
          }
          if localInfo.nonEmpty then defined + local.id else defined

        case LirAssign(target, value) =>
          checkValue(value, env, defined)
          val targetInfo = checkPlace(target, env, defined)
          targetInfo.foreach { place =>
            if !place.mutable then
              error(
                "cosmo0.lir.invalid-mutability",
                s"${env.function.id} cannot assign to immutable ${place.description}",
              )
            if !assignable(value.valueType, place.valueType) then
              error(
                "cosmo0.lir.assignment-mismatch",
                s"${env.function.id} assigns ${value.valueType.display} to ${place.description}: ${place.valueType.display}",
              )
          }
          targetInfo.flatMap(_.local).fold(defined)(id => defined + id)

        case LirFieldGet(output, receiver, field, valueType) =>
          checkValue(receiver, env, defined)
          fieldInfo(receiver.valueType, field, env).foreach { fieldInfo =>
            if !sameType(fieldInfo.valueType, valueType) then
              error(
                "cosmo0.lir.assignment-mismatch",
                s"${env.function.id} reads field $field as ${valueType.display}, expected ${fieldInfo.valueType.display}",
              )
          }
          defineOutput(output, valueType, env, defined, s"field_get $field")

        case LirFieldSet(receiver, field, value) =>
          checkValue(receiver, env, defined)
          checkValue(value, env, defined)
          fieldInfo(receiver.valueType, field, env).foreach { fieldInfo =>
            if !fieldInfo.mutable || !mutableValue(receiver, env) then
              error(
                "cosmo0.lir.invalid-mutability",
                s"${env.function.id} cannot set immutable field $field through ${valueDescription(receiver)}",
              )
            if !assignable(value.valueType, fieldInfo.valueType) then
              error(
                "cosmo0.lir.assignment-mismatch",
                s"${env.function.id} sets field $field with ${value.valueType.display}, expected ${fieldInfo.valueType.display}",
              )
          }
          defined

        case LirDirectCall(output, callee, args, signature) =>
          args.foreach(checkValue(_, env, defined))
          env.declarations.functions.get(callee) match
            case Some(function) =>
              if !sameSignature(signature, function.signature) then
                error(
                  "cosmo0.lir.invalid-call",
                  s"${env.function.id} call to $callee uses signature ${signatureDisplay(signature)}, expected ${signatureDisplay(function.signature)}",
                )
            case None =>
              error(
                "cosmo0.lir.unknown-function",
                s"${env.function.id} calls unknown function $callee",
              )
          checkInvocationArgs(env.function.id, s"call $callee", args, signature.params)
          defineCallOutput(output, signature.returnType, env, defined, s"call $callee")

        case LirLoweredMethodCall(output, receiver, method, args, signature) =>
          checkValue(receiver, env, defined)
          args.foreach(checkValue(_, env, defined))
          expectedMethodSignature(receiver, method, env).foreach { expected =>
            if expected.receiverMutable && !mutableValue(receiver, env) then
              error(
                "cosmo0.lir.invalid-mutability",
                s"${env.function.id} method $method requires a mutable receiver",
              )
            if !sameSignature(signature, expected.signature) then
              error(
                "cosmo0.lir.invalid-call",
                s"${env.function.id} method $method uses signature ${signatureDisplay(signature)}, expected ${signatureDisplay(expected.signature)}",
              )
          }
          signature.sourceSignature.flatMap(_.receiver).foreach { receiverInfo =>
            if receiverInfo.mutable && !mutableValue(receiver, env) then
              error(
                "cosmo0.lir.invalid-mutability",
                s"${env.function.id} method $method requires a mutable receiver",
              )
          }
          checkInvocationArgs(env.function.id, s"method $method", args, signature.params)
          defineCallOutput(output, signature.returnType, env, defined, s"method $method")

        case LirDescriptorIntrinsic(output, descriptor, name, args, resultType) =>
          args.foreach(checkValue(_, env, defined))
          expectedDescriptorOperation(descriptor, name) match
            case Some(expected) =>
              if expected.receiverMutable then
                args.headOption.foreach { receiver =>
                  if !mutableValue(receiver, env) then
                    error(
                      "cosmo0.lir.invalid-mutability",
                      s"${env.function.id} descriptor ${descriptor.name}::$name requires a mutable receiver",
                    )
                }
              checkInvocationArgs(env.function.id, s"descriptor ${descriptor.name}::$name", args, expected.signature.params)
              val actualResult = resultType.getOrElse(LirTypeRef(SourceType.Unit))
              if !sameType(actualResult, expected.signature.returnType) then
                error(
                  "cosmo0.lir.invalid-descriptor",
                  s"${env.function.id} descriptor ${descriptor.name}::$name reports ${actualResult.display}, expected ${expected.signature.returnType.display}",
                )
              defineCallOutput(output, expected.signature.returnType, env, defined, s"descriptor ${descriptor.name}::$name")
            case None =>
              defined

        case LirConstructType(output, owner, fields) =>
          fields.foreach(field => checkValue(field.value, env, defined))
          typeDeclFor(owner.source, env.declarations) match
            case Some(ty) =>
              val fieldsByName = ty.fields.map(field => field.name -> field).toMap
              val providedNames = fields.map(_.name)
              providedNames.groupBy(identity).toList.sortBy(_._1).foreach { case (name, values) =>
                if values.length > 1 then
                  error(
                    "cosmo0.lir.invalid-call",
                    s"${env.function.id} constructor for ${owner.display} initializes field $name more than once",
                  )
              }
              ty.fields.foreach { field =>
                if !providedNames.contains(field.name) then
                  error(
                    "cosmo0.lir.invalid-call",
                    s"${env.function.id} constructor for ${owner.display} is missing field ${field.name}",
                  )
              }
              fields.foreach { init =>
                fieldsByName.get(init.name) match
                  case Some(field) =>
                    if !assignable(init.value.valueType, field.valueType) then
                      error(
                        "cosmo0.lir.assignment-mismatch",
                        s"${env.function.id} constructor field ${owner.display}.${init.name} has type ${init.value.valueType.display}, expected ${field.valueType.display}",
                      )
                  case None =>
                    error(
                      "cosmo0.lir.unknown-field",
                      s"${env.function.id} constructor for ${owner.display} uses unknown field ${init.name}",
                    )
              }
            case None =>
              error(
                "cosmo0.lir.unknown-type",
                s"no LIR type declaration for constructor owner ${owner.display}",
              )
          defineOutput(output, owner, env, defined, s"construct ${owner.display}")

        case LirConstructVariant(output, owner, variant, payload) =>
          payload.foreach(checkValue(_, env, defined))
          variantShape(owner, variant, env).foreach { shape =>
            checkInvocationArgs(env.function.id, s"variant ${owner.display}::$variant", payload, shape.payload)
          }
          defineOutput(output, owner, env, defined, s"variant ${owner.display}::$variant")

        case LirReadVariantTag(output, scrutinee, owner) =>
          checkValue(scrutinee, env, defined)
          if !sameSourceType(SourceType.deref(scrutinee.valueType.source), owner.source) then
            error(
              "cosmo0.lir.assignment-mismatch",
              s"${env.function.id} reads variant tag for ${owner.display} from ${scrutinee.valueType.display}",
            )
          if variantNames(owner, env).isEmpty then
            error(
              "cosmo0.lir.invalid-variant",
              s"${env.function.id} reads variant tag from non-variant type ${owner.display}",
            )
          defineOutput(output, LirTypeRef(SourceType.I32), env, defined, s"variant_tag ${owner.display}")

        case LirCheckVariantTag(output, scrutinee, owner, variant) =>
          checkValue(scrutinee, env, defined)
          if !sameSourceType(SourceType.deref(scrutinee.valueType.source), owner.source) then
            error(
              "cosmo0.lir.assignment-mismatch",
              s"${env.function.id} checks variant tag for ${owner.display} from ${scrutinee.valueType.display}",
            )
          if !variantNames(owner, env).contains(variant) then
            error(
              "cosmo0.lir.invalid-variant",
              s"${env.function.id} checks missing variant ${owner.display}::$variant",
            )
          defineOutput(output, LirTypeRef(SourceType.Bool), env, defined, s"variant_is ${owner.display}::$variant")

        case LirReadVariantPayload(output, scrutinee, variant, index, valueType) =>
          checkValue(scrutinee, env, defined)
          val owner = LirTypeRef(SourceType.deref(scrutinee.valueType.source))
          variantShape(owner, variant, env).foreach { shape =>
            if index < 0 || index >= shape.payload.length then
              error(
                "cosmo0.lir.invalid-variant",
                s"${env.function.id} reads invalid payload index $index from ${owner.display}::$variant",
              )
            else if !sameType(valueType, shape.payload(index)) then
              error(
                "cosmo0.lir.assignment-mismatch",
                s"${env.function.id} reads ${owner.display}::$variant[$index] as ${valueType.display}, expected ${shape.payload(index).display}",
              )
          }
          defineOutput(output, valueType, env, defined, s"variant_payload $variant[$index]")

    private final case class ExpectedCallable(
        signature: LirCallableSignature,
        receiverMutable: Boolean,
    )

    private def expectedMethodSignature(
        receiver: LirValue,
        method: String,
        env: FunctionEnv,
    ): Option[ExpectedCallable] =
      SourceType.dealias(SourceType.deref(receiver.valueType.source)) match
        case owner: SourceType.Standard =>
          descriptorMethod(owner, method, receiver.valueType)
        case owner: SourceType.Builtin =>
          descriptorMethod(owner, method, receiver.valueType)
        case SourceType.User(name) =>
          env.declarations.typesByName.get(name).flatMap { ty =>
            env.declarations.functions.values
              .find(function => function.owner.contains(ty.id) && function.name == method)
              .map(function => ExpectedCallable(methodCallSignature(function), methodReceiverMutable(function)))
          }
        case _ =>
          None

    private def methodCallSignature(function: LirFunction): LirCallableSignature =
      val dropReceiver =
        function.sourceSignature.exists(_.receiver.nonEmpty) &&
          function.params.headOption.exists(_.name == "self")
      LirCallableSignature(
        function.params.drop(if dropReceiver then 1 else 0).map(_.valueType),
        function.returnType,
        function.sourceSignature,
      )

    private def methodReceiverMutable(function: LirFunction): Boolean =
      function.sourceSignature.flatMap(_.receiver).exists(_.mutable)

    private def expectedDescriptorOperation(
        descriptor: LirDescriptorRef,
        name: String,
    ): Option[ExpectedCallable] =
      descriptors.get(descriptor.name) match
        case None =>
          error(
            "cosmo0.lir.invalid-descriptor",
            s"unknown descriptor ${descriptor.name}",
          )
          None
        case Some(info) if info.arity != descriptor.args.length =>
          error(
            "cosmo0.lir.invalid-descriptor",
            s"descriptor ${descriptor.name} expects ${info.arity} type argument(s), got ${descriptor.args.length}",
          )
          None
        case Some(info) =>
          val owner =
            descriptorOwnerType(descriptor)
          syntheticDescriptorOperation(descriptor, name).orElse(info.method(name) match
            case Some(callable) =>
              val signature = callable.instantiate(owner, syntheticSpan)
              val params = LirTypeRef(owner) :: signature.params.map(param => LirTypeRef(param.valueType))
              Some(
                ExpectedCallable(
                  LirCallableSignature(params, LirTypeRef(signature.returnType), Some(signature)),
                  signature.receiver.exists(_.mutable),
                ),
              )
            case None =>
              info.constructor(name) match
                case Some(callable) =>
                  val signature = callable.instantiate(owner, syntheticSpan)
                  Some(
                    ExpectedCallable(
                      LirCallableSignature.fromSource(signature),
                      receiverMutable = false,
                    ),
                  )
                case None =>
                  error(
                    "cosmo0.lir.invalid-descriptor",
                    s"descriptor ${descriptor.name} has no operation $name",
                  )
                  None
          )

    private def syntheticDescriptorOperation(
        descriptor: LirDescriptorRef,
        name: String,
    ): Option[ExpectedCallable] =
      descriptor.name match
        case "Vec" | "Set" | "Arena" if descriptor.args.length == 1 =>
          val owner = LirTypeRef(descriptorOwnerType(descriptor))
          val itemType = descriptor.args.head
          name match
            case "iter_has_next" =>
              Some(
                ExpectedCallable(
                  LirCallableSignature(List(owner), LirTypeRef(SourceType.Bool)),
                  receiverMutable = false,
                ),
              )
            case "iter_next" =>
              Some(
                ExpectedCallable(
                  LirCallableSignature(List(owner), itemType),
                  receiverMutable = false,
                ),
              )
            case _ =>
              None
        case "Map" if descriptor.args.length == 2 =>
          val owner = LirTypeRef(descriptorOwnerType(descriptor))
          val keyType = descriptor.args.head
          name match
            case "iter_has_next" =>
              Some(
                ExpectedCallable(
                  LirCallableSignature(List(owner), LirTypeRef(SourceType.Bool)),
                  receiverMutable = false,
                ),
              )
            case "iter_next" =>
              Some(
                ExpectedCallable(
                  LirCallableSignature(List(owner), keyType),
                  receiverMutable = false,
                ),
              )
            case _ =>
              None
        case _ =>
          None

    private def descriptorMethod(
        owner: SourceType,
        method: String,
        receiverType: LirTypeRef,
    ): Option[ExpectedCallable] =
      descriptorName(owner).flatMap(name => descriptors.get(name).flatMap(_.method(method))).map { callable =>
        val signature = callable.instantiate(owner, syntheticSpan)
        ExpectedCallable(
          LirCallableSignature(signature.params.map(param => LirTypeRef(param.valueType)), LirTypeRef(signature.returnType), Some(signature)),
          signature.receiver.exists(_.mutable),
        )
      }

    private def checkTerminator(
        block: LirBlock,
        env: FunctionEnv,
        defined: Set[LirLocalId],
    ): Unit =
      block.terminator match
        case LirReturn(value) =>
          value.foreach(checkValue(_, env, defined))
          (value, env.function.returnType.source) match
            case (None, SourceType.Unit) =>
            case (None, _) =>
              error(
                "cosmo0.lir.invalid-return",
                s"${env.function.id} block ${block.label} returns no value, expected ${env.function.returnType.display}",
              )
            case (Some(actual), expected) if !assignable(actual.valueType.source, expected) =>
              error(
                "cosmo0.lir.invalid-return",
                s"${env.function.id} block ${block.label} returns ${actual.valueType.display}, expected ${env.function.returnType.display}",
              )
            case _ =>

        case LirBranch(_) =>

        case LirCondBranch(condition, _, _) =>
          checkValue(condition, env, defined)
          if !sameSourceType(condition.valueType.source, SourceType.Bool) then
            error(
              "cosmo0.lir.invalid-branch",
              s"${env.function.id} block ${block.label} branches on ${condition.valueType.display}, expected Bool",
            )

        case LirUnreachable(_) =>

        case LirErrorExit(_, _) =>

    private def checkPlace(
        place: LirPlace,
        env: FunctionEnv,
        defined: Set[LirLocalId],
    ): Option[PlaceInfo] =
      place match
        case LirLocalPlace(id, valueType) =>
          env.locals.get(id) match
            case Some(local) =>
              if !sameType(valueType, local.valueType) then
                error(
                  "cosmo0.lir.assignment-mismatch",
                  s"${env.function.id} local place $id has type ${valueType.display}, expected ${local.valueType.display}",
                )
              Some(PlaceInfo(local.valueType, local.mutable, Some(id), id.toString))
            case None =>
              error(
                "cosmo0.lir.unknown-local",
                s"${env.function.id} references undeclared local $id",
              )
              None

        case LirFieldPlace(receiver, field, valueType) =>
          checkValue(receiver, env, defined)
          fieldInfo(receiver.valueType, field, env).map { fieldInfo =>
            if !sameType(valueType, fieldInfo.valueType) then
              error(
                "cosmo0.lir.assignment-mismatch",
                s"${env.function.id} field place $field has type ${valueType.display}, expected ${fieldInfo.valueType.display}",
              )
            PlaceInfo(
              fieldInfo.valueType,
              fieldInfo.mutable && mutableValue(receiver, env),
              None,
              s"${valueDescription(receiver)}.$field",
            )
          }

    private def checkValue(
        value: LirValue,
        env: FunctionEnv,
        defined: Set[LirLocalId],
    ): Unit =
      value match
        case LirUnitValue =>

        case LirBoolValue(_) =>

        case LirIntValue(_, valueType) =>
          if !SourceType.isInteger(valueType.source) then
            error(
              "cosmo0.lir.assignment-mismatch",
              s"integer literal is annotated as ${valueType.display}",
            )

        case LirFloatValue(_, valueType) =>
          if !SourceType.isFloat(valueType.source) then
            error(
              "cosmo0.lir.assignment-mismatch",
              s"float literal is annotated as ${valueType.display}",
            )

        case LirStringValue(_) =>

        case LirCharValue(_) =>

        case LirLocalRef(id, valueType) =>
          env.locals.get(id) match
            case Some(local) =>
              if !sameType(valueType, local.valueType) then
                error(
                  "cosmo0.lir.assignment-mismatch",
                  s"${env.function.id} local reference $id has type ${valueType.display}, expected ${local.valueType.display}",
                )
              if !defined.contains(id) then
                error(
                  "cosmo0.lir.use-before-definition",
                  s"${env.function.id} reads $id before it is defined",
                )
            case None =>
              error(
                "cosmo0.lir.unknown-local",
                s"${env.function.id} references undeclared local $id",
              )

        case LirGlobalRef(id, valueType) =>
          env.declarations.globals.get(id) match
            case Some(global) =>
              if !sameType(valueType, global.valueType) then
                error(
                  "cosmo0.lir.assignment-mismatch",
                  s"${env.function.id} global reference $id has type ${valueType.display}, expected ${global.valueType.display}",
                )
            case None =>
              error(
                "cosmo0.lir.unknown-global",
                s"${env.function.id} references unknown global $id",
              )

        case LirFunctionRef(id, signature) =>
          env.declarations.functions.get(id) match
            case Some(function) =>
              if !sameSignature(signature, function.signature) then
                error(
                  "cosmo0.lir.invalid-call",
                  s"${env.function.id} function reference $id has signature ${signatureDisplay(signature)}, expected ${signatureDisplay(function.signature)}",
                )
            case None =>
              error(
                "cosmo0.lir.unknown-function",
                s"${env.function.id} references unknown function $id",
              )

        case LirBorrowValue(inner, mutable) =>
          checkValue(inner, env, defined)
          if !borrowableValue(inner) then
            error(
              "cosmo0.lir.invalid-borrow",
              s"${env.function.id} cannot borrow non-place ${inner.valueType.display}",
            )
          if mutable && !mutableValue(inner, env) then
            error(
              "cosmo0.lir.invalid-mutability",
              s"${env.function.id} cannot take mutable reference to ${valueDescription(inner)}",
            )

        case LirDerefValue(inner, valueType) =>
          checkValue(inner, env, defined)
          SourceType.dealias(inner.valueType.source) match
            case SourceType.Ref(target, _) =>
              if !sameSourceType(target, valueType.source) then
                error(
                  "cosmo0.lir.assignment-mismatch",
                  s"${env.function.id} dereferences ${inner.valueType.display} as ${valueType.display}",
                )
            case other =>
              error(
                "cosmo0.lir.assignment-mismatch",
                s"${env.function.id} cannot dereference non-reference value ${other.display}",
              )

        case LirFieldRef(receiver, field, valueType) =>
          checkValue(receiver, env, defined)
          fieldInfo(receiver.valueType, field, env).foreach { fieldInfo =>
            if !sameType(valueType, fieldInfo.valueType) then
              error(
                "cosmo0.lir.assignment-mismatch",
                s"${env.function.id} field reference $field has type ${valueType.display}, expected ${fieldInfo.valueType.display}",
              )
          }

    private def checkModuleValue(value: LirValue, declarations: DeclEnv): Unit =
      value match
        case LirGlobalRef(id, valueType) =>
          declarations.globals.get(id).foreach { global =>
            if !sameType(valueType, global.valueType) then
              error(
                "cosmo0.lir.assignment-mismatch",
                s"global reference $id has type ${valueType.display}, expected ${global.valueType.display}",
              )
          }
        case LirFunctionRef(id, signature) =>
          declarations.functions.get(id).foreach { function =>
            if !sameSignature(signature, function.signature) then
              error(
                "cosmo0.lir.invalid-call",
                s"function reference $id has signature ${signatureDisplay(signature)}, expected ${signatureDisplay(function.signature)}",
              )
          }
        case LirBorrowValue(inner, _) =>
          checkModuleValue(inner, declarations)
        case LirDerefValue(inner, _) =>
          checkModuleValue(inner, declarations)
        case LirFieldRef(receiver, _, _) =>
          checkModuleValue(receiver, declarations)
        case _ =>

    private def defineOutput(
        output: LirLocalId,
        expectedType: LirTypeRef,
        env: FunctionEnv,
        defined: Set[LirLocalId],
        construct: String,
    ): Set[LirLocalId] =
      env.locals.get(output) match
        case Some(local) =>
          if local.param then
            error(
              "cosmo0.lir.invalid-output",
              s"${env.function.id} writes $construct result to parameter $output",
            )
          if !sameType(local.valueType, expectedType) then
            error(
              "cosmo0.lir.invalid-output",
              s"${env.function.id} writes $construct result ${expectedType.display} to $output: ${local.valueType.display}",
            )
          if defined.contains(output) && !local.mutable then
            error(
              "cosmo0.lir.invalid-mutability",
              s"${env.function.id} cannot redefine immutable local $output with $construct",
            )
          defined + output
        case None =>
          error(
            "cosmo0.lir.unknown-local",
            s"${env.function.id} writes $construct result to undeclared local $output",
          )
          defined

    private def defineCallOutput(
        output: Option[LirLocalId],
        resultType: LirTypeRef,
        env: FunctionEnv,
        defined: Set[LirLocalId],
        construct: String,
    ): Set[LirLocalId] =
      output match
        case Some(id) =>
          defineOutput(id, resultType, env, defined, construct)
        case None if !sameSourceType(resultType.source, SourceType.Unit) && !sameSourceType(resultType.source, SourceType.Never) =>
          error(
            "cosmo0.lir.invalid-output",
            s"${env.function.id} drops non-Unit result ${resultType.display} from $construct",
          )
          defined
        case None =>
          defined

    private def checkInvocationArgs(
        functionId: LirDeclId,
        construct: String,
        args: List[LirValue],
        params: List[LirTypeRef],
    ): Unit =
      if args.length != params.length then
        error(
          "cosmo0.lir.invalid-call",
          s"$functionId $construct expects ${params.length} argument(s), got ${args.length}",
        )
      args.zip(params).zipWithIndex.foreach { case ((arg, param), index) =>
        if !assignable(arg.valueType, param) then
          error(
            "cosmo0.lir.assignment-mismatch",
            s"$functionId $construct argument $index has type ${arg.valueType.display}, expected ${param.display}",
          )
      }

    private def fieldInfo(
        receiverType: LirTypeRef,
        field: String,
        env: FunctionEnv,
    ): Option[LirField] =
      typeDeclFor(receiverType.source, env.declarations) match
        case Some(ty) =>
          ty.fields.find(_.name == field) match
            case Some(value) => Some(value)
            case None =>
              error(
                "cosmo0.lir.unknown-field",
                s"${ty.id} has no field $field",
              )
              None
        case None =>
          error(
            "cosmo0.lir.unknown-type",
            s"no LIR type declaration for ${receiverType.display}",
          )
          None

    private def variantShape(
        owner: LirTypeRef,
        variant: String,
        env: FunctionEnv,
    ): Option[VariantShape] =
      SourceType.dealias(owner.source) match
        case standard: SourceType.Standard =>
          descriptors.get(standard.name) match
            case Some(descriptor) if descriptor.arity == standard.args.length =>
              descriptor.constructor(variant) match
                case Some(callable) =>
                  val signature = callable.instantiate(standard, syntheticSpan)
                  if !sameSourceType(signature.returnType, standard) then
                    error(
                      "cosmo0.lir.invalid-variant",
                      s"${owner.display}::$variant returns ${signature.returnType.display}, expected ${owner.display}",
                    )
                  Some(VariantShape(owner, variant, signature.params.map(param => LirTypeRef(param.valueType))))
                case None =>
                  error(
                    "cosmo0.lir.invalid-variant",
                    s"${owner.display} has no variant $variant",
                  )
                  None
            case Some(descriptor) =>
              error(
                "cosmo0.lir.invalid-descriptor",
                s"${standard.name} expects ${descriptor.arity} type argument(s), got ${standard.args.length}",
              )
              None
            case None =>
              error(
                "cosmo0.lir.unknown-type",
                s"unknown descriptor-backed variant owner ${standard.name}",
              )
              None

        case _ =>
          typeDeclFor(owner.source, env.declarations) match
            case Some(ty) =>
              ty.variants.find(_.name == variant) match
                case Some(value) =>
                  Some(VariantShape(owner, variant, value.payload.map(_.valueType)))
                case None =>
                  error(
                    "cosmo0.lir.invalid-variant",
                    s"${ty.id} has no variant $variant",
                  )
                  None
            case None =>
              error(
                "cosmo0.lir.unknown-type",
                s"no LIR type declaration for variant owner ${owner.display}",
              )
              None

    private def variantNames(owner: LirTypeRef, env: FunctionEnv): Set[String] =
      SourceType.dealias(owner.source) match
        case standard: SourceType.Standard =>
          descriptors.get(standard.name).filter(_.arity == standard.args.length).fold(Set.empty[String])(_.constructors.keySet)
        case _ =>
          typeDeclFor(owner.source, env.declarations).fold(Set.empty[String])(_.variants.map(_.name).toSet)

    private def typeDeclFor(
        sourceType: SourceType,
        declarations: DeclEnv,
    ): Option[LirTypeDecl] =
      SourceType.dealias(SourceType.deref(sourceType)) match
        case SourceType.User(name) =>
          declarations.typesByName.get(name)
        case _ =>
          None

    private def descriptorOwnerType(descriptor: LirDescriptorRef): SourceType =
      SourceType.scalar(descriptor.name).filter(_ => descriptor.args.isEmpty).getOrElse {
        SourceType.Standard(descriptor.name, descriptor.args.map(_.source))
      }

    private def descriptorName(ownerType: SourceType): Option[String] =
      SourceType.dealias(ownerType) match
        case SourceType.Standard(name, _) => Some(name)
        case SourceType.Builtin(name)     => Some(name)
        case _                            => None

    private def mutableValue(value: LirValue, env: FunctionEnv): Boolean =
      SourceType.refMutable(value.valueType.source).contains(true) ||
        (value match
          case LirLocalRef(id, _) =>
            env.locals.get(id).exists(info => info.mutable || info.mutationAllowed)
          case LirGlobalRef(id, _) =>
            env.declarations.globals.get(id).exists(global => global.mutable || global.mutationAllowed)
          case LirDerefValue(inner, _) =>
            SourceType.refMutable(inner.valueType.source).contains(true)
          case LirFieldRef(receiver, field, _) =>
            fieldInfo(receiver.valueType, field, env).exists(_ => mutableValue(receiver, env))
          case _                   => false
        )

    private def borrowableValue(value: LirValue): Boolean =
      value match
        case _: LirLocalRef              => true
        case _: LirGlobalRef             => true
        case _: LirDerefValue            => true
        case LirFieldRef(receiver, _, _) => borrowableValue(receiver)
        case _                           => false

    private def successors(
        block: LirBlock,
        knownLabels: Set[LirLabel],
    ): List[LirLabel] =
      branchTargets(block.terminator).filter(knownLabels.contains)

    private def branchTargets(terminator: LirTerminator): List[LirLabel] =
      terminator match
        case LirBranch(target) =>
          List(target)
        case LirCondBranch(_, trueTarget, falseTarget) =>
          List(trueTarget, falseTarget)
        case _ =>
          Nil

    private def firstBy[A, K](values: List[A])(key: A => K): Map[K, A] =
      values.foldLeft(Map.empty[K, A]) { (acc, value) =>
        val valueKey = key(value)
        if acc.contains(valueKey) then acc else acc.updated(valueKey, value)
      }

    private def sameSignature(
        left: LirCallableSignature,
        right: LirCallableSignature,
    ): Boolean =
      left.params.length == right.params.length &&
        left.params.zip(right.params).forall { case (a, b) => sameType(a, b) } &&
        sameType(left.returnType, right.returnType)

    private def sameType(left: LirTypeRef, right: LirTypeRef): Boolean =
      sameSourceType(left.source, right.source)

    private def sameSourceType(left: SourceType, right: SourceType): Boolean =
      SourceType.same(left, right)

    private def assignable(from: LirTypeRef, to: LirTypeRef): Boolean =
      assignable(from.source, to.source)

    private def assignable(from: SourceType, to: SourceType): Boolean =
      SourceType.assignable(from, to)

    private def valueDescription(value: LirValue): String =
      value match
        case LirLocalRef(id, _)    => id.toString
        case LirGlobalRef(id, _)   => id.toString
        case LirFunctionRef(id, _) => id.toString
        case LirBorrowValue(inner, true) => s"&mut ${valueDescription(inner)}"
        case LirBorrowValue(inner, false) => s"&${valueDescription(inner)}"
        case LirDerefValue(inner, _) => s"*${valueDescription(inner)}"
        case LirFieldRef(receiver, field, _) => s"${valueDescription(receiver)}.$field"
        case _                     => value.valueType.display

    private def signatureDisplay(signature: LirCallableSignature): String =
      s"(${signature.params.map(_.display).mkString(", ")}) -> ${signature.returnType.display}"

    private lazy val syntheticSpan: SourceSpan =
      SourceFile("<lir>", "").span(0, 0)

    private def error(code: String, message: String): Unit =
      errors += Diagnostic(
        Phase.Check,
        DiagnosticSeverity.Error,
        code,
        message,
      )
