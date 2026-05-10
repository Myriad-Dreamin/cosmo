package cosmo0

enum SourceType:
  case Builtin(name: String)
  case User(name: String)
  case Alias(name: String, target: SourceType)
  case Ref(target: SourceType, mutable: Boolean)
  case Standard(name: String, args: List[SourceType])
  case Function(params: List[SourceType], returnType: SourceType)
  case Never
  case Error

  def display: String =
    this match
      case SourceType.Builtin(name) => name
      case SourceType.User(name)    => name
      case SourceType.Alias(name, _) => name
      case SourceType.Ref(target, true) =>
        s"&mut ${target.display}"
      case SourceType.Ref(target, false) =>
        s"&${target.display}"
      case SourceType.Standard(name, args) =>
        s"$name[${args.map(_.display).mkString(", ")}]"
      case SourceType.Function(params, returnType) =>
        s"(${params.map(_.display).mkString(", ")}) -> ${returnType.display}"
      case SourceType.Never => "never"
      case SourceType.Error => "<error>"

object SourceType:
  val Unit: SourceType = Builtin("Unit")
  val Bool: SourceType = Builtin("Bool")
  val I32: SourceType = Builtin("i32")
  val Usize: SourceType = Builtin("usize")
  val String: SourceType = Builtin("String")
  val Char: SourceType = Builtin("Char")
  val Byte: SourceType = Builtin("u8")
  val F64: SourceType = Builtin("f64")

  val scalarTypes: Map[String, SourceType] =
    Map(
      "Unit" -> Unit,
      "Bool" -> Bool,
      "bool" -> Bool,
      "i8" -> Builtin("i8"),
      "i16" -> Builtin("i16"),
      "i32" -> I32,
      "i64" -> Builtin("i64"),
      "u8" -> Byte,
      "u16" -> Builtin("u16"),
      "u32" -> Builtin("u32"),
      "u64" -> Builtin("u64"),
      "usize" -> Usize,
      "Byte" -> Byte,
      "Char" -> Char,
      "String" -> String,
      "str" -> String,
      "f32" -> Builtin("f32"),
      "f64" -> F64,
    )

  def scalar(name: String): Option[SourceType] =
    scalarTypes.get(name)

  def dealias(value: SourceType): SourceType =
    value match
      case Alias(_, target) => dealias(target)
      case Ref(target, mutable) =>
        Ref(dealias(target), mutable)
      case Standard(name, args) =>
        Standard(name, args.map(dealias))
      case Function(params, returnType) =>
        Function(params.map(dealias), dealias(returnType))
      case other => other

  def same(left: SourceType, right: SourceType): Boolean =
    (dealias(left), dealias(right)) match
      case (Error, _) | (_, Error)     => true
      case (Never, _) | (_, Never)     => true
      case (Builtin(a), Builtin(b))    => a == b
      case (User(a), User(b))          => a == b
      case (Ref(a, ma), Ref(b, mb))    => ma == mb && same(a, b)
      case (Standard(na, aa), Standard(nb, ab)) =>
        na == nb && aa.length == ab.length && aa.zip(ab).forall { case (a, b) => same(a, b) }
      case (Function(ap, ar), Function(bp, br)) =>
        ap.length == bp.length && ap.zip(bp).forall { case (a, b) => same(a, b) } && same(ar, br)
      case _ => false

  def assignable(from: SourceType, to: SourceType): Boolean =
    same(from, to)

  def isInteger(value: SourceType): Boolean =
    dealias(value) match
      case Builtin("i8" | "i16" | "i32" | "i64" | "u8" | "u16" | "u32" | "u64" | "usize") =>
        true
      case _ => false

  def isFloat(value: SourceType): Boolean =
    dealias(value) match
      case Builtin("f32" | "f64") => true
      case _                      => false

  def isNumeric(value: SourceType): Boolean =
    isInteger(value) || isFloat(value)

  def deref(value: SourceType): SourceType =
    dealias(value) match
      case Ref(target, _) => target
      case other          => other

  def refMutable(value: SourceType): Option[Boolean] =
    dealias(value) match
      case Ref(_, mutable) => Some(mutable)
      case _               => None

sealed trait SourceTypeTemplate:
  def instantiate(args: List[SourceType]): SourceType

object SourceTypeTemplate:
  final case class Arg(index: Int) extends SourceTypeTemplate:
    def instantiate(args: List[SourceType]): SourceType =
      args.lift(index).getOrElse(SourceType.Error)

  final case class Concrete(value: SourceType) extends SourceTypeTemplate:
    def instantiate(args: List[SourceType]): SourceType = value

  final case class Ref(target: SourceTypeTemplate, mutable: Boolean) extends SourceTypeTemplate:
    def instantiate(args: List[SourceType]): SourceType =
      SourceType.Ref(target.instantiate(args), mutable)

  final case class Standard(name: String, values: List[SourceTypeTemplate])
      extends SourceTypeTemplate:
    def instantiate(args: List[SourceType]): SourceType =
      SourceType.Standard(name, values.map(_.instantiate(args)))

final case class CallableParam(
    name: String,
    valueType: SourceType,
    span: SourceSpan,
)

final case class CallableReceiver(
    valueType: SourceType,
    mutable: Boolean,
)

final case class CallableSignature(
    name: String,
    params: List[CallableParam],
    returnType: SourceType,
    receiver: Option[CallableReceiver] = None,
):
  def functionType: SourceType =
    SourceType.Function(params.map(_.valueType), returnType)

final case class DescriptorParam(
    name: String,
    valueType: SourceTypeTemplate,
)

final case class DescriptorCallable(
    name: String,
    params: List[DescriptorParam],
    returnType: SourceTypeTemplate,
    receiverMutable: Boolean = false,
):
  def instantiate(owner: SourceType.Standard, span: SourceSpan): CallableSignature =
    CallableSignature(
      name,
      params.map(param =>
        CallableParam(param.name, param.valueType.instantiate(owner.args), span),
      ),
      returnType.instantiate(owner.args),
      Some(CallableReceiver(owner, receiverMutable)),
    )

final case class StandardGenericDescriptor(
    name: String,
    arity: Int,
    constructors: Map[String, DescriptorCallable],
    methods: Map[String, DescriptorCallable],
):
  def constructor(name: String): Option[DescriptorCallable] =
    constructors.get(name)

  def method(name: String): Option[DescriptorCallable] =
    methods.get(name)

object StandardGenericDescriptors:
  private val T = SourceTypeTemplate.Arg(0)
  private val U = SourceTypeTemplate.Arg(1)
  private val UnitT = SourceTypeTemplate.Concrete(SourceType.Unit)
  private val BoolT = SourceTypeTemplate.Concrete(SourceType.Bool)
  private val UsizeT = SourceTypeTemplate.Concrete(SourceType.Usize)

  val all: Map[String, StandardGenericDescriptor] =
    List(
      descriptor(
        "Vec",
        1,
        constructors = List(call("<init>", Nil, SourceTypeTemplate.Standard("Vec", List(T)))),
        methods = List(
          call("push", List(param("value", T)), UnitT, receiverMutable = true),
          call("get", List(param("index", UsizeT)), T),
          call("set", List(param("index", UsizeT), param("value", T)), UnitT, receiverMutable = true),
          call("size", Nil, UsizeT),
          call("is_empty", Nil, BoolT),
        ),
      ),
      descriptor(
        "Option",
        1,
        constructors = List(
          call("Some", List(param("value", T)), SourceTypeTemplate.Standard("Option", List(T))),
          call("None", Nil, SourceTypeTemplate.Standard("Option", List(T))),
        ),
        methods = List(
          call("is_some", Nil, BoolT),
          call("is_none", Nil, BoolT),
        ),
      ),
      descriptor(
        "Result",
        2,
        constructors = List(
          call("Ok", List(param("value", T)), SourceTypeTemplate.Standard("Result", List(T, U))),
          call("Err", List(param("error", U)), SourceTypeTemplate.Standard("Result", List(T, U))),
        ),
        methods = List(
          call("is_ok", Nil, BoolT),
          call("is_err", Nil, BoolT),
        ),
      ),
      descriptor(
        "Arena",
        1,
        constructors = List(call("<init>", Nil, SourceTypeTemplate.Standard("Arena", List(T)))),
        methods = List(
          call(
            "alloc",
            List(param("value", T)),
            SourceTypeTemplate.Standard("Id", List(T)),
            receiverMutable = true,
          ),
          call(
            "get",
            List(param("id", SourceTypeTemplate.Standard("Id", List(T)))),
            SourceTypeTemplate.Ref(T, mutable = false),
          ),
        ),
      ),
      descriptor("Id", 1, constructors = Nil, methods = Nil),
      descriptor(
        "Map",
        2,
        constructors = List(call("<init>", Nil, SourceTypeTemplate.Standard("Map", List(T, U)))),
        methods = List(
          call("get", List(param("key", T)), SourceTypeTemplate.Standard("Option", List(U))),
          call(
            "insert",
            List(param("key", T), param("value", U)),
            SourceTypeTemplate.Standard("Option", List(U)),
            receiverMutable = true,
          ),
          call("contains", List(param("key", T)), BoolT),
          call("contains_key", List(param("key", T)), BoolT),
        ),
      ),
      descriptor(
        "Set",
        1,
        constructors = List(call("<init>", Nil, SourceTypeTemplate.Standard("Set", List(T)))),
        methods = List(
          call("insert", List(param("value", T)), UnitT, receiverMutable = true),
          call("contains", List(param("value", T)), BoolT),
        ),
      ),
      descriptor(
        "Ptr",
        1,
        constructors = List(
          call(
            "<init>",
            List(param("value", SourceTypeTemplate.Ref(T, false))),
            SourceTypeTemplate.Standard("Ptr", List(T)),
          ),
        ),
        methods = Nil,
      ),
      descriptor(
        "Box",
        1,
        constructors = List(
          call("<init>", List(param("value", T)), SourceTypeTemplate.Standard("Box", List(T))),
        ),
        methods = List(
          call("get", Nil, SourceTypeTemplate.Ref(T, mutable = false)),
        ),
      ),
      descriptor("Ref", 1, constructors = Nil, methods = Nil),
      descriptor("RefMut", 1, constructors = Nil, methods = Nil),
    ).map(descriptor => descriptor.name -> descriptor).toMap

  def get(name: String): Option[StandardGenericDescriptor] =
    all.get(name)

  private def descriptor(
      name: String,
      arity: Int,
      constructors: List[DescriptorCallable],
      methods: List[DescriptorCallable],
  ): StandardGenericDescriptor =
    StandardGenericDescriptor(
      name,
      arity,
      constructors.map(call => call.name -> call).toMap,
      methods.map(call => call.name -> call).toMap,
    )

  private def call(
      name: String,
      params: List[DescriptorParam],
      returnType: SourceTypeTemplate,
      receiverMutable: Boolean = false,
  ): DescriptorCallable =
    DescriptorCallable(name, params, returnType, receiverMutable)

  private def param(
      name: String,
      valueType: SourceTypeTemplate,
  ): DescriptorParam =
    DescriptorParam(name, valueType)
