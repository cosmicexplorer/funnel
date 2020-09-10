package funnel.language

import org.parboiled2._

import scala.util.matching.Regex


// TODO: We would prefer to isolate parts of these in different containing `object`s or files, but
// there are a number of circular dependencies in types that are contained. This may be possible to
// refactor.


object Errors {
  import NonNodeData._
  import FunnelPEG._

  sealed abstract class FunnelParseError(message: String) extends Exception(message)

  case class InvalidIndexError(index: Int, message: String) extends FunnelParseError(message)

  sealed abstract class InternalError(location: String, message: String)
      extends FunnelParseError(s"internal error in $location: $message")

  case class InvalidParameterPackConstruction(
    message: String,
  ) extends InternalError("parameter pack construction", message)

  object InvalidIdentifierKind {
    def apply[Kind <: ExpressionKinds](
      id: NamedIdentifier[Kind],
      pattern: Regex,
    )(implicit kind: Kind): InvalidIdentifierKind[Kind] = InvalidIdentifierKind(kind, id, pattern)
  }
  case class InvalidIdentifierKind[Kind <: ExpressionKinds](
    kind: Kind,
    id: NamedIdentifier[Kind],
    pattern: Regex,
  ) extends FunnelParseError(s"$kind identifier recognition for $id failed: did not match $pattern")

  case class InvalidCaseName(name: String, pattern: Regex)
      extends FunnelParseError(s"case names must always match the pattern $pattern: was $name")

  case class ExceptionHandlingError(message: String)
      extends InternalError("while handling an exception", message)

  case class PullOutNewTypeParamsError(
    parent: AstNode, typeParams: TypeParamPack, message: String,
  ) extends InternalError(
    s"while trying to pull out type params $typeParams from parent $parent",
    message)
}


// Attributes of AST nodes (data they contain in public fields):
object NonNodeData {
  import Errors._
  import FunnelPEG._

  object ExpressionKinds {
    implicit val _v = ValueKind
    implicit val _t = TypeKind
  }

  sealed abstract class ExpressionKinds
  case object ValueKind extends ExpressionKinds
  case object TypeKind extends ExpressionKinds

  // Define classes which are *not* AST nodes.
  case class OneIndexedPosition[Kind <: ExpressionKinds](position: Int) {
    if (position <= 0) {
      throw InvalidIndexError(position, s"position must be greater than 0, but was $position")
    }

    def intoIdentifier(implicit kind: Kind): NamedIdentifier[Kind] = {
      // NB: This is how you can match on a compile time-resolved case object!!!
      val generalKind: ExpressionKinds = kind
      val name = generalKind match {
        case ValueKind => s"-${position}"
        case TypeKind => s"_${position}"
      }
      NamedIdentifier[Kind](name)
    }
  }

  object NamedIdentifier {
    val nonEmptyIdentifier = """.+""".r
    val valueIdentifier = """[a-z\-][a-zA-Z\-_0-9]*""".r
    val typeIdentifier = """[A-Z_][a-zA-Z\-_0-9]*""".r
  }
  case class NamedIdentifier[Kind <: ExpressionKinds](name: String)(implicit kind: Kind) {
    import NamedIdentifier._

    name match {
      case nonEmptyIdentifier() => ()
      case _ => throw InvalidIdentifierKind(this, nonEmptyIdentifier)
    }
    val generalKind: ExpressionKinds = kind
    generalKind match {
      case ValueKind => name match {
        case valueIdentifier() => ()
        case _ => throw InvalidIdentifierKind(this, valueIdentifier)
      }
      case TypeKind => name match {
        case typeIdentifier() => ()
        case _ => throw InvalidIdentifierKind(this, typeIdentifier)
      }
    }
  }

  trait HasNamedIdentifier[Kind <: ExpressionKinds] {
    def namedIdentifier: NamedIdentifier[Kind]
  }

  // Capitalization rules are enforced for types and values, so we need to have the kind parameter
  // available even up here.
  sealed abstract class LocalIdentifierKinds[Kind <: ExpressionKinds] extends HasNamedIdentifier[Kind]
  case class LocalNamedIdentifier[Kind <: ExpressionKinds](id: NamedIdentifier[Kind])
      extends LocalIdentifierKinds[Kind] {
    override def namedIdentifier = id
  }
  case class PositionalIdentifier[Kind <: ExpressionKinds](
    position: OneIndexedPosition[Kind],
  )(implicit kind: Kind) extends LocalIdentifierKinds[Kind] {
    override def namedIdentifier = position.intoIdentifier
  }

  // Describes the symbol used to identify an alternation case, e.g. '+a'.
  object AlternationCaseName {
    val caseIdentifier = """[a-z\-][a-zA-Z\-_0-9]*""".r
  }
  case class AlternationCaseName(name: String) {
    import AlternationCaseName._

    name match {
      case caseIdentifier() => ()
      case _ => throw InvalidCaseName(name, caseIdentifier)
    }
  }

  sealed abstract class VarKinds[Kind <: ExpressionKinds] extends HasNamedIdentifier[Kind]
  case class GlobalVar[Kind <: ExpressionKinds](id: NamedIdentifier[Kind]) extends VarKinds[Kind] {
    override def namedIdentifier = id
  }
  case class LocalVar[Kind <: ExpressionKinds](localId: LocalIdentifierKinds[Kind])
      extends VarKinds[Kind] {
    override def namedIdentifier: NamedIdentifier[Kind] = localId.namedIdentifier
  }

  // Define parameter packing types.
  object ParameterPackKinds {
    def empty[K <: ExpressionKinds, V <: ExpressionKinds](
      implicit emptyPack: EmptyParameterPack[K, V],
    ): EmptyParameterPack[K, V] = emptyPack

  }
  sealed abstract class ParameterPackKinds[K <: ExpressionKinds, V <: ExpressionKinds] {
    def isEmpty: Boolean
    def convertIfEmpty(implicit emptyPack: EmptyParameterPack[K, V]): ParameterPackKinds[K, V] =
      if (isEmpty) {
        emptyPack
      } else { this }
    def intoLazyPack(): LazyParameterPack[K, V]
  }
  sealed abstract class EmptyParameterPack[K <: ExpressionKinds, V <: ExpressionKinds]
      extends ParameterPackKinds[K, V] {
    override def isEmpty = true
    override def intoLazyPack(): LazyParameterPack[K, V] = LazyParameterPack(Seq())
  }
  implicit case object EmptyValueTypeParameterPack
      extends EmptyParameterPack[ValueKind.type, TypeKind.type]
  implicit case object EmptyValueValueParameterPack
      extends EmptyParameterPack[ValueKind.type, ValueKind.type]
  implicit case object EmptyTypeTypeParameterPack
      extends EmptyParameterPack[TypeKind.type, TypeKind.type]

  case class StandaloneParameter[K <: ExpressionKinds, V <: ExpressionKinds](
    k: K,
    expr: Expression[V],
  ) extends ParameterPackKinds[K, V] {
    override def isEmpty = false
    override def intoLazyPack(): LazyParameterPack[K, V] =
      LazyParameterPack(Seq(None -> expr))
  }
  case class PositionalParameterPack[K <: ExpressionKinds, V <: ExpressionKinds](
    k: K,
    exprs: Seq[Expression[V]],
  ) extends ParameterPackKinds[K, V] {
    override def isEmpty = exprs.size == 0
    override def intoLazyPack(): LazyParameterPack[K, V] =
      LazyParameterPack(exprs.map(None -> _))
  }
  object NamedParameterPack {
    def merge[K <: ExpressionKinds, V <: ExpressionKinds](
      packs: Seq[NamedParameterPack[K, V]],
    ): NamedParameterPack[K, V] = NamedParameterPack(packs.flatMap(_.fulfilled).toMap)
  }
  case class NamedParameterPack[K <: ExpressionKinds, V <: ExpressionKinds](
    fulfilled: Map[NamedIdentifier[K], Expression[V]],
  ) extends ParameterPackKinds[K, V] {
    override def isEmpty = fulfilled.size == 0
    override def intoLazyPack(): LazyParameterPack[K, V] =
      LazyParameterPack(fulfilled.map {
        case (namedId, expr) => (Some(namedId) -> expr)
      }.toSeq)
  }

  
  // Define classes used to manipulate AST nodes and their attributes. These may also be
  // `NodeAttribute`s.
  // When parameters are coalesced, they come from a specific direction relative to the source pack
  // (with <=, =>, <-, or ->).
  sealed abstract class ParameterPushLocationKinds
  case object Left extends ParameterPushLocationKinds
  case object Right extends ParameterPushLocationKinds

  trait SidewaysMergeable[Rhs, Result] {
    def merge(fromLocation: ParameterPushLocationKinds, other: Rhs): Result
  }

  trait SelfSidewaysMergeable[T] extends SidewaysMergeable[T, T]

  // This class is used to allow coalescing arguments which may be provided by name OR by position,
  // e.g.:
  // $f(2, 3, .x(4), 5)
  object LazyParameterPack {
    def empty[K <: ExpressionKinds, V <: ExpressionKinds]: LazyParameterPack[K, V] =
      LazyParameterPack[K, V](Seq())
  }
  case class LazyParameterPack[K <: ExpressionKinds, V <: ExpressionKinds](
    exprs: Seq[(Option[NamedIdentifier[K]], Expression[V])],
  ) extends SelfSidewaysMergeable[LazyParameterPack[K, V]] {
    def size: Int = exprs.size
    def headIdent: Option[NamedIdentifier[K]] = exprs.head._1

    override def merge(
      fromLocation: ParameterPushLocationKinds,
      other: LazyParameterPack[K, V],
    ): LazyParameterPack[K, V] = (fromLocation, other) match {
      case (Left, LazyParameterPack(leftExprs)) => LazyParameterPack(leftExprs ++ exprs)
      case (Right, LazyParameterPack(rightExprs)) => LazyParameterPack(exprs ++ rightExprs)
    }
    def intoFullPack()(implicit kind: K): FullParameterPack[K, V] = FullParameterPack(
      mapping = exprs.zipWithIndex.map {
        case ((maybeNamedId, expr), index) => {
          // If there's no explicit named parameter, fill out the hole with a positional param.
          val namedId = maybeNamedId.getOrElse {
            // NB: We use 1-based indexing -- hence index + 1.
            // TODO: Test this!!!
            OneIndexedPosition[K](index + 1).intoIdentifier
          }
          (namedId -> expr)
        }
      }.toMap
    )
  }

  // This class represents an argument pack after it has subsumed any further arguments from the
  // left or right. Positional and standalone argument packs are converted to identifiers by
  // OneIndexedPosition.intoIdentifier()!
  case class FullParameterPack[K <: ExpressionKinds, V <: ExpressionKinds](
    mapping: Map[NamedIdentifier[K], Expression[V]],
  )
}

// Define AST entities.
object FunnelPEG {
  import Errors._
  import NonNodeData._

  sealed abstract class AstNode

  sealed abstract class CaseNode extends AstNode
  case class AlternationCase(
    name: AlternationCaseName,
    slt: StructTypeLiteral,
  ) extends CaseNode

  sealed abstract class BaseStatement extends AstNode
  sealed abstract class Statement[Kind <: ExpressionKinds] extends BaseStatement

  object ParamsDeclaration {
    def empty[Kind <: ExpressionKinds](
      implicit emptyPack: EmptyParameterPack[Kind, TypeKind.type],
    ): ParamsDeclaration[Kind] = ParamsDeclaration(emptyPack)
    def apply[Kind <: ExpressionKinds](
      params: ParameterPackKinds[Kind, TypeKind.type],
    ): ParamsDeclaration[Kind] = ParamsDeclaration(params.intoLazyPack())
    def apply(
      params: NamedValueParamPack,
    ): ParamsDeclaration[ValueKind.type] = params.bidiPack.functionParams
  }
  case class ParamsDeclaration[Kind <: ExpressionKinds](
    declaration: LazyParameterPack[Kind, TypeKind.type]
  ) extends SelfSidewaysMergeable[ParamsDeclaration[Kind]] {
    override def merge(
      fromLocation: ParameterPushLocationKinds,
      other: ParamsDeclaration[Kind],
    ): ParamsDeclaration[Kind] =
      ParamsDeclaration(declaration.merge(fromLocation, other.declaration))
  }

  object BidirectionalParameterPack {
    def empty[K <: ExpressionKinds, V <: ExpressionKinds](
      implicit emptyPack: EmptyParameterPack[K, V],
      emptyPack2: EmptyParameterPack[K, TypeKind.type],
    ): BidirectionalParameterPack[K, V] =
      BidirectionalParameterPack(emptyPack, ParamsDeclaration.empty[K])
  }
  case class BidirectionalParameterPack[K <: ExpressionKinds, V <: ExpressionKinds](
    parameterPack: ParameterPackKinds[K, V],
    functionParams: ParamsDeclaration[K],
  )

  trait IsValue {
    def bidiPack: BidirectionalParameterPack[ValueKind.type, ValueKind.type] =
      BidirectionalParameterPack.empty
  }

  trait HasType {
    def typeParams: ParamsDeclaration[TypeKind.type] = ParamsDeclaration.empty
    def extractType: TypeExpression
  }

  trait IsTypePack {
    def bidiTypePack: BidirectionalParameterPack[TypeKind.type, TypeKind.type] =
      BidirectionalParameterPack.empty
  }

  sealed abstract class Expression[V <: ExpressionKinds] extends AstNode with HasType

  sealed abstract class ValueExpression extends Expression[ValueKind.type] with IsValue

  case class TypeParamsWrapperForValue(
    subject: ValueExpression,
    tpp: NamedTypeParamPack,
    fromLocation: ParameterPushLocationKinds,
  )
      extends ValueExpression {
    override def bidiPack = subject.bidiPack
    override def extractType = TypePlaceholder
    override def typeParams = subject.typeParams.merge(fromLocation, tpp.typeParams)
  }

  sealed abstract class ValueTerm extends ValueExpression {
    override def bidiPack = super.bidiPack.copy(
      parameterPack = StandaloneParameter(ValueKind, this),
    )
    override def extractType = TypePlaceholder
  }
  case class GlobalValueVar(g: GlobalVar[ValueKind.type]) extends ValueTerm
  case class LocalValueVar(l: LocalVar[ValueKind.type]) extends ValueTerm

  // TODO: It's kind of weird how we still have to add the [Kind <: ...] type parameter here,
  // because it does not appear to be possible to refer to the type of a case object like ValueKind!
  // NB: Otherwise known as struct literals!
  object ValuePackExpression {
    def empty = EmptyStruct
  }
  sealed abstract class ValuePackExpression(
    pack: ParameterPackKinds[ValueKind.type, ValueKind.type],
  ) extends ValueExpression {
    override def bidiPack = super.bidiPack.copy(
      parameterPack = pack,
    )
  }
  case object EmptyStruct extends ValuePackExpression(EmptyValueValueParameterPack) {
    override def extractType: TypeExpression = StructTypeLiteral(ParameterPackKinds.empty)
  }
  case class StandaloneValueExpression(ve: ValueExpression)
      extends ValuePackExpression(StandaloneParameter(ValueKind, ve)) {
    override def extractType: TypeExpression = ve.extractType
  }
  case class PositionalValueParameterPack(exprs: Seq[ValueExpression])
      extends ValuePackExpression(PositionalParameterPack(ValueKind, exprs)) {
    override def extractType: TypeExpression =
      StructTypeLiteral(PositionalParameterPack(ValueKind, exprs.map(_.extractType)))
  }
  object NamedValuePack {
    def apply(
      fullPack: FullParameterPack[ValueKind.type, ValueKind.type],
    ): NamedValuePack = NamedValuePack(fullPack.mapping)
    def merge(packs: Seq[NamedValuePack]): NamedValuePack =
      NamedValuePack(packs.flatMap(_.fulfilled).toMap)
  }
  case class NamedValuePack(
    fulfilled: Map[NamedIdentifier[ValueKind.type], Expression[ValueKind.type]],
  ) extends ValuePackExpression(NamedParameterPack(fulfilled)) {
    override def extractType: TypeExpression = StructTypeLiteral(NamedParameterPack(fulfilled.map {
      case (namedIdentifier, valueExpr) => (namedIdentifier -> valueExpr.extractType)
    }))
  }

  sealed abstract class FunctionCallLike extends ValueExpression {
    override def bidiPack = super.bidiPack.copy(
      parameterPack = StandaloneParameter(ValueKind, this)
    )
    override def extractType: TypeExpression = TypePlaceholder
  }
  case class FunctionCall(
    source: ValueExpression,
    arguments: ParameterPackKinds[ValueKind.type, ValueKind.type],
  ) extends FunctionCallLike
  case class CurriedFunctionCall(
    source: ValueExpression,
    arguments: NamedValuePack,
  ) extends FunctionCallLike

  // TODO!!!
  // case class TypeValueFunctionCall(
  //   source: ValueExpression,
  //   arguments: ParameterPackKinds[TypeKind.type, TypeKind.type],
  // ) extends ValueExpression {
  //   override def bidiPack = source.bidiPack
  //   override def extractType = TypePlaceholder
  // }
  case class TypeTypeFunctionCall(
    source: TypeExpression,
    arguments: ParameterPackKinds[TypeKind.type, TypeKind.type],
  ) extends TypePack(StandaloneParameter(TypeKind, source))

  sealed abstract class TypeExpression extends Expression[TypeKind.type] {
    override def extractType = this
  }

  sealed abstract class TypeTerm extends TypeExpression
  case class GlobalTypeVar(g: GlobalVar[TypeKind.type]) extends TypeTerm
  case class LocalTypeVar(l: LocalVar[TypeKind.type]) extends TypeTerm
  // This is used in cases where the type needs to be computed after the parse phase.
  case object TypePlaceholder extends TypeTerm

  // TODO: remove this!!!
  case class TypePackWorkaround(typePack: TypePack) extends TypeExpression

  // NB: While `TypePack` instances may be used to declare and invoke type functions, they do *NOT*
  // serve as "expressions" in the way values do! This is because we want a `TypeExpression` to
  // strictly denote a *single*, *non-compound* type! We expect to be able to ensure this via
  // parsing.
  sealed abstract class TypePack(
    pack: ParameterPackKinds[TypeKind.type, TypeKind.type],
  ) extends AstNode with IsTypePack {
    override def bidiTypePack = super.bidiTypePack.copy(
      parameterPack = pack,
    )
  }
  case object EmptyStructType extends TypePack(EmptyTypeTypeParameterPack)
  case class StandaloneTypeExpression(te: TypeExpression)
      extends TypePack(StandaloneParameter(TypeKind, te))
  case class PositionalTypePack(exprs: Seq[TypeExpression])
      extends TypePack(PositionalParameterPack(TypeKind, exprs))
  object NamedTypePack {
    def merge(fields: Seq[NamedTypePack]): NamedTypePack =
      NamedTypePack(fields.flatMap(_.fulfilled).toMap)
    def apply(
      fullPack: FullParameterPack[TypeKind.type, TypeKind.type],
    ): NamedTypePack = NamedTypePack(fullPack.mapping)
  }
  case class NamedTypePack(
    fulfilled: Map[NamedIdentifier[TypeKind.type], Expression[TypeKind.type]],
  ) extends TypePack(NamedParameterPack(fulfilled))

  // NB: There are no 0-argument functions, so no empty struct analogy here! Same with positional!
  sealed abstract class TypeParamPack(
    pack: ParameterPackKinds[TypeKind.type, TypeKind.type],
  ) extends TypeExpression with IsTypePack {
    private val fullPack = pack.intoLazyPack().intoFullPack()
    // If we're at the end of the input, this turns into an anonymous function, essentially, so we
    // create a named value pack (after coalescing all the fields) which refers to all the local
    // function params! E.g. if we have:
    // $F <= [\.X, \.Y]
    // that becomes equivalent to:
    // $F <= [\.X, \.Y] => [.X, .Y]
    private val namedParamPack = fullPack.mapping.map {
      case (namedId, _) => (namedId -> LocalTypeVar(LocalVar(LocalNamedIdentifier(namedId))))
    }
    override def typeParams = ParamsDeclaration(pack)
    override def bidiTypePack = super.bidiTypePack.copy(
      parameterPack = NamedParameterPack(namedParamPack),
      functionParams = ParamsDeclaration(pack),
    )
  }
  object NamedTypeParamPack {
    def merge(packs: Seq[NamedTypeParamPack]): NamedTypeParamPack =
      NamedTypeParamPack(packs.flatMap(_.unfulfilled).toMap)
  }
  case class NamedTypeParamPack(
    unfulfilled: Map[NamedIdentifier[TypeKind.type], Expression[TypeKind.type]],
  ) extends TypeParamPack(NamedParameterPack(unfulfilled))

  case class EnumLiteralValue(cases: Seq[AlternationCase])
      extends ValueExpression {
    override def bidiPack = super.bidiPack.copy(
      parameterPack = StandaloneParameter(ValueKind, this)
    )
    override def extractType = EnumTypeLiteral(this)
  }

  sealed abstract class ValueLiteral(ty: TypeLiteral) extends ValueExpression {
    override def bidiPack = super.bidiPack.copy(
      parameterPack = StandaloneParameter(ValueKind, this),
    )
    override def extractType = ty
  }
  case class IntegerLiteral(numberValue: Int) extends ValueLiteral(IntegerTypeLiteral)
  case class FloatLiteral(numberValue: Double) extends ValueLiteral(FloatTypeLiteral)
  case class StringLiteral(inner: String) extends ValueLiteral(StringTypeLiteral)

  sealed abstract class TypeLiteral extends TypeExpression
  case object IntegerTypeLiteral extends TypeLiteral
  case object FloatTypeLiteral extends TypeLiteral
  case object StringTypeLiteral extends TypeLiteral
  case class StructTypeLiteral(struct: ParameterPackKinds[ValueKind.type, TypeKind.type])
      extends TypeLiteral
  case class EnumTypeLiteral(enumLiteral: EnumLiteralValue)
      extends TypeLiteral

  sealed abstract class FunctionSignatureLike(
    output: TypeExpression,
    functionParams: ParamsDeclaration[ValueKind.type],
  ) extends TypeLiteral
  // This is used for:
  // <$f> <- <(\.x, \.y)>
  case class NamedValueParamPackSignature(
    output: TypeExpression,
    functionParams: ParamsDeclaration[ValueKind.type],
  ) extends FunctionSignatureLike(output, functionParams)
  // <$f> <- <\.p -> $Integer>
  case class MethodSignature(
    output: TypeExpression,
    functionParams: ParamsDeclaration[ValueKind.type],
    typeFunctionParams: ParamsDeclaration[TypeKind.type] = ParamsDeclaration.empty,
  ) extends FunctionSignatureLike(output, functionParams) {
    override def typeParams = typeFunctionParams
  }

  sealed abstract class ValueParamPack(
    pack: ParameterPackKinds[ValueKind.type, TypeKind.type],
  ) extends ValueExpression {
    private val fullPack = pack.intoLazyPack().intoFullPack()
    // If we're at the end of the input, this turns into an anonymous function, essentially, so we
    // create a named value pack (after coalescing all the fields) which refers to all the local
    // function params! E.g. if we have:
    // $f <= (\.x, \.y)
    // that becomes equivalent to:
    // $f <= (\.x, \.y) => (.x, .y)
    private val namedParamPack = fullPack.mapping.map {
      case (namedId, _) => (namedId -> LocalValueVar(LocalVar(LocalNamedIdentifier(namedId))))
    }
    override def bidiPack = super.bidiPack.copy(
      parameterPack = NamedParameterPack(namedParamPack),
      functionParams = ParamsDeclaration(pack),
    )
    override def extractType = NamedValueParamPackSignature(
      output = NamedValuePack(namedParamPack).extractType,
      functionParams = ParamsDeclaration(pack),
    )
  }
  // NB: There are no 0-argument functions, so no empty struct analogy here! Same with positional!
  object NamedValueParamPack {
    def merge(packs: Seq[NamedValueParamPack]): NamedValueParamPack =
      NamedValueParamPack(packs.flatMap(_.unfulfilled).toMap)
  }
  case class NamedValueParamPack(
    unfulfilled: Map[NamedIdentifier[ValueKind.type], TypeExpression],
  ) extends ValueParamPack(NamedParameterPack(unfulfilled))

  case class AnonymousMethod(
    output: ValueExpression,
    functionParams: ParamsDeclaration[ValueKind.type],
    typeFunctionParams: ParamsDeclaration[TypeKind.type] = ParamsDeclaration.empty,
  ) extends ValueExpression {
    override def bidiPack = super.bidiPack.copy(
      parameterPack = output.bidiPack.parameterPack,
      functionParams = functionParams,
    )
    override def typeParams = typeFunctionParams
    override def extractType = MethodSignature(
      output = output.extractType,
      functionParams = functionParams,
      typeFunctionParams = typeFunctionParams,
    )

    def pullOutAnyNewTypeParams: AnonymousMethod = {
      // throw new Exception(toString)
      val (modifiedFunctionParams, newTypeFunctionParams): (LazyParameterPack[ValueKind.type, TypeKind.type], LazyParameterPack[TypeKind.type, TypeKind.type]) =
        bidiPack.functionParams.declaration.exprs
          .zipWithIndex
          .foldLeft(LazyParameterPack.empty[ValueKind.type, TypeKind.type] -> typeParams.declaration) {
            (acc, cur) => (acc -> cur) match {
              case ((modifiedFunctionParams, newTypeFunctionParams), ((maybeNamedId, expr), index)) => try {
                val typeParamPack = expr.asInstanceOf[TypeParamPack]
                val lazyTypeParams: LazyParameterPack[TypeKind.type, TypeKind.type] =
                  typeParamPack.typeParams.declaration
                if (lazyTypeParams.size != 1) {
                  throw PullOutNewTypeParamsError(
                    this, typeParamPack, "expected type param pack to only have a single entry!")
                } else {
                  val namedId = lazyTypeParams.headIdent.getOrElse {
                    OneIndexedPosition[TypeKind.type](index + 1).intoIdentifier
                  }
                  (
                    modifiedFunctionParams.merge(Right, LazyParameterPack(Seq(
                      maybeNamedId -> LocalTypeVar(LocalVar[TypeKind.type](
                        LocalNamedIdentifier[TypeKind.type](namedId)))
                    ))),
                    newTypeFunctionParams.merge(Right, lazyTypeParams)
                  )
                }
              } catch {
                case e: ClassCastException => (
                  modifiedFunctionParams.merge(Right, LazyParameterPack(Seq(
                    maybeNamedId -> expr))),
                  newTypeFunctionParams,
                )
              }
            }}
      copy(
        functionParams = ParamsDeclaration(modifiedFunctionParams),
        typeFunctionParams = ParamsDeclaration(newTypeFunctionParams),
      )
    }
  }

  case class AnonymousTypeMethod(
    output: TypePack,
    typeFunctionParams: ParamsDeclaration[TypeKind.type],
  ) extends TypePack(output.bidiTypePack.parameterPack) {
    override def bidiTypePack = super.bidiTypePack.copy(
      functionParams = typeFunctionParams,
    )
  }

  sealed abstract class ValueStatement extends Statement[ValueKind.type]
  case class ValueAssignment(place: GlobalValueVar, ve: ValueExpression) extends ValueStatement
  case class ValueAssertion(lhs: ValueExpression, rhs: ValueExpression) extends ValueStatement

  case class ValueWithTypeAssertion(
    ve: Option[ValueExpression],
    te: Option[TypeExpression],
  ) {
    def mergeWithValue(subject: ValueExpression): ValueExpression = {
      val intermediateVe = ve match {
        case Some(ve) => InlineValueAssertion(subject, ve)
        case None => subject
      }
      te match {
        case Some(te) => InlineTypeAssertionForValue(intermediateVe, te)
        case None => intermediateVe
      }
    }
  }

  sealed abstract class InlineAssertionForValue(subject: ValueExpression) extends ValueExpression {
    override def bidiPack = super.bidiPack.copy(
      parameterPack = subject.bidiPack.parameterPack,
    )
    override def extractType = subject.extractType
  }
  case class InlineValueAssertion(subject: ValueExpression, constraint: ValueExpression)
      extends InlineAssertionForValue(subject)

  sealed abstract class TypeStatement extends Statement[TypeKind.type]
  case class TypeAssignment(place: GlobalTypeVar, te: TypeExpression) extends TypeStatement
  case class TypeAssertion(lhs: TypeExpression, rhs: TypeExpression) extends TypeStatement

  case class InlineTypeAssertionForValue(subject: ValueExpression, constraint: TypeExpression)
      extends InlineAssertionForValue(subject)

  case class InlineTypeAssertionForType(subject: TypeExpression, constraint: TypeExpression)
      extends TypeExpression {
    override def extractType = subject
  }

  sealed abstract class ExtendedValueCurrying extends AstNode
  case class IncompleteValuePack(
    innerPacks: Seq[ValuePackExpression],
  ) extends ExtendedValueCurrying {
    def appendIncompletePack(vpe: ValuePackExpression): IncompleteValuePack =
      IncompleteValuePack(innerPacks :+ vpe)
    def extendIncompletePacks(ivp: IncompleteValuePack): IncompleteValuePack =
      IncompleteValuePack(innerPacks ++ ivp.innerPacks)
    def intoNamedValuePack: NamedValuePack =
      NamedValuePack(innerPacks.map(_.bidiPack.parameterPack.intoLazyPack())
        .fold(LazyParameterPack.empty) { (lazyPack1, lazyPack2) => lazyPack2.merge(Left, lazyPack1) }
        .intoFullPack())
  }
}

class FunnelPEG(override val input: ParserInput) extends Parser {
  import NonNodeData._
  import FunnelPEG._

  // Define the parsing rules.
  def Funnel: Rule1[Seq[BaseStatement]] = rule { WhiteSpace ~ ReplOrFile ~ WhiteSpace ~ EOI }

  def TopLevel: Rule1[BaseStatement] = rule {
    ParseValueAssignment | ParseValueAssertion | ParseTypeAssignment | ParseTypeAssertion
  }

  def ReplOrFile: Rule1[Seq[BaseStatement]] = rule {
    zeroOrMore(TopLevel).separatedBy(NewLine) ~> ((s: Seq[BaseStatement]) => s)
  }

  def ParseValueAssignment: Rule1[ValueAssignment] = rule {
    (ParseGlobalValueVar ~ ParseTypeParamsCreateNoInline.? ~ ParseStructDeclNoInline.? ~ WhiteSpace ~ "<=" ~ ParseValueExpression) ~> (
      (v: GlobalValueVar, tpc: Option[NamedTypeParamPack], slv: Option[NamedValueParamPack],
        ve: ValueExpression) => {
        val valExpr = (tpc, slv) match {
          case (Some(tpc), Some(slv)) => AnonymousMethod(
            output = ve,
            functionParams = ParamsDeclaration(NamedParameterPack(slv.unfulfilled)),
            typeFunctionParams = ParamsDeclaration(NamedParameterPack(tpc.unfulfilled))).pullOutAnyNewTypeParams
          case (Some(tpc), None) => TypeParamsWrapperForValue(
            subject = ve,
            tpp = NamedTypeParamPack(tpc.unfulfilled),
            fromLocation = Left,
          )
          case (None, Some(slv)) => AnonymousMethod(
            output = ve,
            functionParams = ParamsDeclaration(NamedParameterPack(slv.unfulfilled))).pullOutAnyNewTypeParams
          case (None, None) => ve
        }
        ValueAssignment(v, valExpr)
      }
    )
  }
  def ParseValueAssertion: Rule1[ValueAssertion] = rule {
    (_ParseDefinitelyNotAnAssertionValue ~ WhiteSpace ~ "<!=" ~ ParseValueExpression) ~> (
      (lhs: ValueExpression, rhs: ValueExpression) => ValueAssertion(lhs, rhs)
    )
  }

  def ParseTypeAssignment: Rule1[TypeAssignment] = rule {
    (ParseGlobalTypeVar ~ "<-" ~ ParseTypeExpression) ~> (
      (t: GlobalTypeVar, te: TypeExpression) => TypeAssignment(t, te)
    )
  }
  def ParseTypeAssertion: Rule1[TypeAssertion] = rule {
    (_ParseDefinitelyNotAnAssertionType ~ WhiteSpace ~ "<!-" ~ ParseTypeExpression) ~> (
      (lhs: TypeExpression, rhs: TypeExpression) => TypeAssertion(lhs, rhs)
    )
  }

  def ParseRestOfIdentifier: Rule0 = rule { zeroOrMore(CharPredicate.AlphaNum | anyOf("-_")) }

  def ParseValueIdentifier: Rule1[NamedIdentifier[ValueKind.type]] = rule {
    capture((CharPredicate.LowerAlpha | anyOf("-")) ~ ParseRestOfIdentifier) ~ WhiteSpace ~> (
      (s: String) => NamedIdentifier[ValueKind.type](s))
  }
  def ParseTypeIdentifier: Rule1[NamedIdentifier[TypeKind.type]] = rule {
    capture((CharPredicate.UpperAlpha | anyOf("_")) ~ ParseRestOfIdentifier) ~ WhiteSpace ~> (
      (s: String) => NamedIdentifier[TypeKind.type](s))
  }

  def ParseGlobalValueVar: Rule1[GlobalValueVar] = rule {
    ("$" ~ ParseValueIdentifier) ~> ((namedIdentifier: NamedIdentifier[ValueKind.type]) => {
      println(s"id: $namedIdentifier")
      GlobalValueVar(GlobalVar(namedIdentifier))
    })
  }
  def ParseGlobalTypeVar: Rule1[GlobalTypeVar] = rule {
    "$" ~ ParseTypeIdentifier ~> ((namedIdentifier: NamedIdentifier[TypeKind.type]) => GlobalTypeVar(GlobalVar(namedIdentifier)))
  }

  def ParseNamedLocalValueVar: Rule1[LocalValueVar] = rule {
    "." ~ ParseValueIdentifier ~> ((namedIdentifier: NamedIdentifier[ValueKind.type]) => LocalValueVar(LocalVar(LocalNamedIdentifier(namedIdentifier))))
  }
  def ParseNamedLocalTypeVar: Rule1[LocalTypeVar] = rule {
    "." ~ ParseTypeIdentifier ~> ((namedIdentifier: NamedIdentifier[TypeKind.type]) => LocalTypeVar(LocalVar(LocalNamedIdentifier(namedIdentifier))))
  }

  def ParseIntegerLiteral: Rule1[IntegerLiteral] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((s: String) => IntegerLiteral(s.toInt))
  }
  def ParseStringLiteral: Rule1[StringLiteral] = rule {
    ("\"" ~ capture(oneOrMore(noneOf("\"\\"))) ~ "\"") ~> ((s: String) => StringLiteral(s))
  }

  def ParseFloatLiteral: Rule1[FloatLiteral] = rule {
    ((capture(oneOrMore(CharPredicate.Digit)) ~ str(".") ~ capture(oneOrMore(CharPredicate.Digit))) ~> (
      (lhs: String, rhs: String) => FloatLiteral(s"$lhs.$rhs".toDouble)
    )
      | ((str(".") ~ capture(oneOrMore(CharPredicate.Digit))) ~> (
        (rhs: String) => FloatLiteral(s"0.$rhs".toDouble)
      )))
  }

  def ParseValueLiteral: Rule1[ValueLiteral] = rule {
    ParseFloatLiteral | ParseIntegerLiteral | ParseStringLiteral
  }

  def ParseEmptyStruct: Rule1[ValuePackExpression] = rule {
    capture("()") ~> ((_: String) => EmptyStruct)
  }

  def ParsePositionalValueParameterPack: Rule1[PositionalValueParameterPack] = rule {
    "(" ~ oneOrMore(ParseValueExpression).separatedBy(",") ~ ")" ~> (
      (values: Seq[ValueExpression]) => PositionalValueParameterPack(values)
    )
  }

  def _ParseValueWithTypeAssertion: Rule1[ValueWithTypeAssertion] = rule {
    ((optional("<=") ~ "(" ~ ParseValueExpression ~ ")" ~ optional("<-") ~ "[" ~ ParseTypeExpression ~ "]") ~> (
      (ve: ValueExpression, te: TypeExpression) => ValueWithTypeAssertion(Some(ve), Some(te)))
      | (optional("<=") ~ "(" ~ ParseValueExpression ~ ")") ~> (
        (ve: ValueExpression) => ValueWithTypeAssertion(Some(ve), None))
      | (optional("<-") ~ "[" ~ ParseTypeExpression ~ "]") ~> (
        (te: TypeExpression) => ValueWithTypeAssertion(None, Some(te)))
      | capture(MATCH) ~> ((_: String) => ValueWithTypeAssertion(None, None)))
  }
  def _ParseStructField: Rule1[NamedValuePack] = rule {
    ParseNamedLocalValueVar ~ _ParseValueWithTypeAssertion ~> (
      (localVar: LocalValueVar, valueWithType: ValueWithTypeAssertion) => NamedValuePack(Map(
        localVar.l.namedIdentifier -> valueWithType.mergeWithValue(localVar),
      )))
  }
  def ParseNamedValuePack: Rule1[NamedValuePack] = rule {
    (("(" ~ oneOrMore(_ParseStructField).separatedBy(",") ~ ")") ~> (
      (fields: Seq[NamedValuePack]) => NamedValuePack.merge(fields))
      | _ParseStructField ~> (
        (field: NamedValuePack) => field)
    )
  }

  def ParseValueParameterPack: Rule1[ValuePackExpression] = rule {
    ParseEmptyStruct |
    ParseNamedValuePack |
    ParsePositionalValueParameterPack
  }

  def ParseTypePack: Rule1[TypePack] = rule {
    ParseNamedTypePack | ParsePositionalTypePack | ParseTypeFunctionCall | ParseAppliedTypeParams
  }

  def ParseFunctionSource: Rule1[ValueExpression] = rule {
    ("(" ~ (ParseValueCurrying | ParseInlineAssertionsForValue) ~ ")") |
    ParseStructDeclValueKeepingAfter |
    ParseGlobalValueVar |
    ParseNamedLocalValueVar |
    ParseValueLiteral
  }

  def ParseFunctionCall: Rule1[FunctionCall] = rule {
    // FIXME: this should really be using something more like the "chained" value expressions, but
    // those don't work yet. Right now it only works when using a global function as the source.
    ((ParseFunctionSource ~ "<=".? ~ ParseValueParameterPack) ~> (
      (ve: ValueExpression, vpe: ValuePackExpression) => FunctionCall(ve, vpe.bidiPack.parameterPack)
    ))
  }

  def _ParseUnparenthesizedValue: Rule1[ValueExpression] = rule {
    ParseFunctionCall |
    ParseValueLiteral |
      ParseGlobalValueVar |
      _ParseStructField |
      ParseEnumDecl
  }

  def _ParseNonPackedValueExpression: Rule1[ValueExpression] = rule {
    ParseInlineAssertionsForValue |
    ParseStructDeclValueKeepingAfter |
    _ParseUnparenthesizedValue
  }

  def _ParseDefinitelyNotAnAssertionValue: Rule1[ValueExpression] = rule {
    _ParseUnparenthesizedValue |
    ("(" ~ _ParseUnparenthesizedValue ~ ")") |
    ParseValueParameterPack
  }

  def ParseValueExpression: Rule1[ValueExpression] = rule {
    _ParseNonPackedValueExpression |
    ((ParseStructDecl ~> (
        (slv: NamedValueParamPack) => AnonymousMethod(
          output = NamedValuePack(slv.bidiPack.parameterPack.intoLazyPack().intoFullPack()),
          functionParams = slv.bidiPack.functionParams,
        ).pullOutAnyNewTypeParams
    ))) |
    ("(" ~ _ParseNonPackedValueExpression ~ ")") |
    ParseValueParameterPack
  }

  def ParseTypeTerm: Rule1[TypeTerm] = rule {
    ParseGlobalTypeVar |
    ParseNamedLocalTypeVar
  }

  // Avoid having to use the backslashes in struct and enum literals within the body of the type
  // extraction (<...>) operator.
  def _ParseReducedStructFieldDecl: Rule1[NamedValueParamPack] = rule {
    ParseNamedLocalValueVar ~ _ParseFieldWithTypeAssertion.? ~> (
      (localVar: LocalValueVar, te: Option[TypeExpression]) =>
      NamedValueParamPack(Map(
        localVar.l.namedIdentifier -> te.getOrElse(TypePlaceholder),
      ))
    )
  }
  def _ParseReducedInlineFieldDecl: Rule1[NamedValueParamPack] = rule {
    ParseNamedLocalValueVar ~ optional(_ParseInlineFieldType) ~> (
      (localVar: LocalValueVar, te: Option[TypeExpression]) =>
      NamedValueParamPack(Map(
        localVar.l.namedIdentifier -> te.getOrElse(TypePlaceholder),
      ))
    )
  }
  def ParseReducedStructDecl: Rule1[NamedValueParamPack] = rule {
    (((_ParseReducedInlineFieldDecl) ~> (
        (decl: NamedValueParamPack) => decl))
      | ("(" ~ oneOrMore(_ParseReducedStructFieldDecl).separatedBy(",") ~ ")") ~> (
        (fields: Seq[NamedValueParamPack]) => NamedValueParamPack.merge(fields)
    ))
  }

  def _ParseReducedAlternationCase: Rule1[AlternationCase] = rule {
    "+" ~ ParseAlternationIdentifier ~ ParseReducedStructDecl.? ~> (
      (caseName: AlternationCaseName, innerFields: Option[NamedValueParamPack]) =>
      AlternationCase(caseName, StructTypeLiteral(
        innerFields.map(paramPack => NamedParameterPack(paramPack.unfulfilled))
          .getOrElse(ParameterPackKinds.empty)))
    )
  }
  def ParseReducedEnumDecl: Rule1[EnumTypeLiteral] = rule {
    "(" ~ oneOrMore(_ParseReducedAlternationCase).separatedBy(",") ~ ")" ~> (
      (cases: Seq[AlternationCase]) => EnumTypeLiteral(EnumLiteralValue(cases))
    )
  }

  def _KnownTypeExtractableExpressions: Rule1[TypeExpression] = rule {
    (ParseReducedEnumDecl ~> ((te: TypeExpression) => te)
      | ParseReducedStructDecl ~> ((nvpp: NamedValueParamPack) => StructTypeLiteral(NamedParameterPack(nvpp.unfulfilled)))
      | ParseValueExpression ~> ((ve: ValueExpression) => ve.extractType)
      | ParseEmptyStruct ~> ((ve: ValueExpression) => ve.extractType))
  }
  // This implements the "type extraction operator" <...>.
  def _ParseTypeExtractableExpressions: Rule1[TypeExpression] = rule {
    "<" ~ _KnownTypeExtractableExpressions ~ str(">")
  }

  def ParseTypeFunctionCall: Rule1[TypeTypeFunctionCall] = rule {
    ((ParseGlobalTypeVar ~ ParseTypePack) ~> (
      (gv: GlobalTypeVar, tpe: TypePack) => TypeTypeFunctionCall(gv, tpe.bidiTypePack.parameterPack)
    ))
  }

  def ParseAppliedTypeParams: Rule1[TypeTypeFunctionCall] = rule {
    (ParseTypeParamsKeepingAfter ~ "->" ~ ParseTypeExpression ~>
      ((atm: AnonymousTypeMethod, te: TypeExpression) => TypeTypeFunctionCall(
      source = te,
      arguments = NamedParameterPack(atm.typeFunctionParams.declaration.intoFullPack().mapping),
      )))
  }

  def _ParseUnparenthesizedType: Rule1[TypeExpression] = rule {
    ParseTypeTerm |
    _ParseTypeExtractableExpressions
  }

  def _ParseNonPackedTypeExpression: Rule1[TypeExpression] = rule {
    ParseInlineTypeAssertionForType |
    ParseAnonymousMethodSignature |
    _ParseUnparenthesizedType
  }

  def _ParseDefinitelyNotAnAssertionType: Rule1[TypeExpression] = rule {
    _ParseUnparenthesizedType |
    // TODO: this is a workaround!!!
    ParseTypeFunctionCall ~> ((tp: TypePack) => TypePackWorkaround(tp)) |
    ParseAppliedTypeParams ~> ((tp: TypePack) => TypePackWorkaround(tp))
  }

  def _ParseBaseTypeExpression: Rule1[TypeExpression] = rule {
    // TODO: this is a workaround!!!
    ParseTypeParamsKeepingAfter ~> ((tp: TypePack) => TypePackWorkaround(tp)) |
    ParseTypeFunctionCall ~> ((tp: TypePack) => TypePackWorkaround(tp)) |
    ParseAppliedTypeParams ~> ((tp: TypePack) => TypePackWorkaround(tp))
  }
  def ParseTypeExpression: Rule1[TypeExpression] = rule {
    (_ParseBaseTypeExpression ~> ((ty: TypeExpression) => ty) |
    _ParseNonPackedTypeExpression |
    // ParseAppliedTypeParams ~> ((ttfc: TypeTypeFunctionCall) => TypePackWorkaround(ttfc)) |
    ParseTypeParamsCreate |
    (("[" ~ _ParseNonPackedTypeExpression ~ "]") ~> ((ty: TypeExpression) => ty)))
  }

  def ParseAlternationIdentifier: Rule1[AlternationCaseName] = rule {
    capture((CharPredicate.LowerAlpha | anyOf("-")) ~ ParseRestOfIdentifier) ~ WhiteSpace ~> (
      (s: String) => AlternationCaseName(s)
    )
  }

  def _ParseFieldWithTypeAssertion: Rule1[TypeExpression] = rule {
    (optional("<-") ~ "[" ~ ParseTypeExpression ~ "]" ~> (
      (te: TypeExpression) => te
    )
      | "<-" ~ ParseTypeExpression ~> (
        (te: TypeExpression) => te
    ))
  }
  def _ParseStructFieldDecl: Rule1[NamedValueParamPack] = rule {
    "\\" ~ _ParseReducedStructFieldDecl
  }
  def _ParseInlineFieldType: Rule1[TypeExpression] = rule {
    (("<-".? ~ "[" ~ ParseTypeExpression ~ "]")
     | ("<-" ~ ParseTypeExpression))
  }
  def _ParseInlineFieldDecl: Rule1[NamedValueParamPack] = rule {
    "\\" ~ _ParseReducedInlineFieldDecl
  }
  def ParseStructDeclNoInline: Rule1[NamedValueParamPack] = rule {
    ("(" ~ oneOrMore(_ParseStructFieldDecl).separatedBy(",") ~ ")") ~> (
      (fields: Seq[NamedValueParamPack]) => NamedValueParamPack.merge(fields)
    )
  }
  def ParseStructDecl: Rule1[NamedValueParamPack] = rule {
    (((_ParseInlineFieldDecl) ~> (
        (decl: NamedValueParamPack) => decl))
      | ParseStructDeclNoInline)
  }

  def ParseInlineValueAssertion: Rule1[InlineValueAssertion] = rule {
    (_ParseDefinitelyNotAnAssertionValue ~ WhiteSpace ~ "<!=" ~ ParseValueExpression ~> (
      (subject: ValueExpression, constraint: ValueExpression) =>
      InlineValueAssertion(subject, constraint))
      | _ParseDefinitelyNotAnAssertionValue ~ WhiteSpace ~ "=!>" ~ ParseValueExpression ~> (
        (constraint: ValueExpression, subject: ValueExpression) =>
        InlineValueAssertion(subject, constraint)
      ))
  }
  def ParseInlineTypeAssertionForValue: Rule1[InlineTypeAssertionForValue] = rule {
    (_ParseDefinitelyNotAnAssertionValue ~ WhiteSpace ~ "<!-" ~ ParseTypeExpression ~> (
      (subject: ValueExpression, constraint: TypeExpression) =>
      InlineTypeAssertionForValue(subject, constraint))
      | _ParseDefinitelyNotAnAssertionType ~ WhiteSpace ~ "-!>" ~ ParseValueExpression ~> (
        (constraint: TypeExpression, subject: ValueExpression) =>
        InlineTypeAssertionForValue(subject, constraint)
      ))
  }
  def ParseInlineAssertionsForValue: Rule1[InlineAssertionForValue] = rule {
    ParseInlineValueAssertion | ParseInlineTypeAssertionForValue
  }

  def ParseInlineTypeAssertionForType: Rule1[InlineTypeAssertionForType] = rule {
    (_ParseUnparenthesizedType ~ "<!-" ~ ParseTypeExpression ~> (
      (subject: TypeExpression, constraint: TypeExpression) =>
      InlineTypeAssertionForType(subject, constraint))
      | _ParseUnparenthesizedType ~ "-!>" ~ ParseTypeExpression ~> (
        (constraint: TypeExpression, subject: TypeExpression) =>
        InlineTypeAssertionForType(subject, constraint)
      ))
  }

  // TODO: please explain what "keeping after" means here...
  def _ParseStructDeclValueHelper: Rule1[AnonymousMethod] = rule {
    ((ParseStructDecl ~ "=>" ~ ParseValueExpression ~> (
      (slv: NamedValueParamPack, ve: ValueExpression) =>
      AnonymousMethod(
        output = ve,
        functionParams = slv.bidiPack.functionParams)
    )))
  }
  def ParseStructDeclValueKeepingAfter: Rule1[AnonymousMethod] = rule {
    optional(ParseTypeParamsCreate ~ "->") ~ _ParseStructDeclValueHelper ~> (
      (tpc: Option[NamedTypeParamPack], anon: AnonymousMethod) =>
      anon.copy(
        typeFunctionParams = ParamsDeclaration(
          NamedParameterPack(tpc.map(_.unfulfilled).getOrElse(Map.empty)),
        ).merge(Left, anon.typeParams)
      )
    )
  }

  def _ParseInlineTypeParamDecl: Rule1[NamedTypeParamPack] = rule {
    "\\" ~ ParseNamedLocalTypeVar ~ optional(_ParseInlineFieldType) ~> (
      (localVar: LocalTypeVar, te: Option[TypeExpression]) =>
      NamedTypeParamPack(Map(
        localVar.l.namedIdentifier -> te.getOrElse(TypePlaceholder),
      ))
    )
  }
  def ParseTypeParamsCreateNoInline: Rule1[NamedTypeParamPack] = rule {
    ("[" ~ oneOrMore(_ParseInlineTypeParamDecl).separatedBy(",") ~ "]") ~> (
      (fields: Seq[NamedTypeParamPack]) => NamedTypeParamPack.merge(fields)
    )
  }
  def ParseTypeParamsCreate: Rule1[NamedTypeParamPack] = rule {
    (((_ParseInlineTypeParamDecl) ~> (
      (decl: NamedTypeParamPack) => decl))
      | ParseTypeParamsCreateNoInline)
  }

  def ParseTypeParamsKeepingAfter: Rule1[AnonymousTypeMethod] = rule {
    ((ParseTypeParamsCreate ~ "->" ~ ParseTypePack ~> (
      (tpc: NamedTypeParamPack, tp: TypePack) => AnonymousTypeMethod(
        output = tp,
        typeFunctionParams = ParamsDeclaration(NamedParameterPack(tpc.unfulfilled)))
    )))
  }

  def ParseAlternationCase: Rule1[AlternationCase] = rule {
    "\\+" ~ ParseAlternationIdentifier ~ ParseStructDecl.? ~> (
      (caseName: AlternationCaseName, innerFields: Option[NamedValueParamPack]) =>
      AlternationCase(caseName, StructTypeLiteral(innerFields.map(nvpp => NamedParameterPack(nvpp.unfulfilled)).getOrElse(ParameterPackKinds.empty)))
    )
  }
  def ParseEnumDecl: Rule1[EnumLiteralValue] = rule {
    "(" ~ oneOrMore(ParseAlternationCase).separatedBy(",") ~ ")" ~> (
      (cases: Seq[AlternationCase]) => EnumLiteralValue(cases)
    )
  }

  def ParseAnonymousMethodSignature: Rule1[MethodSignature] = rule {
    ParseStructDecl ~ "=>" ~ ParseTypeExpression ~> (
      (slt: NamedValueParamPack, te: TypeExpression) =>
      MethodSignature(
        output = te,
        functionParams = slt.bidiPack.functionParams)
    )
  }

  def ParsePositionalTypePack: Rule1[PositionalTypePack] = rule {
    "[" ~ oneOrMore(ParseTypeExpression).separatedBy(",") ~ "]" ~> (
      (types: Seq[TypeExpression]) => PositionalTypePack(types)
    )
  }

  def _ParseNamedTypeParamDereference: Rule1[NamedTypePack] = rule {
    ParseNamedLocalTypeVar ~ _ParseFieldWithTypeAssertion.? ~> (
      (localVar: LocalTypeVar, tyAssertion: Option[TypeExpression]) =>
      NamedTypePack(Map(
        localVar.l.namedIdentifier -> tyAssertion.getOrElse {
          LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T"))))
        },
      ))
    )
  }
  def ParseNamedTypePack: Rule1[NamedTypePack] = rule {
    (("[" ~ oneOrMore(_ParseNamedTypeParamDereference).separatedBy(",") ~ "]") ~> (
      (fields: Seq[NamedTypePack]) => NamedTypePack.merge(fields))
      | _ParseNamedTypeParamDereference ~> (
        (field: NamedTypePack) => field
      ))
  }

  def _ParseEnclosedIncompleteValuePack: Rule1[IncompleteValuePack] = rule {
    "(" ~ (
      ((oneOrMore(ParseValueExpression).separatedBy(",")) ~> (
        (values: Seq[ValueExpression]) => PositionalValueParameterPack(values))
        | (oneOrMore(_ParseStructField).separatedBy(",")) ~> (
          (fields: Seq[NamedValuePack]) => NamedValuePack.merge(fields)
        )) ~ "," ~ "..." ~> ((vpe: ValuePackExpression) => IncompleteValuePack(Seq(vpe)))
    ) ~ ")"
  }
  def _ParseInlineIncompleteValuePack: Rule1[IncompleteValuePack] = rule {
    ((_ParseNonPackedValueExpression ~> ((ve: ValueExpression) => StandaloneValueExpression(ve)))
      | _ParseStructField) ~ "..." ~> (
      (vpe: ValuePackExpression) => IncompleteValuePack(Seq(vpe))
    )
  }
  def _ParseBasicIncompleteValuePack: Rule1[IncompleteValuePack] = rule {
    _ParseEnclosedIncompleteValuePack | _ParseInlineIncompleteValuePack
  }
  def ParseIncompleteValuePack: Rule1[IncompleteValuePack] = rule {
    ((_ParseBasicIncompleteValuePack ~ "=>" ~ _ParseBasicIncompleteValuePack ~> (
      (source: IncompleteValuePack, dest: IncompleteValuePack) =>
      source.extendIncompletePacks(dest)))
      | (_ParseBasicIncompleteValuePack ~ "<=" ~ _ParseBasicIncompleteValuePack ~> (
        (dest: IncompleteValuePack, source: IncompleteValuePack) =>
        source.extendIncompletePacks(dest)))
    )
  }

  def ParseCompletedValuePack: Rule1[NamedValuePack] = rule {
    ((ParseIncompleteValuePack ~ "=>" ~ ParseValueParameterPack ~> (
      (ivp: IncompleteValuePack, vpe: ValuePackExpression) =>
      ivp.appendIncompletePack(vpe)))
      | (ParseValueParameterPack ~ "<=" ~ ParseIncompleteValuePack ~> (
        (vpe: ValuePackExpression, ivp: IncompleteValuePack) =>
        ivp.appendIncompletePack(vpe)
      ))) ~> ((ivp: IncompleteValuePack) => ivp.intoNamedValuePack)
  }

  def _ParseMaybeParenthesizedNonPackedValueExpression: Rule1[ValueExpression] = rule {
    (("(" ~ _ParseNonPackedValueExpression ~ ")")
      | _ParseNonPackedValueExpression)
  }
  def ParseValueCurrying: Rule1[CurriedFunctionCall] = rule {
    ((ParseIncompleteValuePack ~ "=>" ~ _ParseMaybeParenthesizedNonPackedValueExpression ~> (
      (ivp: IncompleteValuePack, ve: ValueExpression) =>
      CurriedFunctionCall(ve, ivp.intoNamedValuePack)
    ))
      | (_ParseMaybeParenthesizedNonPackedValueExpression ~ "<=" ~ ParseIncompleteValuePack) ~> (
        (ve: ValueExpression, ivp: IncompleteValuePack) =>
      CurriedFunctionCall(ve, ivp.intoNamedValuePack)))
  }

  def NewLine: Rule0 = rule { oneOrMore("\n") }
  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  // See https://github.com/cosmicexplorer/parboiled2#handling-whitespace for recommendations on
  // handling whitespace with PEG parsers (namely, by matching whitespace strictly after every
  // terminal).
  implicit def whitespaceLiteralConverter(s: String): Rule0 = rule {
    str(s) ~ WhiteSpace
  }
}
