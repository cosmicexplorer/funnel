package funnel.language

import org.parboiled2._

import scala.util.Try
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
}


// Attributes of AST nodes (data they contain in public fields):
object NonNodeData {
  import Errors._
  import FunnelPEG._

  object ExpressionKinds {
    implicit val _v = ValueKind
    implicit val _t = TypeKind
  }
  import ExpressionKinds._

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

  trait SidewaysMergeable {
    def merge(fromLocation: ParameterPushLocationKinds, other: this.type): Try[this.type]
  }

  // This class is used to allow coalescing arguments which may be provided by name OR by position,
  // e.g.:
  // $f(2, 3, .x(4), 5)
  case class LazyParameterPack[K <: ExpressionKinds, V <: ExpressionKinds](
    exprs: Seq[(Option[NamedIdentifier[K]], Expression[V])],
  ) extends SidewaysMergeable {
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
  }
  case class ParamsDeclaration[Kind <: ExpressionKinds](
    declaration: ParameterPackKinds[Kind, TypeKind.type]
  )

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
  }
  case class NamedValuePack(
    fulfilled: Map[NamedIdentifier[ValueKind.type], Expression[ValueKind.type]],
  ) extends ValuePackExpression(NamedParameterPack(fulfilled)) {
    override def extractType: TypeExpression = StructTypeLiteral(NamedParameterPack(fulfilled.map {
      case (namedIdentifier, valueExpr) => (namedIdentifier -> valueExpr.extractType)
    }))
  }

  case class FunctionCall(
    source: ValueExpression,
    arguments: ParameterPackKinds[ValueKind.type, ValueKind.type],
  )
      extends ValueExpression {
    override def bidiPack = super.bidiPack.copy(
      parameterPack = StandaloneParameter(ValueKind, this)
    )
    override def extractType: TypeExpression = TypePlaceholder
  }

  sealed abstract class TypeExpression extends Expression[TypeKind.type] {
    override def extractType = this
  }

  sealed abstract class TypeTerm extends TypeExpression
  case class GlobalTypeVar(g: GlobalVar[TypeKind.type]) extends TypeTerm
  case class LocalTypeVar(l: LocalVar[TypeKind.type]) extends TypeTerm
  // This is used in cases where the type needs to be computed after the parse phase.
  case object TypePlaceholder extends TypeTerm

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
    def apply(
      fullPack: FullParameterPack[TypeKind.type, TypeKind.type],
    ): NamedTypePack = NamedTypePack(fullPack.mapping)
  }
  case class NamedTypePack(
    fulfilled: Map[NamedIdentifier[TypeKind.type], Expression[TypeKind.type]],
  ) extends TypePack(NamedParameterPack(fulfilled))

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

  case class MethodSignature(
    output: TypeExpression,
    functionParams: ParamsDeclaration[ValueKind.type],
    typeFunctionParams: ParamsDeclaration[TypeKind.type] = ParamsDeclaration.empty,
  ) extends TypeLiteral {
    override def typeParams = typeFunctionParams
  }

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
  }

  sealed abstract class ValueStatement extends Statement[ValueKind.type]
  case class ValueAssignment(place: GlobalValueVar, ve: ValueExpression) extends ValueStatement
  case class ValueAssertion(lhs: ValueExpression, rhs: ValueExpression) extends ValueStatement

  case class InlineValueAssertion(subject: ValueExpression, constraint: ValueExpression)
      extends ValueExpression {
    override def bidiPack = super.bidiPack.copy(
      parameterPack = subject.bidiPack.parameterPack,
    )
    override def extractType = subject.extractType
  }

  sealed abstract class TypeStatement extends Statement[TypeKind.type]
  case class TypeAssignment(place: GlobalTypeVar, te: TypeExpression) extends TypeStatement
  case class TypeAssertion(lhs: TypeExpression, rhs: TypeExpression) extends TypeStatement

  case class InlineTypeAssertionForValue(subject: ValueExpression, constraint: TypeExpression)
      extends ValueExpression {
    override def bidiPack = super.bidiPack.copy(
      parameterPack = subject.bidiPack.parameterPack
    )
    override def extractType = subject.extractType
  }

  case class InlineTypeAssertionForType(subject: TypeExpression, constraint: TypeExpression)
      extends TypeExpression {
    override def extractType = subject
  }
}

class FunnelPEG(override val input: ParserInput) extends Parser {
  import FunnelPEG._

  // Define the parsing rules.
  def Funnel: Rule1[Seq[BaseStatement]] = rule { WhiteSpace ~ ReplOrFile ~ EOI }

  def TopLevel: Rule1[BaseStatement] = rule {
    ParseValueAssignment | ParseValueAssertion | ParseTypeAssignment | ParseTypeAssertion
  }

  def ReplOrFile: Rule1[Seq[BaseStatement]] = rule {
    zeroOrMore(TopLevel) ~> ((s: Seq[BaseStatement]) => s)
  }

  def ParseValueAssignment: Rule1[ValueAssignment] = rule {
    (ParseGlobalValueVar ~ ParseTypeParamsCreateNoInline.? ~ ParseStructDeclValueNoInline.? ~ "<=" ~ ParseValueExpression) ~> (
      (v: GlobalValueVar, tpc: Option[TypeParamsCreate], slv: Option[StructLiteralValue],
        ve: ValueExpression) => {
        val valExpr = (tpc, slv) match {
          case (Some(tpc), Some(slv)) => AnonymousMethod(slv, ve, tpc)
          case (Some(tpc), None) => AnonymousMethod(output = ve, typeParams = tpc)
          case (None, Some(slv)) => AnonymousMethod(slv, ve)
          case (None, None) => ve
        }
        ValueAssignment(v, valExpr)
      }

    )
  }
  def ParseValueAssertion: Rule1[ValueAssertion] = rule {
    // TODO: Why is this WhiteSpace necessary?
    (ParseValueExpression ~ WhiteSpace ~ "<=" ~ ParseValueExpression) ~> (
      (lhs: ValueExpression, rhs: ValueExpression) => ValueAssertion(lhs, rhs)
    )
  }

  def ParseTypeAssignment: Rule1[TypeAssignment] = rule {
    (ParseGlobalTypeVar ~ "<-" ~ ParseTypeExpression) ~> (
      (t: GlobalTypeVar, te: TypeExpression) => TypeAssignment(t, te)
    )
  }
  def ParseTypeAssertion: Rule1[TypeAssertion] = rule {
    (ParseTypeExpression ~ "<-" ~ ParseTypeExpression) ~> (
      (lhs: TypeExpression, rhs: TypeExpression) => TypeAssertion(lhs, rhs)
    )
  }

  def ParseRestOfIdentifier: Rule0 = rule { zeroOrMore(CharPredicate.AlphaNum | anyOf("-_")) }

  def ParseValueIdentifier: Rule1[NamedIdentifier] = rule {
    capture((CharPredicate.LowerAlpha | anyOf("-")) ~ ParseRestOfIdentifier) ~ WhiteSpace ~> (
      (s: String) => NamedIdentifier(ValueKind, s))
  }
  def ParseTypeIdentifier: Rule1[NamedIdentifier] = rule {
    capture((CharPredicate.UpperAlpha | anyOf("_")) ~ ParseRestOfIdentifier) ~ WhiteSpace ~> (
      (s: String) => NamedIdentifier(TypeKind, s))
  }

  def ParseGlobalValueVar: Rule1[GlobalValueVar] = rule {
    ("$" ~ ParseValueIdentifier) ~> ((namedIdentifier: NamedIdentifier) => {
      println(s"id: $namedIdentifier")
      GlobalValueVar(GlobalVar(namedIdentifier))
    })
  }
  def ParseGlobalTypeVar: Rule1[GlobalTypeVar] = rule {
    "$" ~ ParseTypeIdentifier ~> ((namedIdentifier: NamedIdentifier) => GlobalTypeVar(GlobalVar(namedIdentifier)))
  }

  def ParseNamedLocalValueVar: Rule1[LocalValueVar] = rule {
    "." ~ ParseValueIdentifier ~> ((namedIdentifier: NamedIdentifier) => LocalValueVar(LocalVar(LocalNamedIdentifier(namedIdentifier))))
  }
  def ParseNamedLocalTypeVar: Rule1[LocalTypeVar] = rule {
    "." ~ ParseTypeIdentifier ~> ((namedIdentifier: NamedIdentifier) => LocalTypeVar(LocalVar(LocalNamedIdentifier(namedIdentifier))))
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
  def _ParseStructField: Rule1[NamedStructField] = rule {
    ParseNamedLocalValueVar ~ _ParseValueWithTypeAssertion ~> (
      (localVar: LocalValueVar, valueWithType: ValueWithTypeAssertion) => NamedStructField(localVar, valueWithType))
  }
  def ParseNamedValuePack: Rule1[NamedValuePack] = rule {
    (("(" ~ oneOrMore(_ParseStructField).separatedBy(",") ~ ")") ~> (
      (fields: Seq[NamedStructField]) => NamedValuePack(
        LazyParameterPack(
          ValueKind,
          fields.flatMap(_.parameterPack.intoLazyPack().exprs)).intoFullPack()))
      | _ParseStructField ~> (
        (field: NamedStructField) => NamedValuePack(
          field.parameterPack.intoLazyPack().intoFullPack()))
    )
  }

  def ParseParameterPack: Rule1[ValuePackExpression] = rule {
    ParseEmptyStruct |
    ParsePositionalValueParameterPack |
    ParseNamedValuePack
  }

  def ParseTypeParameterPack: Rule1[TypePack] = rule {
    ParsePositionalTypeParameterPack | ParseNamedTypePack
  }

  def ParseFunctionCall: Rule1[FunctionCall] = rule {
    // FIXME: this should really be using something more like the "chained" value expressions, but
    // those don't work yet. Right now it only works when using a global function as the source.
    ((ParseGlobalValueVar ~ "<=".? ~ ParseParameterPack) ~> (
      (gv: GlobalValueVar, vpe: ValuePackExpression) => FunctionCall(gv, vpe.parameterPack)
    ))
  }

  def _ParseBaseValueExpression: Rule1[ValueExpression] = rule {
    ParseFunctionCall |
    ParseStructDeclValueKeepingAfter |
    ParseGlobalValueVar |
    ParseNamedLocalValueVar |
    ParseValueLiteral |
    ParseEnumDeclValue |
    ParseEmptyStruct |
    ParsePositionalValueParameterPack |
    ParseNamedValuePack
  }
  def ParseValueExpression: Rule1[ValueExpression] = rule {
    (("(" ~ ParseValueExpression ~ ")") ~> ((value: ValueExpression) => value)
    | _ParseBaseValueExpression ~> ((value: ValueExpression) => value))
  }

  def ParseTypeTerm: Rule1[TypeTerm] = rule {
    ParseGlobalTypeVar |
    ParseNamedLocalTypeVar
  }

  // Avoid having to use the backslashes in struct and enum literals within the body of the type
  // extraction (<...>) operator.
  def _ParseReducedStructFieldDecl: Rule1[NamedStructFieldDecl] = rule {
    ParseNamedLocalValueVar ~ _ParseFieldWithTypeAssertion.? ~> (
      (localVar: LocalValueVar, te: Option[TypeExpression]) =>
        NamedStructFieldDecl(localVar, te.getOrElse(TypePlaceholder))
    )
  }
  def _ParseReducedInlineFieldDecl: Rule1[NamedStructFieldDecl] = rule {
    ParseNamedLocalValueVar ~ optional(_ParseInlineFieldType) ~> (
      (localVar: LocalValueVar, te: Option[TypeExpression]) =>
        NamedStructFieldDecl(localVar, te.getOrElse(TypePlaceholder))
    )
  }
  def ParseReducedStructDecl: Rule1[StructLiteralType] = rule {
    (((_ParseReducedInlineFieldDecl) ~> (
        (decl: NamedStructFieldDecl) => NamedStructFieldDecl.merge(Seq(decl))))
      | ("(" ~ oneOrMore(_ParseReducedStructFieldDecl).separatedBy(",") ~ ")") ~> (
        (fields: Seq[NamedStructFieldDecl]) => NamedStructFieldDecl.merge(fields)
    ))
  }

  def _ParseReducedAlternationCase: Rule1[AlternationCase] = rule {
    "+" ~ ParseAlternationIdentifier ~ ParseReducedStructDecl.? ~> (
      (caseName: AlternationCaseName, innerFields: Option[StructLiteralType]) =>
      AlternationCase(caseName, innerFields.getOrElse(EmptyStructType))
    )
  }
  def ParseReducedEnumDecl: Rule1[TypeAlternation] = rule {
    "(" ~ oneOrMore(_ParseReducedAlternationCase).separatedBy(",") ~ ")" ~> (
      (cases: Seq[AlternationCase]) => TypeAlternation(
        cases.map {
          case AlternationCase(name, tpe) => (name -> tpe)
        }.toMap
      )
    )
  }


  def _ParseFullyEnclosedNamedTypePack: Rule1[NamedTypePack] = rule {

  }

  def _KnownTypeExtractableExpressions: Rule1[TypeExpression] = rule {
    ((ParseReducedStructDecl | ParseReducedEnumDecl) ~> ((te: TypeExpression) => te)
      | ParseValueExpression ~> ((ve: ValueExpression) => ve.extractType)
      | ParseEmptyStruct ~> ((ve: ValueExpression) => ve.extractType))
  }
  // This implements the "type extraction operator" <...>.
  def _ParseTypeExtractableExpressions: Rule1[TypeExpression] = rule {
    "<" ~ _KnownTypeExtractableExpressions ~ ">"
  }

  def ParseTypeParamsDereference: Rule1[TypeParamsDereference] = rule {
    _ParseFullyEnclosedNamedTypePack | ParsePositionalTypeParameterPack
  }
  def ParseTypeParamPack: Rule1[TypePack] = rule {

  }

  def ParseTypeFunctionCall: Rule1[TypeFunctionCall] = rule {
    ((ParseGlobalTypeVar ~ ParseTypeParameterPack) ~> (
      (gv: GlobalTypeVar, tpe: TypePack) => TypeFunctionCall(gv, tpe.typeParameterPack)
    ))
  }

  def _ParseBaseTypeExpression: Rule1[TypeExpression] = rule {
    ParseTypeFunctionCall |
    ParseTypeParamsKeepingAfter |
    ParseTypeParamPack |
    ParseTypeTerm |
    ParseAnonymousMethodSignature |
    _ParseTypeExtractableExpressions
  }
  def ParseTypeExpression: Rule1[TypeExpression] = rule {
    (("[" ~ _ParseBaseTypeExpression ~ "]") ~> ((ty: TypeExpression) => ty)
     | _ParseBaseTypeExpression ~> ((ty: TypeExpression) => ty))
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
  def _ParseStructFieldDecl: Rule1[NamedStructFieldDecl] = rule {
    "\\" ~ _ParseReducedStructFieldDecl
  }
  def _ParseInlineFieldType: Rule1[TypeExpression] = rule {
    ("[" ~ ParseTypeExpression ~ "]"
     | "<-" ~ ParseTypeExpression)
  }
  def _ParseInlineFieldDecl: Rule1[NamedStructFieldDecl] = rule {
    "\\" ~ _ParseReducedInlineFieldDecl
  }
  def ParseStructDeclNoInline: Rule1[StructLiteralType] = rule {
    ("(" ~ oneOrMore(_ParseStructFieldDecl).separatedBy(",") ~ ")") ~> (
      (fields: Seq[NamedStructFieldDecl]) => NamedStructFieldDecl.merge(fields)
    )
  }
  def ParseStructDecl: Rule1[StructLiteralType] = rule {
    (((_ParseInlineFieldDecl) ~> (
        (decl: NamedStructFieldDecl) => NamedStructFieldDecl.merge(Seq(decl))))
      | ParseStructDeclNoInline)
  }


  def ParseStructDeclValue: Rule1[StructLiteralValue] = rule {
    ParseStructDecl ~> ((decl: StructLiteralType) => StructLiteralValue(decl))
  }
  def ParseStructDeclValueNoInline: Rule1[StructLiteralValue] = rule {
    ParseStructDeclNoInline ~> ((decl: StructLiteralType) => StructLiteralValue(decl))
  }

  // TODO: please explain what "keeping after" means here...
  def _ParseStructDeclValueHelper: Rule1[AnonymousMethod] = rule {
    ((ParseStructDeclValue ~ "=>" ~ ParseValueExpression ~> (
      (slv: StructLiteralValue, ve: ValueExpression) => AnonymousMethod(slv, ve)
    ))
      | ((ParseStructDeclValue ~ EOI ~> (
        (slv: StructLiteralValue) => AnonymousMethod(
          slv,
          slv.dereferenceParamsPack())
      ))))
  }
  def ParseStructDeclValueKeepingAfter: Rule1[AnonymousMethod] = rule {
    optional(ParseTypeParamsCreate ~ "->") ~ _ParseStructDeclValueHelper ~> (
      (tpc: Option[TypeParamsCreate], anon: AnonymousMethod) =>
      anon.copy(
        typeParams = tpc.getOrElse(TypeParamsCreate.empty()).merge(Left, anon.typeParams)
      )
    )
  }

  def _ParseInlineTypeParamDecl: Rule1[NamedTypeParamCreate] = rule {
    "\\" ~ ParseNamedLocalTypeVar ~ optional(_ParseInlineFieldType) ~> (
      (localVar: LocalTypeVar, te: Option[TypeExpression]) =>
        NamedTypeParamCreate(localVar, te.getOrElse(TypePlaceholder))
    )
  }
  def ParseTypeParamsCreateNoInline: Rule1[TypeParamsCreate] = rule {
    ("[" ~ oneOrMore(_ParseInlineTypeParamDecl).separatedBy(",") ~ "]") ~> (
      (fields: Seq[NamedTypeParamCreate]) => TypeParamsCreate(
        LazyParameterPack(
          TypeKind,
          fields.flatMap(_.typeParameterPack.intoLazyPack().exprs)).intoFullPack())
    )
  }
  def ParseTypeParamsCreate: Rule1[TypeParamsCreate] = rule {
    (((_ParseInlineTypeParamDecl) ~> (
      (decl: NamedTypeParamCreate) => TypeParamsCreate(
        decl.parameterPack.intoLazyPack().intoFullPack())))
      | ParseTypeParamsCreateNoInline)
  }

  def ParseTypeParamsKeepingAfter: Rule1[AnonymousTypeMethod] = rule {
    ((ParseTypeParamsCreate ~ "->" ~ ParseTypeExpression ~> (
      (tpc: TypeParamsCreate, te: TypeExpression) => AnonymousTypeMethod(tpc, te)
    ))
      | ((ParseTypeParamsCreate ~ EOI ~> (
        (tpc: TypeParamsCreate) => AnonymousTypeMethod(
          tpc,
          tpc.dereferenceTypeParamsPack())
    ))))
  }

  def ParseAlternationCase: Rule1[AlternationCase] = rule {
    "\\+" ~ ParseAlternationIdentifier ~ ParseStructDecl.? ~> (
      (caseName: AlternationCaseName, innerFields: Option[StructLiteralType]) =>
      AlternationCase(caseName, innerFields.getOrElse(EmptyStructType))
    )
  }
  def ParseEnumDecl: Rule1[TypeAlternation] = rule {
    "(" ~ oneOrMore(ParseAlternationCase).separatedBy(",") ~ ")" ~> (
      (cases: Seq[AlternationCase]) => TypeAlternation(
        cases.map {
          case AlternationCase(name, tpe) => (name -> tpe)
        }.toMap
      )
    )
  }

  def ParseEnumDeclValue: Rule1[ValueAlternation] = rule {
    ParseEnumDecl ~> ((decl: TypeAlternation) => ValueAlternation(decl))
  }

  def ParseAnonymousMethodSignature: Rule1[MethodSignature] = rule {
    ParseStructDecl ~ "=>" ~ ParseTypeExpression ~> (
      (slt: StructLiteralType, te: TypeExpression) => MethodType(slt, te)
    )
  }

  def ParsePositionalTypeParameterPack: Rule1[PositionalTypePack] = rule {
    "[" ~ oneOrMore(ParseTypeExpression).separatedBy(",") ~ "]" ~> (
      (types: Seq[TypeExpression]) => PositionalTypePack(types)
    )
  }

  def _ParseNamedTypeParamDereference: Rule1[NamedTypeParamDereference] = rule {
    ParseNamedLocalTypeVar ~ _ParseFieldWithTypeAssertion.? ~> (
      (localVar: LocalTypeVar, tyAssertion: Option[TypeExpression]) =>
        NamedTypeParamDereference(localVar, tyAssertion.getOrElse(TypePlaceholder))
    )
  }
  def ParseNamedTypePack: Rule1[NamedTypePack] = rule {
    (("[" ~ oneOrMore(_ParseNamedTypeParamDereference).separatedBy(",") ~ "]") ~> (
      (fields: Seq[NamedTypeParamDereference]) => NamedTypePack(
        LazyParameterPack(
          TypeKind,
          fields.flatMap(_.typeParameterPack.intoLazyPack().exprs)).intoFullPack()))
      | _ParseNamedTypeParamDereference ~> (
        (field: NamedTypeParamDereference) => NamedTypePack(
          field.typeParameterPack.intoLazyPack().intoFullPack())
      ))
  }

  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  // See https://github.com/cosmicexplorer/parboiled2#handling-whitespace for recommendations on
  // handling whitespace with PEG parsers (namely, by matching whitespace strictly after every
  // terminal).
  implicit def whitespaceLiteralConverter(s: String): Rule0 = rule {
    str(s) ~ WhiteSpace
  }
}
