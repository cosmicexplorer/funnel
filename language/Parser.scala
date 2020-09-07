package funnel.language

import org.parboiled2._

import scala.util.matching.Regex


object FunnelPEG {
  // Define clases which are *not* AST nodes.
  sealed abstract class FunnelParseError(message: String) extends Exception(message)

  case class InvalidIndexError(index: Int, message: String) extends FunnelParseError(message)
  case class OneIndexedPosition(position: Int) {
    if (position <= 0) {
      throw InvalidIndexError(position, s"position must be greater than 0, but was $position")
    }
  }

  sealed abstract class InternalError(location: String, message: String)
      extends FunnelParseError(s"internal error in $location: $message")

  case class InvalidParameterPackConstruction(
    message: String,
  ) extends InternalError("parameter pack construction", message)

  case class InvalidIdentifierKind(
    kind: ExpressionKinds,
    id: NamedIdentifier,
    pattern: Regex,
  ) extends FunnelParseError(s"$kind identifier recognition for $id failed: did not match $pattern")

  case class ParameterPacksHaveDifferentKinds(
    entityDescription: String,
    exprs: Traversable[ExpressionKinds],
    kinds: Set[ExpressionKinds],
  ) extends InternalError(
    "checking kinds of expressions within a parameter pack",
    s"$entityDescription $exprs must all have one common expression kind, but had: $kinds")

  case class ExpressionKindsDidNotMatch(
    parameterPack: ParameterPackKinds,
    kind: ExpressionKinds,
  ) extends InternalError(
    "checking the assigned kind against the parameter pack kind",
    s"$kind did not match the kind ${parameterPack.expressionKind} from $parameterPack")

  case class FullPackExpressionKindsDidNotMatch(
    targetKind: ExpressionKinds,
    fullPackKind: ExpressionKinds,
    fullPack: FullParameterPack,
  ) extends InternalError(
    "checking the target kind against the full parameter pack kind",
    s"$targetKind did match the kind $fullPackKind from $fullPack")

  case class FailedFieldDereferenceError(
    field: LocalVar,
    message: String,
  ) extends FunnelParseError(s"failed to locate field $field: $message")

  sealed abstract class ExpressionKinds
  case object ValueKind extends ExpressionKinds
  case object TypeKind extends ExpressionKinds

  trait HasExpressionKind {
    def expressionKind: ExpressionKinds
  }

  object NamedIdentifier {
    val nonEmptyIdentifier = """.""".r
    val valueIdentifier = """[a-z\-][a-zA-Z\-_0-9]""".r
    val typeIdentifier = """[A-Z_][a-zA-Z\-_0-9]""".r
  }
  case class NamedIdentifier(override val expressionKind: ExpressionKinds, name: String)
      extends HasExpressionKind {
    import NamedIdentifier._

    name match {
      case nonEmptyIdentifier() => ()
      case _ => throw InvalidIdentifierKind(expressionKind, this, nonEmptyIdentifier)
    }
    expressionKind match {
      case ValueKind => name match {
        case valueIdentifier() => ()
        case _ => throw InvalidIdentifierKind(expressionKind, this, valueIdentifier)
      }
      case TypeKind => name match {
        case typeIdentifier() => ()
        case _ => throw InvalidIdentifierKind(expressionKind, this, typeIdentifier)
      }
    }
  }

  trait HasNamedIdentifier extends HasExpressionKind {
    override val expressionKind: ExpressionKinds = namedIdentifier.expressionKind
    def namedIdentifier: NamedIdentifier
  }

  // Capitalization is enforced for types and values, so we need to have the
  //  type parameter available even up here.
  sealed abstract class LocalIdentifierKinds extends HasNamedIdentifier
  case class LocalNamedIdentifier(override val namedIdentifier: NamedIdentifier)
      extends LocalIdentifierKinds
  case class PositionalIdentifier(
    kind: ExpressionKinds,
    position: OneIndexedPosition,
  ) extends LocalIdentifierKinds {
    override val namedIdentifier: NamedIdentifier = {
      val name = kind match {
        case ValueKind => s"-${position.position}"
        case TypeKind => s"_${position.position}"
      }
      NamedIdentifier(kind, name)
    }
  }

  sealed abstract class VarKinds extends HasNamedIdentifier
  case class GlobalVar(override val namedIdentifier: NamedIdentifier) extends VarKinds
  case class LocalVar(localId: LocalIdentifierKinds) extends VarKinds {
    override val namedIdentifier: NamedIdentifier = localId.namedIdentifier
  }

  // Define parameter packing types.
  object KindsChecker {
    def ensureNonEmptySameKind(
      entityDescription: String,
      exprs: Traversable[ExpressionKinds],
    ): ExpressionKinds = {
      if (exprs.isEmpty) {
        throw InvalidParameterPackConstruction(s"$entityDescription $exprs were empty")
      }
      val allKinds = exprs.toSet
      if (allKinds.size != 1) {
        throw ParameterPacksHaveDifferentKinds(entityDescription, exprs, allKinds)
      }
      allKinds.head
    }
  }
  import KindsChecker._

  sealed abstract class ParameterPackKinds(override val expressionKind: ExpressionKinds)
      extends HasExpressionKind {
    def intoLazyPack(): LazyParameterPack
  }
  case class EmptyParameterPack(kind: ExpressionKinds) extends ParameterPackKinds(kind) {
    override def intoLazyPack(): LazyParameterPack = LazyParameterPack(Seq())
  }
  case class StandaloneParameter(expr: Expression) extends ParameterPackKinds(expr.expressionKind) {
    override def intoLazyPack(): LazyParameterPack = LazyParameterPack(Seq((None -> expr)))
  }
  case class PositionalParameterPack(exprs: Seq[Expression])
      extends ParameterPackKinds(ensureNonEmptySameKind("positional parameter pack", exprs.map(_.expressionKind))) {
    override def intoLazyPack(): LazyParameterPack = LazyParameterPack(exprs.map(None -> _))
  }
  case class NamedParameterPack(
    fulfilled: Map[NamedIdentifier, Expression],
  ) extends ParameterPackKinds(
      ensureNonEmptySameKind("named parameter pack",
      fulfilled.flatMap { case (namedIdentifier, expr) => Seq(namedIdentifier.expressionKind, expr.expressionKind) },
    )) {
    override def intoLazyPack(): LazyParameterPack = LazyParameterPack(
      exprs = fulfilled.map {
        case (namedIdentifier, expr) => (Some(namedIdentifier) -> expr)
      }.toSeq)
  }

  // When parameters are coalesced, they come from a specific direction relative to the source pack
  // (with <=, =>, <-, or ->).
  sealed abstract class ParameterPushLocationKinds
  case object Left extends ParameterPushLocationKinds
  case object Right extends ParameterPushLocationKinds

  // NB: This is *NOT* a ParameterPackKinds!!!
  case class LazyParameterPack(
    exprs: Seq[(Option[NamedIdentifier], Expression)]
  ) extends HasExpressionKind {
    override val expressionKind = ensureNonEmptySameKind(
      "lazy parameter pack",
      exprs.flatMap {
        case (namedIdentifier, expr) => namedIdentifier.map(_.expressionKind).toSeq :+ expr.expressionKind
      })

    def merge(
      fromLocation: ParameterPushLocationKinds,
      other: LazyParameterPack,
    ): LazyParameterPack = (fromLocation, other) match {
      case (Left, LazyParameterPack(leftExprs)) => LazyParameterPack(leftExprs ++ exprs)
      case (Right, LazyParameterPack(rightExprs)) => LazyParameterPack(exprs ++ rightExprs)
    }

    def intoFullPack(): FullParameterPack = FullParameterPack(
      mapping = exprs.zipWithIndex.map {
        case ((maybeVar, expr), index) => {
          // If there's no explicit named parameter, fill out the hole with a positional param.
          val localVar = maybeVar
            .map(namedIdentifier => LocalVar(LocalNamedIdentifier(namedIdentifier))).getOrElse {
            // NB: We use 1-based indexing -- hence index + 1.
            // TODO: Test this!!!
            LocalVar(PositionalIdentifier(expressionKind, OneIndexedPosition(index + 1)))
          }
          (localVar -> expr)
        }
      }.toMap
    )
  }

  // NB: This is *ALSO NOT* a ParameterPackKinds!!!
  case class FullParameterPack(
    mapping: Map[LocalVar, Expression],
  ) extends HasExpressionKind {
    override val expressionKind = ensureNonEmptySameKind(
      "full parameter pack",
      mapping.flatMap {
        case (localVar, expr) => Seq(localVar.expressionKind, expr.expressionKind)
      })
  }


  // Define AST entities.
  sealed abstract class AstNode

  sealed abstract class Statement(override val expressionKind: ExpressionKinds)
      extends AstNode with HasExpressionKind

  sealed abstract class Expression(override val expressionKind: ExpressionKinds)
      extends AstNode with HasExpressionKind {
    def parameterPack: ParameterPackKinds
    def extractType: TypeExpression
    def functionParams: StructLiteralType

    if (expressionKind != parameterPack.expressionKind) {
      throw ExpressionKindsDidNotMatch(parameterPack, expressionKind)
    }
  }

  sealed abstract class ValueExpression extends Expression(ValueKind) {
    override val functionParams: StructLiteralType = extractType.functionParams
  }

  sealed abstract class ValueTerm extends ValueExpression {
    override val parameterPack = StandaloneParameter(this)
    override val extractType = TypePlaceholder
  }
  case class GlobalValueVar(g: GlobalVar) extends ValueTerm
  case class LocalValueVar(l: LocalVar) extends ValueTerm

  sealed abstract class ValuePackExpression(override val parameterPack: ParameterPackKinds)
      extends ValueExpression
  case object EmptyStruct extends ValuePackExpression(EmptyParameterPack(ValueKind)) {
    override val extractType: TypeExpression = EmptyStructType
  }
  case class StandaloneValueExpression(ve: ValueExpression)
      extends ValuePackExpression(StandaloneParameter(ve)) {
    override val extractType: TypeExpression = StandaloneTypeExpression(ve.extractType)
  }
  case class PositionalValueParameterPack(exprs: Seq[ValueExpression])
      extends ValuePackExpression(PositionalParameterPack(exprs)) {
    override val extractType: TypeExpression =
      PositionalTypePack(exprs.map(_.extractType))
  }
  object NamedValuePack {
    def apply(fullPack: FullParameterPack): NamedValuePack = {
      if (fullPack.expressionKind != ValueKind) {
        throw FullPackExpressionKindsDidNotMatch(ValueKind, fullPack.expressionKind, fullPack)
      }
      NamedValuePack(
        fulfilled = fullPack.mapping.map {
          case (localVar, expr) => (localVar.namedIdentifier, expr.asInstanceOf[ValueExpression])
        }
      )
    }
  }
  case class NamedValuePack(fulfilled: Map[NamedIdentifier, ValueExpression])
      extends ValuePackExpression(NamedParameterPack(fulfilled)) {
    override val extractType: TypeExpression = NamedTypePack(fulfilled.map {
      case (namedIdentifier, valueExpr) => (namedIdentifier -> valueExpr.extractType)
    })
  }

  sealed abstract class TypeExpression extends Expression(TypeKind) {
    override val extractType: TypeExpression = this
    override val functionParams: StructLiteralType = StructLiteralType.empty()
  }

  sealed abstract class TypeTerm extends TypeExpression {
    override val parameterPack = StandaloneParameter(this)
  }
  case class GlobalTypeVar(g: GlobalVar) extends TypeTerm
  case class LocalTypeVar(l: LocalVar) extends TypeTerm
  // This is used in cases where the type needs to be computed.
  case object TypePlaceholder extends TypeTerm

  sealed abstract class TypePackExpression(override val parameterPack: ParameterPackKinds)
      extends TypeExpression
  case object EmptyStructType extends TypePackExpression(EmptyParameterPack(TypeKind))
  case class StandaloneTypeExpression(te: TypeExpression)
      extends TypePackExpression(StandaloneParameter(te))
  case class PositionalTypePack(exprs: Seq[TypeExpression])
      extends TypePackExpression(PositionalParameterPack(exprs))
  case class NamedTypePack(fulfilled: Map[NamedIdentifier, TypeExpression])
      extends TypePackExpression(NamedParameterPack(fulfilled))

  // sealed abstract class ChainableValueExpression(val source: ValueExpression)
  //     extends ValueExpression
  // case class StructFieldDereference(source: ValueExpression, localVar: LocalVar(ValueKind))
  //     extends ChainableValueExpression(source) {
  //   private lazy val sourcePack = source.parameterPack
  //   private lazy val dereferencedExpression =
  //     sourcePack.intoLazyPack().fulfilled.get(localVar).getOrElse {
  //       throw FailedFieldDereferenceError(
  //         localVar, s"attempted to retrieve from source pack $sourcePack")
  //     }

  //   override def parameterPack: ParameterPackKinds =
  //     dereferencedExpression.parameterPack
  //   override def extractType: TypeExpression = dereferencedExpression.extractType
  // }

  // case class FunctionCall(source: ValueExpression, arguments: ParameterPackKinds)
  //     extends ChainableValueExpression(source) {
  //   private lazy val sourceType = source.extractType
  //   private lazy val sourceParams = sourceType.functionParams

  //   override def parameterPack: ParameterPackKinds =
  //     StandaloneParameter(this)
  //   override def extractType: TypeExpression = {
  //     // TODO: match `arguments` against `sourceParams`!
  //     ???
  //   }
  // }

  case class StructLiteralType(fulfilled: Map[NamedIdentifier, TypeExpression])
      extends TypeExpression {
    override val parameterPack: ParameterPackKinds = StandaloneParameter(this)

    def isEmpty: Boolean = fulfilled.isEmpty
  }
  object StructLiteralType {
    def empty(): StructLiteralType = StructLiteralType(Map.empty)
  }

  case class ValueWithTypeAssertion(
    ve: Option[ValueExpression] = None,
    typeAssertion: Option[TypeExpression] = None,
  )
      extends ValueExpression {
    override val parameterPack: ParameterPackKinds =
      StandaloneParameter(ve.getOrElse(EmptyStruct))
    override val extractType: TypeExpression =
      typeAssertion.getOrElse {
        ve.map(_.extractType).getOrElse(EmptyStructType)
      }
  }

  case class NamedStructField(
    localVar: LocalValueVar,
    valueWithType: ValueWithTypeAssertion,
  ) extends ValueExpression with HasNamedIdentifier {
    override val namedIdentifier: NamedIdentifier = localVar.l.localId.namedIdentifier
    override val parameterPack: ParameterPackKinds = NamedParameterPack(
      fulfilled = Map(
        namedIdentifier -> valueWithType
      )
    )
    override val extractType: TypeExpression = StructLiteralType(
      fulfilled = Map(namedIdentifier -> valueWithType.extractType)
    )
  }

  // sealed abstract class UnaryOperator[
  //   SourceKind <: ExpressionKinds,
  //   TargetKind <: ExpressionKinds,
  // ](val source: Expression[SourceKind]) extends Expression[TargetKind] {
  //   override def parameterPack: ParameterPackKinds[TargetKind] =
  //     StandaloneParameter[TargetKind](transformed)
  //   override def extractType: TypeExpression = transformed.extractType

  //   def transform(): Expression[TargetKind]
  //   private lazy val transformed = transform()
  // }
  // case class TypeExtractionOperator(ve: ValueExpression)
  //     extends UnaryOperator[ValueKind, TypeKind](ve) {
  //   override def transform(): Expression = ve.extractType
  // }

  sealed abstract class ValueLiteral(override val extractType: TypeLiteral) extends ValueExpression {
    override val parameterPack: ParameterPackKinds = StandaloneParameter(this)
  }
  case class IntegerLiteral(numberValue: Int) extends ValueLiteral(IntegerTypeLiteral)
  case class FloatLiteral(numberValue: Double) extends ValueLiteral(FloatTypeLiteral)
  case class StringLiteral(inner: String) extends ValueLiteral(StringTypeLiteral)

  sealed abstract class TypeLiteral extends TypeExpression {
    override val parameterPack: ParameterPackKinds = StandaloneParameter(this)
  }
  case object IntegerTypeLiteral extends TypeLiteral
  case object FloatTypeLiteral extends TypeLiteral
  case object StringTypeLiteral extends TypeLiteral

  // sealed abstract class ValueOperator extends ValueExpression

  case class ValueAssignment(place: GlobalValueVar, ve: ValueExpression)
      extends Statement(ValueKind)

  // sealed abstract class ValueParameterPack(val source: ValueExpression, val target: ValueExpression)
  //     extends ValueExpression
  // case class RightDoubleArrowInline(source: ValueExpression, target: ValueExpression)
  //     extends ValueParameterPack(source, target)
  // case class LeftDoubleArrowInline(source: ValueExpression, target: ValueExpression)
  //     extends ValueParameterPack(source, target)

  // case class ValueParameterSpread(source: ValueExpression) extends ValueOperator

  // sealed abstract class Value

  // case class FunctionCall(target: ValueExpression, params: )
  //     extends ValueOperator

  // case class SingleValueParameterEvaluation(vp: VarPlace, constraint: Option[TypeExpression])
  // case class ValueParameterPack(mapping: Map[VarPlace, Option[TypeExpression]])
  // object ValueParameterPack {
  //   def empty() = ValueParameterPack(Map.empty)
  // }

  // case class SingleTypeParameterEvaluation(tp: TypePlace, constraint: TypeExpression)
  // case class TypeParameterPack(mapping: Map[TypePlace, TypeExpression])
  // object TypeParameterPack {
  //   def empty() = TypeParameterPack(Map.empty)
  // }

  // sealed abstract class LeftSingleArrowTopLevel extends Statement
  // sealed abstract class TypeDefinition extends LeftSingleArrowTopLevel

  // case class StructFields(fields: Map[ExpressionKinds, Option[TypeExpression]])
  // case class StructDefinition(tyn: TypePlace, fields: StructFields) extends TypeDefinition

  // sealed abstract class ObjectStructureDeclaration extends TypeDefinition
  // case class SingleStructField(id: ExpressionKinds, constraint: Option[TypeExpression]) extends ObjectStructureDeclaration
  // case class SingleEnumCase(id: ExpressionKinds, fields: Option[StructFields]) extends ObjectStructureDeclaration

  // case class EnumCases(cases: Map[ExpressionKinds, Option[StructFields]])
  // case class EnumDefinition(tyn: TypePlace, cases: EnumCases) extends TypeDefinition

  // case class MethodTypeAnnotation(
  //   params: Map[Identifier, Option[TypeExpression]],
  //   result: TypeExpression
  // ) extends TypeExpression
  // case class MethodSignature(place: VarPlace, annot: MethodTypeAnnotation)
  // case class MethodBody(statements: Seq[Statement]) extends ValueExpression
  // case class AnonymousMethodDefinition(vpp: ValueParameterPack, expr: ValueExpression)
  //     extends ValueExpression
  // case class MethodDefinition(place: VarPlace, vpp: ValueParameterPack, expr: ValueExpression) extends ValueExpression

  // sealed abstract class RightDoubleArrow extends ValueOperator
  // case class CreateValueParameter(vpp: ValueParameterPack) extends RightDoubleArrow
  // sealed abstract class DotOperator extends ValueOperator
  // case class ExtractMember(ve: ValueExpression, id: Identifier) extends DotOperator

  // sealed abstract class TypeOperator extends TypeExpression
  // sealed abstract class LeftSingleArrow extends TypeOperator
  // case class VariableTypeAssertion(te: TypeExpression, vpe: ValuePlaceExpression)
  //     extends LeftSingleArrow
  // case class TypeParameterNameAssignment(tpe: TypePlaceExpression, tpp: TypeParameterPack)
  //     extends LeftSingleArrow
  // sealed abstract class RightSingleArrow extends TypeOperator
  // case class CreateTypeParameter(tpp: TypeParameterPack, te: TypeExpression)
  //     extends RightSingleArrow

  // sealed abstract class BasicTypeExpression extends TypeExpression
  // case class TypeName(id: Identifier) extends BasicTypeExpression
  // case class TypeNameWithParams(id: Identifier, tpp: TypeParameterPack)
  //     extends BasicTypeExpression
}

class FunnelPEG(override val input: ParserInput) extends Parser {
  import FunnelPEG._

  // Define the parsing rules.
  def Funnel: Rule1[Seq[Statement]] = rule { WhiteSpace ~ ReplOrFile ~ EOI }

  def TopLevel: Rule1[Statement] = rule {
    VariableAssignment
    // | TypeAssignment
  }

  def ReplOrFile: Rule1[Seq[Statement]] = rule {
    zeroOrMore(TopLevel) ~> ((s: Seq[Statement]) => s)
  }

  def VariableAssignment: Rule1[Statement] = rule {
    (ParseGlobalValueVar ~ "<=" ~ ParseValueExpression) ~> (
      (v: GlobalValueVar, ve: ValueExpression) => ValueAssignment(v, ve)
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
    "$" ~ ParseValueIdentifier ~> ((namedIdentifier: NamedIdentifier) => GlobalValueVar(GlobalVar(namedIdentifier)))
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
    // TODO: this won't allow any string literals with backslashes in them whatsoever!!
    "\"" ~ capture(oneOrMore(!anyOf("\"\\") | "\\\\" | "\\\"")) ~> (StringLiteral(_)) ~ "\""
  }

  // TODO: Do this right!!!
  def ParseFloatLiteral: Rule1[FloatLiteral] = rule {
    capture("0.0") ~> ((s: String) => FloatLiteral(s.toDouble))
  }

  def ParseValueLiteral: Rule1[ValueLiteral] = rule {
    ParseIntegerLiteral | ParseStringLiteral | ParseFloatLiteral
  }


  def ParseEmptyStruct: Rule1[ValueExpression] = rule {
    capture("()") ~> ((_: String) => EmptyStruct)
  }
  def ParsePositionalValueParameterPack: Rule1[PositionalValueParameterPack] = rule {
    "(" ~ oneOrMore(ParseValueExpression).separatedBy("," ~ WhiteSpace) ~ ")" ~> (
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
        LazyParameterPack(fields.flatMap(_.parameterPack.intoLazyPack().exprs)).intoFullPack()))
      | _ParseStructField ~> (
        (field: NamedStructField) => NamedValuePack(
          field.parameterPack.intoLazyPack().intoFullPack()))
    )
  }

  // def _InnerDoubleArrowValueExpression: Rule1[FunctionCall] = rule {
  //   ((_ParseChainedValueExpression ~ "=>" ~ _ParseChainedValueExpression) ~> (
  //     ((target: StandaloneValueExpression, source: StandaloneValueExpression) =>
  //       FunctionCall(source, ))
  //   ))
  // }
  // def _ParseChainedValueExpression: Rule1[StandaloneValueExpression] = rule {
  //   (("(" ~  ~ ")"))
  // }

  // def ParseStandaloneValueExpression: Rule1[StandaloneValueExpression] = rule {
  //   (("(" ~ ParseValueExpression ~ ")") ~> ((value: ValueExpression) => StandaloneValueExpression(value))
  //   | _ParseChainedValueExpression ~> ((value: StandaloneValueExpression) => value))
  // }

  // def _ParseFunctionCallBase: Rule1[ValueExpression] = rule {
  //   (("(" ~ ParseValueExpression ~ ")") ~> ((value: ValueExpression) => value)
  //   | ParseStandaloneValueExpression ~> (value: StandaloneValueExpression) => value)
  // }
  // def ParseFunctionCall: Rule1[FunctionCall] = rule {
  //   _ParseFunctionCallBase ~ ParsePositionalValueParameterPack ~> FunctionCall(_, _)
  // }

  def _ParseBaseValueExpression: Rule1[ValueExpression] = rule {
    ParseGlobalValueVar |
    ParseNamedLocalValueVar |
    ParseIntegerLiteral |
    ParseFloatLiteral |
    ParseStringLiteral |
    ParseEmptyStruct |
    ParsePositionalValueParameterPack |
    ParseNamedValuePack
  }
  def ParseValueExpression: Rule1[ValueExpression] = rule {
    (("(" ~ _ParseBaseValueExpression ~ ")") ~> ((value: ValueExpression) => value)
    | _ParseBaseValueExpression ~> ((value: ValueExpression) => value))
  }

  // def ParseValueExpression: Rule1[ValueExpression] = rule {
  //   ParseAnonymousMethod | ParseVariableRefName | ParseLiterals
  // }

  // def ParseTypeNameCreation: Rule1[TypePlace] = rule {
  //   ":" ~ ParseIdentifier ~> ((id: Identifier) => TypePlace(TypeWrapper(id), constraint = None))
  // }

  // def TypeAssignment: Rule1[TypeDefinition] = rule {
  //   StructAssignment | EnumAssignment
  // }

  // def StructAssignment: Rule1[StructDefinition] = rule {
  //   ParseTypeNameCreation ~ "<=" ~ ParseStructBody ~> (StructDefinition(_, _))
  // }

  // def EnumAssignment: Rule1[EnumDefinition] = rule {
  //   ParseTypeNameCreation ~ "<=" ~ ParseEnumBody ~> (EnumDefinition(_, _))
  // }

  // def ParseTypeAssertion: Rule1[VariableTypeAssertion] = rule {
  //   ParseVariablePlaceName ~ "<-" ~ ParseTypeExpression ~> ((varPlace, typeExpr) =>
  //   VariableTypeAssertion(typeExpr, varPlace))
  // }

  // def ParseTypeParameterEvaluation: Rule1[SingleTypeParameterEvaluation] = rule {
  //   "<-" ~ "(" ~ ParseTypeNameCreation ~ "<-" ~ ParseTypeExpression ~ ")" ~> (
  //     (typePlace, typeExpr) => SingleTypeParameterEvaluation(typePlace, typeExpr))
  // }

  // def ParseTypeParameterPack: Rule1[TypeParameterPack] = rule {
  //   zeroOrMore(ParseTypeParameterEvaluation) ~> ((params: Seq[SingleTypeParameterEvaluation]) => TypeParameterPack(
  //     mapping = params.map {
  //       case SingleTypeParameterEvaluation(ty, constraint) => (ty -> constraint)
  //     }.toMap
  //   ))
  // }

  def ParseTypeTerm: Rule1[TypeTerm] = rule {
    ParseGlobalTypeVar |
    ParseNamedLocalTypeVar
  }

  def ParseTypeExpression: Rule1[TypeExpression] = rule {
    ParseTypeTerm
  }

  // def ParseStructField: Rule1[SingleStructField] = rule {
  //   "." ~ ParseIdentifier ~ ("<-" ~ ParseTypeExpression).? ~> (
  //     (varName, maybeTypeExpr) => SingleStructField(varName, maybeTypeExpr))
  // }

  // def ParseStructBody: Rule1[StructFields] = rule {
  //   "(" ~ zeroOrMore(ParseStructField).separatedBy("," ~ WhiteSpace)  ~ ")" ~> (
  //     (fields: Seq[SingleStructField]) => StructFields(fields.map {
  //       case SingleStructField(id, constraint) => (id -> constraint)
  //     }.toMap))
  // }

  // def ParseEnumCase: Rule1[SingleEnumCase] = rule {
  //   "+" ~ ParseIdentifier ~ ParseStructBody.? ~> (
  //     (caseName, innerFields) => SingleEnumCase(caseName, innerFields)
  //   )
  // }

  // def ParseEnumBody: Rule1[EnumCases] = rule {
  //   "(" ~ oneOrMore(ParseEnumCase).separatedBy("," ~ WhiteSpace) ~ ")" ~> (
  //     (cases: Seq[SingleEnumCase]) => EnumCases(cases.map {
  //       case SingleEnumCase(id, fields) => (id -> fields)
  //     }.toMap))
  // }

  // def ParseValueParameterCreation: Rule1[SingleValueParameterEvaluation] = rule {
  //   ParseVariablePlaceName ~ ("<-" ~ ParseTypeExpression).? ~> (
  //     (varPlace, maybeTypeExpr) => SingleValueParameterEvaluation(varPlace, maybeTypeExpr)
  //   )
  // }

  // def AssociatedValueParameter: Rule1[SingleValueParameterEvaluation] = rule {
  //   (("(" ~ ParseValueParameterCreation ~ ")") ~> ((param: SingleValueParameterEvaluation) => param)
  //   | ParseValueParameterCreation ~> ((param: SingleValueParameterEvaluation) => param))
  // }

  // def ParseAnonymousMethod: Rule1[AnonymousMethodDefinition] = rule {
  //   oneOrMore(AssociatedValueParameter ~ "=>") ~ ParseValueExpression ~> (
  //     (params: Seq[SingleValueParameterEvaluation], expr: ValueExpression) => AnonymousMethodDefinition(
  //       vpp = ValueParameterPack(mapping = params.map {
  //         case SingleValueParameterEvaluation(vp, constraint) => (vp -> constraint)
  //       }.toMap),
  //       expr = expr))
  // }

  // def HandlePrecedingTypeParameterDecl = rule {
  //   zeroOrMore(HandleTypeParameter) ~> (places => TypeParameterPack(
  //     mapping = places.map {
  //       case TypePlace(ty, constraint) => (ty -> constraint)
  //     }.toMap
  //   ))
  // }

  // def ParseAnonymousMethodDefinition: Rule1[AnonymousMethodDefinition] = rule {
  //   zeroOrMore("(" ~ ParseValueParameterCreation ~ ")" ~ "=>") ~ ParseValueExpression ~> (
  //     (params: Seq[SingleValueParameterEvaluation], expr: ValueExpression) => AnonymousMethodDefinition(
  //       vpp = ValueParameterPack(
  //         params.map {
  //           case SingleValueParameterEvaluation(vp, constraint) => (vp -> constraint)
  //         }.toMap,
  //       ),
  //       expr = expr)
  //   )
  // }

  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  // See https://github.com/cosmicexplorer/parboiled2#handling-whitespace for recommendations on
  // handling whitespace with PEG parsers (namely, by matching whitespace strictly after every
  // terminal).
  implicit def whitespaceLiteralConverter(s: String): Rule0 = rule {
    str(s) ~ WhiteSpace
  }
}
