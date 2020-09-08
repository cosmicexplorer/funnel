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

  case class InvalidCaseName(name: String, pattern: Regex)
      extends FunnelParseError(s"case names must always match the pattern $pattern: was $name")

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
    s"$targetKind did not match the kind $fullPackKind from $fullPack")

  case class FailedParameterMatchupError(
    params: StructLiteralType,
    arguments: ParameterPackKinds,
    failedMatch: FailedParameterPackMatch,
  ) extends FunnelParseError(
    s"params $params did not match arguments $arguments. failed matches were: $failedMatch")

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
    val nonEmptyIdentifier = """.+""".r
    val valueIdentifier = """[a-z\-][a-zA-Z\-_0-9]*""".r
    val typeIdentifier = """[A-Z_][a-zA-Z\-_0-9]*""".r
  }
  case class NamedIdentifier(kind: ExpressionKinds, name: String)
      extends HasExpressionKind {
    override def expressionKind = kind

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

  trait HasNamedIdentifier {
    def namedIdentifier: NamedIdentifier
  }

  trait HasIdentifierDrivenKind extends HasNamedIdentifier with HasExpressionKind {
    override def expressionKind = namedIdentifier.expressionKind
  }

  // Capitalization is enforced for types and values, so we need to have the
  //  type parameter available even up here.
  sealed abstract class LocalIdentifierKinds extends HasIdentifierDrivenKind
  case class LocalNamedIdentifier(id: NamedIdentifier)
      extends LocalIdentifierKinds {
    override def namedIdentifier = id
  }
  case class PositionalIdentifier(
    kind: ExpressionKinds,
    position: OneIndexedPosition,
  ) extends LocalIdentifierKinds {
    override def namedIdentifier: NamedIdentifier = {
      val name = kind match {
        case ValueKind => s"-${position.position}"
        case TypeKind => s"_${position.position}"
      }
      NamedIdentifier(kind, name)
    }
  }

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
  case class AlternationCase(name: AlternationCaseName, tpe: TypePackExpression)

  sealed abstract class VarKinds extends HasIdentifierDrivenKind
  case class GlobalVar(id: NamedIdentifier) extends VarKinds {
    override def namedIdentifier = id
  }
  case class LocalVar(localId: LocalIdentifierKinds) extends VarKinds {
    override def namedIdentifier: NamedIdentifier = localId.namedIdentifier
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

  sealed abstract class ParameterPackKinds extends HasExpressionKind {
    def intoLazyPack(): LazyParameterPack
  }
  case class EmptyParameterPack(kind: ExpressionKinds) extends ParameterPackKinds {
    override def expressionKind = kind
    override def intoLazyPack(): LazyParameterPack = LazyParameterPack(Seq())
  }
  case class StandaloneParameter(expr: Expression) extends ParameterPackKinds {
    override def expressionKind = expr.expressionKind
    override def intoLazyPack(): LazyParameterPack = LazyParameterPack(Seq((None -> expr)))
  }
  case class PositionalParameterPack(exprs: Seq[Expression]) extends ParameterPackKinds {
    override def expressionKind = ensureNonEmptySameKind(
      "positional parameter pack",
      exprs.map(_.expressionKind))
    override def intoLazyPack(): LazyParameterPack = LazyParameterPack(exprs.map(None -> _))
  }
  case class NamedParameterPack(
    fulfilled: Map[NamedIdentifier, Expression],
  ) extends ParameterPackKinds {
    override def expressionKind = ensureNonEmptySameKind(
      "named parameter pack",
      fulfilled.map { case (_, expr) => expr.expressionKind })
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
    override def expressionKind = ensureNonEmptySameKind(
      "lazy parameter pack",
      exprs.map { case (_, expr) => expr.expressionKind })

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
    override def expressionKind = ensureNonEmptySameKind(
      "full parameter pack",
      mapping.map { case (_, expr) => expr.expressionKind })

    def isEmpty: Boolean = mapping.isEmpty
  }

  case class ToBeTypeMatched(expr: Expression, typeExpr: TypeExpression)
  object TypeMatchups {
    def empty(): TypeMatchups = TypeMatchups(Map.empty)
  }
  case class TypeMatchups(typeMatchups: Map[NamedIdentifier, ToBeTypeMatched])

  sealed abstract class ParameterPackMatchResult
  case class SuccessfulParameterPackMatch(matchups: TypeMatchups)
      extends ParameterPackMatchResult
  case class FailedParameterPackMatch(
    missingParams: StructLiteralType,
    tooManyParams: FullParameterPack,
  ) extends ParameterPackMatchResult

  // Define AST entities.
  sealed abstract class AstNode

  sealed abstract class Statement(kind: ExpressionKinds)
      extends AstNode with HasExpressionKind {
    override def expressionKind = kind
  }

  trait HasParameterPack {
    def parameterPack: ParameterPackKinds
    def functionParams: StructLiteralType
    def pendingTypeMatchups: TypeMatchups
  }

  trait HasType {
    def extractType: TypeExpression
  }

  sealed abstract class Expression extends AstNode
      with HasParameterPack
      with HasType
      with HasExpressionKind

  sealed abstract class ValueExpression extends Expression {
    override def expressionKind = ValueKind
    override def functionParams: StructLiteralType = extractType.functionParams
    override def pendingTypeMatchups = TypeMatchups.empty()
  }

  sealed abstract class ValueTerm extends ValueExpression {
    override def parameterPack = StandaloneParameter(this)
    override def extractType = TypePlaceholder
  }
  case class GlobalValueVar(g: GlobalVar) extends ValueTerm
  case class LocalValueVar(l: LocalVar) extends ValueTerm

  sealed abstract class ValuePackExpression(pack: ParameterPackKinds)
      extends ValueExpression {
    override def parameterPack = pack
  }
  case object EmptyStruct extends ValuePackExpression(EmptyParameterPack(ValueKind)) {
    override def extractType: TypeExpression = EmptyStructType
  }
  case class StandaloneValueExpression(ve: ValueExpression)
      extends ValuePackExpression(StandaloneParameter(ve)) {
    override def extractType: TypeExpression = StandaloneTypeExpression(ve.extractType)
  }
  case class PositionalValueParameterPack(exprs: Seq[ValueExpression])
      extends ValuePackExpression(PositionalParameterPack(exprs)) {
    override def extractType: TypeExpression =
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
    override def extractType: TypeExpression = NamedTypePack(fulfilled.map {
      case (namedIdentifier, valueExpr) => (namedIdentifier -> valueExpr.extractType)
    })
  }

  sealed abstract class ComplexValue extends ValueExpression {
    override def parameterPack: ParameterPackKinds = StandaloneParameter(this)
    override def extractType: TypeExpression = TypePlaceholder
  }

  sealed abstract class ChainableValueStart(val startTerm: ValueTerm) extends ComplexValue
  case class GlobalChainValueStart(gv: GlobalValueVar) extends ChainableValueStart(gv)
  case class LocalChainValueStart(lv: LocalValueVar) extends ChainableValueStart(lv)

  sealed abstract class ChainableValueIntermediateExpression extends ComplexValue
  case class StructFieldDereference(localVar: LocalValueVar)
      extends ChainableValueIntermediateExpression

  case class ChainedValueExpression(
    start: ChainableValueStart,
    chain: Seq[ChainableValueIntermediateExpression],
  ) extends ComplexValue

  case class FunctionCall(source: ValueExpression, arguments: ParameterPackKinds)
      extends ValueExpression {
    override def parameterPack: ParameterPackKinds = StandaloneParameter(this)
    override def extractType: TypeExpression = source.extractType
    override def pendingTypeMatchups: TypeMatchups =
      source.functionParams.matchAgainstParameterPack(arguments) match {
        case failed: FailedParameterPackMatch => throw FailedParameterMatchupError(
          source.functionParams, arguments, failed)
        case SuccessfulParameterPackMatch(matchups) => matchups
      }
  }

  sealed abstract class TypeExpression extends Expression {
    override def expressionKind = TypeKind
    override def extractType: TypeExpression = this
    override def functionParams: StructLiteralType = StructLiteralType.empty()
    override def pendingTypeMatchups = TypeMatchups.empty()
  }

  sealed abstract class TypeTerm extends TypeExpression {
    override def parameterPack = StandaloneParameter(this)
  }
  case class GlobalTypeVar(g: GlobalVar) extends TypeTerm
  case class LocalTypeVar(l: LocalVar) extends TypeTerm
  // This is used in cases where the type needs to be computed.
  case object TypePlaceholder extends TypeTerm

  sealed abstract class TypePackExpression(pack: ParameterPackKinds)
      extends TypeExpression {
    override def parameterPack = pack
  }
  case object EmptyStructType extends TypePackExpression(EmptyParameterPack(TypeKind))
  case class StandaloneTypeExpression(te: TypeExpression)
      extends TypePackExpression(StandaloneParameter(te))
  case class PositionalTypePack(exprs: Seq[TypeExpression])
      extends TypePackExpression(PositionalParameterPack(exprs))
  case class NamedTypePack(fulfilled: Map[NamedIdentifier, TypeExpression])
      extends TypePackExpression(NamedParameterPack(fulfilled))

  sealed abstract class TypeAlternationExpression extends TypeExpression {
    override def parameterPack = StandaloneParameter(this)
  }
  case class TypeAlternation(cases: Map[AlternationCaseName, TypePackExpression])
      extends TypeAlternationExpression

  sealed abstract class ValueAlternationExpression extends ValueExpression {
    override def parameterPack = StandaloneParameter(this)
  }
  object ValueAlternation {
    def apply(ta: TypeAlternation): ValueAlternation = ValueAlternation(ta.cases)
  }
  case class ValueAlternation(cases: Map[AlternationCaseName, TypePackExpression])
      extends ValueAlternationExpression {
    override def extractType = TypeAlternation(cases)
  }

  object StructLiteralType {
    def empty(): StructLiteralType = StructLiteralType(Map.empty[NamedIdentifier, TypeExpression])

    def apply(fullPack: FullParameterPack): StructLiteralType = {
      if (fullPack.expressionKind != TypeKind) {
        throw FullPackExpressionKindsDidNotMatch(TypeKind, fullPack.expressionKind, fullPack)
      }
      StructLiteralType(
        fulfilled = fullPack.mapping.map {
          case (localVar, expr) => (localVar.namedIdentifier, expr.asInstanceOf[TypeExpression])
        }
      )
    }
  }
  case class StructLiteralType(fulfilled: Map[NamedIdentifier, TypeExpression])
      extends TypePackExpression(NamedParameterPack(fulfilled)) {
    def matchAgainstParameterPack(pack: ParameterPackKinds): ParameterPackMatchResult = {
      val fullPack = pack.intoLazyPack().intoFullPack()
      val asIdentifiers = fullPack.mapping.map {
        case (localVar, expr) => (localVar.namedIdentifier -> expr)
      }
      val missingParams = StructLiteralType(fulfilled -- asIdentifiers.keys)
      val tooManyParams = FullParameterPack(
        fullPack.mapping -- fulfilled.keys.map(namedIdentifier => LocalVar(LocalNamedIdentifier(namedIdentifier))))
      if (missingParams.isEmpty && tooManyParams.isEmpty) {
        val matchedTypeExprs: Seq[(NamedIdentifier, ToBeTypeMatched)] =
          asIdentifiers.keys.map { namedIdentifier =>
            (namedIdentifier -> ToBeTypeMatched(
              asIdentifiers(namedIdentifier),
              fulfilled(namedIdentifier)))
          }.toSeq
        SuccessfulParameterPackMatch(TypeMatchups(matchedTypeExprs.toMap))
      } else {
        FailedParameterPackMatch(missingParams, tooManyParams)
      }
    }

    def isEmpty: Boolean = fulfilled.isEmpty
  }

  object StructLiteralValue {
    def apply(slt: StructLiteralType): StructLiteralValue = StructLiteralValue(slt.fulfilled)
  }
  case class StructLiteralValue(params: Map[NamedIdentifier, TypeExpression])
      extends ValueExpression {
    override def parameterPack: ParameterPackKinds = NamedParameterPack(params)
    override def extractType: TypeExpression = functionParams
    override def functionParams: StructLiteralType = StructLiteralType(params)
    def dereferenceParamsPack(): NamedValuePack = NamedValuePack(
      parameterPack.intoLazyPack().intoFullPack().mapping.map {
        case (localVar, _) => (localVar.namedIdentifier, LocalValueVar(localVar))
      }
    )
  }

  case class ValueWithTypeAssertion(
    ve: Option[ValueExpression] = None,
    typeAssertion: Option[TypeExpression] = None,
  )
      extends ValueExpression {
    override def parameterPack: ParameterPackKinds =
      StandaloneParameter(ve.getOrElse(EmptyStruct))
    override def extractType: TypeExpression =
      typeAssertion.getOrElse {
        ve.map(_.extractType).getOrElse(EmptyStructType)
      }
  }

  case class NamedStructField(
    localVar: LocalValueVar,
    valueWithType: ValueWithTypeAssertion,
  ) extends ValueExpression with HasNamedIdentifier {
    override def namedIdentifier: NamedIdentifier = localVar.l.localId.namedIdentifier
    override def parameterPack: ParameterPackKinds = NamedParameterPack(
      fulfilled = Map(
        namedIdentifier -> valueWithType
      )
    )
    override def extractType: TypeExpression = StructLiteralType(
      fulfilled = Map(namedIdentifier -> valueWithType.extractType)
    )
  }

  case class NamedStructFieldDecl(
    localVar: LocalValueVar,
    typeAssertion: TypeExpression,
  ) extends TypeExpression with HasNamedIdentifier {
    override def namedIdentifier: NamedIdentifier = localVar.l.localId.namedIdentifier
    override def parameterPack: ParameterPackKinds = NamedParameterPack(
      fulfilled = Map(
        namedIdentifier -> typeAssertion
      )
    )
  }

  sealed abstract class ValueLiteral(ty: TypeLiteral) extends ValueExpression {
    override def extractType = ty
    override def parameterPack: ParameterPackKinds = StandaloneParameter(this)
  }
  case class IntegerLiteral(numberValue: Int) extends ValueLiteral(IntegerTypeLiteral)
  case class FloatLiteral(numberValue: Double) extends ValueLiteral(FloatTypeLiteral)
  case class StringLiteral(inner: String) extends ValueLiteral(StringTypeLiteral)

  sealed abstract class TypeLiteral extends TypeExpression {
    override def parameterPack: ParameterPackKinds = StandaloneParameter(this)
  }
  case object IntegerTypeLiteral extends TypeLiteral
  case object FloatTypeLiteral extends TypeLiteral
  case object StringTypeLiteral extends TypeLiteral

  sealed abstract class MethodSignature extends TypeExpression
  case class MethodType(
    params: StructLiteralType,
    output: TypeExpression,
  ) extends MethodSignature {
    override def parameterPack = output.parameterPack
    override def functionParams = params
  }

  sealed abstract class MethodDefinition extends ValueExpression
  case class AnonymousMethod(
    params: StructLiteralValue,
    output: ValueExpression,
  ) extends MethodDefinition {
    override def parameterPack = output.parameterPack
    override def extractType = MethodType(
      StructLiteralType(params.params),
      output.extractType)
  }

  sealed abstract class ValueStatement extends Statement(ValueKind)
  case class ValueAssignment(place: GlobalValueVar, ve: ValueExpression) extends ValueStatement
  case class ValueAssertion(lhs: ValueExpression, rhs: ValueExpression) extends ValueStatement
  // case class ModuleScopeImplicitRegistration(place: GlobalValueVar, ve: ValueExpression)
  //     extends ValueStatement

  // case class LocalImplicitParameterDecl(
  //   localVar: LocalValueVar,
  // ) extends TypeExpression with HasNamedIdentifier

  sealed abstract class TypeStatement extends Statement(TypeKind)
  case class TypeAssignment(place: GlobalTypeVar, te: TypeExpression) extends TypeStatement
  case class TypeAssertion(lhs: TypeExpression, rhs: TypeExpression) extends TypeStatement
}

class FunnelPEG(override val input: ParserInput) extends Parser {
  import FunnelPEG._

  // Define the parsing rules.
  def Funnel: Rule1[Seq[Statement]] = rule { WhiteSpace ~ ReplOrFile ~ EOI }

  def TopLevel: Rule1[Statement] = rule {
    ParseValueAssignment | ParseValueAssertion | ParseTypeAssignment | ParseTypeAssertion
  }

  def ReplOrFile: Rule1[Seq[Statement]] = rule {
    zeroOrMore(TopLevel) ~> ((s: Seq[Statement]) => s)
  }

  def ParseValueAssignment: Rule1[ValueAssignment] = rule {
    (ParseGlobalValueVar ~ "<=" ~ ParseValueExpression) ~> (
      (v: GlobalValueVar, ve: ValueExpression) => ValueAssignment(v, ve)
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
    "(" ~ oneOrMore(MaybeParseStandaloneValueExpression).separatedBy(",") ~ ")" ~> (
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

  def ParseParameterPack: Rule1[ValuePackExpression] = rule {
    ParseEmptyStruct |
    ParsePositionalValueParameterPack |
    ParseNamedValuePack
  }

  def _InnerDoubleArrowValueExpression: Rule1[FunctionCall] = rule {
    ((MaybeParseStandaloneValueExpression ~ "=>" ~ MaybeParseStandaloneValueExpression) ~> (
      (target: ValueExpression, source: ValueExpression) =>
        FunctionCall(source, StandaloneParameter(target)))
      | (MaybeParseStandaloneValueExpression ~ "<=" ~ MaybeParseStandaloneValueExpression) ~> (
        (source: ValueExpression, target: ValueExpression) =>
        FunctionCall(source, StandaloneParameter(target))))
  }

  def _ParseChainedValueExpressionHelper: Rule1[Seq[ChainableValueIntermediateExpression]] = rule {
    zeroOrMore(ParseNamedLocalValueVar) ~> (
      (s: Seq[LocalValueVar]) => s.map(StructFieldDereference(_)))
  }
  def _ParseChainedValueExpression: Rule1[ChainedValueExpression] = rule {
    ((ParseGlobalValueVar ~ _ParseChainedValueExpressionHelper) ~> (
      (gv: GlobalValueVar, chain: Seq[ChainableValueIntermediateExpression]) =>
        ChainedValueExpression(start = GlobalChainValueStart(gv), chain = chain))
      | (ParseNamedLocalValueVar ~ _ParseChainedValueExpressionHelper) ~> (
        (lv: LocalValueVar, chain: Seq[ChainableValueIntermediateExpression]) =>
        ChainedValueExpression(start = LocalChainValueStart(lv), chain = chain)))
  }

  def _ParseSimpleValueExpression: Rule1[ValueExpression] = rule {
    ParseValueLiteral |
    ParseEmptyStruct
  }

  def _ParseStandaloneValueExpressionHelper: Rule1[StandaloneValueExpression] = rule {
    ((_ParseChainedValueExpression) ~> (
      (chainedValue: ChainedValueExpression) => StandaloneValueExpression(chainedValue)))
  }
  def MaybeParseStandaloneValueExpression: Rule1[ValueExpression] = rule {
    (("(" ~ _ParseBaseValueExpression ~ ")") ~> ((value: ValueExpression) => value)
      // | _ParseStandaloneValueExpressionHelper ~> ((value: StandaloneValueExpression) => value)
      | (_ParseSimpleValueExpression) ~> (
        (simpleValue: ValueExpression) => simpleValue)
    )
  }

  def ParseFunctionCall: Rule1[FunctionCall] = rule {
    ((MaybeParseStandaloneValueExpression ~ optional("<=") ~ ParseParameterPack) ~> (
      (ve: ValueExpression, vpe: ValuePackExpression) => FunctionCall(ve, vpe.parameterPack)))
  }

  def _ParseBaseValueExpression: Rule1[ValueExpression] = rule {
    ParseStructDeclValueKeepingAfter |
    ParseGlobalValueVar |
    ParseNamedLocalValueVar |
    ParseValueLiteral |
    ParseEnumDeclValue |
    ParseEmptyStruct |
    ParsePositionalValueParameterPack |
    ParseNamedValuePack |
    ParseFunctionCall
  }
  def ParseValueExpression: Rule1[ValueExpression] = rule {
    (("(" ~ _ParseBaseValueExpression ~ ")") ~> ((value: ValueExpression) => value)
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
        (decl: NamedStructFieldDecl) => StructLiteralType(
          decl.parameterPack.intoLazyPack().intoFullPack())))
      | ("(" ~ oneOrMore(_ParseReducedStructFieldDecl).separatedBy(",") ~ ")") ~> (
        (fields: Seq[NamedStructFieldDecl]) => StructLiteralType(
        LazyParameterPack(fields.flatMap(_.parameterPack.intoLazyPack().exprs)).intoFullPack())
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

  def _KnownTypeExtractableExpressions: Rule1[TypeExpression] = rule {
    ((ParseReducedStructDecl | ParseReducedEnumDecl) ~> ((te: TypeExpression) => te)
      // | ParseStructDeclValueKeepingAfter ~> ((anon: AnonymousMethod) => anon.extractType)
      | ParseValueExpression ~> ((ve: ValueExpression) => ve.extractType))
  }
  // This implements the "type extraction operator" <...>.
  def _ParseTypeExtractableExpressions: Rule1[TypeExpression] = rule {
    "<" ~ _KnownTypeExtractableExpressions ~ ">"
  }

  def _ParseBaseTypeExpression: Rule1[TypeExpression] = rule {
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

  def ParseEmptyStructDecl: Rule1[TypePackExpression] = rule {
    capture("()".?) ~> ((_: String) => EmptyStructType)
  }
  def ParsePositionalStructDecl: Rule1[PositionalTypePack] = rule {
    "(" ~ oneOrMore(ParseTypeExpression).separatedBy(",") ~ ")" ~> (
      (types: Seq[TypeExpression]) => PositionalTypePack(types)
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
  def ParseStructDecl: Rule1[StructLiteralType] = rule {
    (((_ParseInlineFieldDecl) ~> (
        (decl: NamedStructFieldDecl) => StructLiteralType(
          decl.parameterPack.intoLazyPack().intoFullPack())))
      | ("(" ~ oneOrMore(_ParseStructFieldDecl).separatedBy(",") ~ ")") ~> (
        (fields: Seq[NamedStructFieldDecl]) => StructLiteralType(
        LazyParameterPack(fields.flatMap(_.parameterPack.intoLazyPack().exprs)).intoFullPack())
    ))
  }

  def ParseStructDeclValue: Rule1[StructLiteralValue] = rule {
    ParseStructDecl ~> ((decl: StructLiteralType) => StructLiteralValue(decl))
  }

  def ParseStructDeclValueKeepingAfter: Rule1[AnonymousMethod] = rule {
    ((ParseStructDeclValue ~ "=>" ~ ParseValueExpression ~> (
      (slv: StructLiteralValue, ve: ValueExpression) => AnonymousMethod(slv, ve)
    ))
      | ((ParseStructDeclValue ~ EOI ~> (
        (slv: StructLiteralValue) => AnonymousMethod(
          slv,
          slv.dereferenceParamsPack())
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

  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
  // See https://github.com/cosmicexplorer/parboiled2#handling-whitespace for recommendations on
  // handling whitespace with PEG parsers (namely, by matching whitespace strictly after every
  // terminal).
  implicit def whitespaceLiteralConverter(s: String): Rule0 = rule {
    str(s) ~ WhiteSpace
  }
}
