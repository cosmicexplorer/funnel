package funnel.language

import org.parboiled2._


object FunnelPEG {
  // Define AST entities.
  case class IdentifierWrapper(name: String)
  case class VarWrapper(id: IdentifierWrapper)
  case class TypeWrapper(id: IdentifierWrapper)
  case class TypeclassWrapper(id: IdentifierWrapper)

  case class SingleValueParameterEvaluation(vp: VarPlace, constraint: Option[TypeExpression])
  case class ValueParameterPack(mapping: Map[VarPlace, Option[TypeExpression]])
  object ValueParameterPack {
    def empty() = ValueParameterPack(Map.empty)
  }

  case class SingleTypeParameterEvaluation(tp: TypePlace, constraint: TypeExpression)
  case class TypeParameterPack(mapping: Map[TypePlace, TypeExpression])
  object TypeParameterPack {
    def empty() = TypeParameterPack(Map.empty)
  }

  sealed abstract class AstNode

  sealed abstract class Statement extends AstNode
  sealed abstract class Expression extends AstNode
  sealed abstract class ValueExpression extends Expression
  sealed abstract class TypeExpression extends Expression

  // case class MacroOperator extends Expression

  sealed abstract class PlaceExpression[T] extends Expression {
    def getTarget: T
  }

  sealed abstract class ValuePlaceExpression(varWrapper: VarWrapper)
      extends PlaceExpression[VarWrapper] {
    override val getTarget = varWrapper
  }
  case class VarPlace(varWrapper: VarWrapper) extends ValuePlaceExpression(varWrapper)

  sealed abstract class TypePlaceExpression(ty: TypeWrapper, constraint: Option[TypeExpression])
      extends PlaceExpression[TypeWrapper] {
    override val getTarget = ty
  }
  case class TypePlace(ty: TypeWrapper, constraint: Option[TypeExpression])
      extends TypePlaceExpression(ty, constraint)
  case class TypePlaceWithParams(
    ty: TypeWrapper,
    constraint: Option[TypeExpression],
    tpp: TypeParameterPack
  ) extends TypePlaceExpression(ty, constraint)

  sealed abstract class TypeclassPlaceExpression(tyc: TypeclassWrapper, constraint: Option[TypeExpression])
      extends PlaceExpression[TypeclassWrapper] {
    override val getTarget = tyc
  }
  case class TypeclassPlace(tyc: TypeclassWrapper, constraint: Option[TypeExpression])
      extends TypeclassPlaceExpression(tyc, constraint)
  case class TypeclassPlaceWithParams(
    tyc: TypeclassWrapper,
    constraint: Option[TypeExpression],
    tpp: TypeParameterPack
  ) extends TypeclassPlaceExpression(tyc, constraint)

  sealed abstract class BasicValueExpression extends ValueExpression
  case class Variable(varWrapper: VarWrapper) extends BasicValueExpression

  sealed abstract class Literal extends BasicValueExpression
  case class NumericLiteral(numberValue: Int) extends Literal
  case class StringLiteral(inner: String) extends Literal

  sealed abstract class ValueOperator extends ValueExpression

  sealed abstract class LeftDoubleArrow extends Statement
  case class ValueAssignment(vpe: ValuePlaceExpression, ve: ValueExpression) extends LeftDoubleArrow

  sealed abstract class TypeDefinition extends LeftDoubleArrow

  case class StructFields(fields: Map[IdentifierWrapper, Option[TypeExpression]])
  case class StructDefinition(tyn: TypePlace, fields: StructFields) extends TypeDefinition

  sealed abstract class ObjectStructureDeclaration extends TypeDefinition
  case class SingleStructField(id: IdentifierWrapper, constraint: Option[TypeExpression]) extends ObjectStructureDeclaration
  case class SingleEnumCase(id: IdentifierWrapper, fields: Option[StructFields]) extends ObjectStructureDeclaration

  case class EnumCases(cases: Map[IdentifierWrapper, Option[StructFields]])
  case class EnumDefinition(tyn: TypePlace, cases: EnumCases) extends TypeDefinition

  case class MethodTypeAnnotation(
    params: Map[IdentifierWrapper, Option[TypeExpression]],
    result: TypeExpression
  ) extends TypeExpression
  case class MethodSignature(place: VarPlace, annot: MethodTypeAnnotation)
  case class MethodBody(statements: Seq[Statement]) extends ValueExpression
  case class TypeclassMethodDefnAnonymous(vpp: ValueParameterPack, expr: ValueExpression) extends ValueExpression
  case class MethodDefinition(place: VarPlace, vpp: ValueParameterPack, expr: ValueExpression) extends ValueExpression

  sealed abstract class TypeclassDefn extends LeftDoubleArrow
  case class TypeclassDefnFields(fields: Map[VarPlace, MethodTypeAnnotation])
  case class TypeclassDefinition(tyn: TypeclassPlaceExpression, fields: TypeclassDefnFields)
      extends TypeclassDefn

  sealed abstract class TypeclassImpl extends LeftDoubleArrow
  case class TypeclassImplNode(
    tyn: TypeclassWrapper,
    tpp: TypeParameterPack,
    methods: Map[VarPlace, ValueExpression]
  ) extends TypeclassImpl

  sealed abstract class RightDoubleArrow extends ValueOperator
  case class CreateValueParameter(vpp: ValueParameterPack) extends RightDoubleArrow
  sealed abstract class DotOperator extends ValueOperator
  case class ExtractMember(ve: ValueExpression, id: IdentifierWrapper) extends DotOperator

  sealed abstract class TypeOperator extends TypeExpression
  sealed abstract class LeftSingleArrow extends TypeOperator
  case class VariableTypeAssertion(te: TypeExpression, vpe: ValuePlaceExpression)
      extends LeftSingleArrow
  case class TypeParameterNameAssignment(tpe: TypePlaceExpression, tpp: TypeParameterPack)
      extends LeftSingleArrow
  sealed abstract class RightSingleArrow extends TypeOperator
  case class CreateTypeParameter(tpp: TypeParameterPack, te: TypeExpression)
      extends RightSingleArrow

  sealed abstract class BasicTypeExpression extends TypeExpression
  case class TypeName(id: IdentifierWrapper) extends BasicTypeExpression
  case class TypeNameWithParams(id: IdentifierWrapper, tpp: TypeParameterPack)
      extends BasicTypeExpression
  case class Typeclass(id: IdentifierWrapper) extends BasicTypeExpression
  case class TypeclassWithParams(id: IdentifierWrapper, tpp: TypeParameterPack)
      extends BasicTypeExpression
}

class FunnelPEG(override val input: ParserInput) extends Parser {
  import FunnelPEG._

  // Define the parsing rules.
  def Funnel: Rule1[Seq[Statement]] = rule { WhiteSpace ~ ReplOrFile ~ EOI }

  def TopLevel: Rule1[Statement] = rule {

    VariableAssignment | TypeAssignment | ParseTypeclassDefinition | TypeclassImplementation
  }

  def ReplOrFile: Rule1[Seq[Statement]] = rule {
    zeroOrMore(TopLevel) ~> ((s: Seq[Statement]) => s)
  }

  def ParseIdentifier: Rule1[IdentifierWrapper] = rule {
    capture(oneOrMore(CharPredicate.AlphaNum | anyOf("-_"))) ~ WhiteSpace ~> ((s: String) => IdentifierWrapper(s))
  }

  def ParseTypeName: Rule1[TypeName] = rule {
    ":" ~ ParseIdentifier ~> (TypeName(_))
  }

  def ParseVariablePlaceName: Rule1[VarPlace] = rule {
    "$" ~ ParseIdentifier ~> ((id: IdentifierWrapper) => VarPlace(VarWrapper(id)))
  }

  def ParseVariableRefName: Rule1[Variable] = rule {
    "$" ~ ParseIdentifier ~> ((id: IdentifierWrapper) => Variable(VarWrapper(id)))
  }

  def VariableAssignment: Rule1[ValueAssignment] = rule {
    ParseVariablePlaceName ~ "<=" ~ ParseValueExpression ~> (ValueAssignment(_, _))
  }

  def ParseNumericLiterals: Rule1[NumericLiteral] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((s: String) => NumericLiteral(s.toInt))
  }

  def ParseStringLiterals: Rule1[StringLiteral] = rule { "\"" ~ capture(oneOrMore(!anyOf("\"\\"))) ~> (StringLiteral(_)) ~ "\"" }

  def ParseLiterals: Rule1[Literal] = rule { ParseNumericLiterals | ParseStringLiterals }

  def ParseValueExpression: Rule1[ValueExpression] = rule {
    ParseVariableRefName | ParseLiterals
  }

  def ParseTypeNameCreation: Rule1[TypePlace] = rule {
    ":" ~ ParseIdentifier ~> ((id: IdentifierWrapper) => TypePlace(TypeWrapper(id), constraint = None))
  }

  def TypeAssignment: Rule1[TypeDefinition] = rule {
    StructAssignment | EnumAssignment
  }

  def StructAssignment: Rule1[StructDefinition] = rule {
    ParseTypeNameCreation ~ "<=" ~ ParseStructBody ~> (StructDefinition(_, _))
  }

  def EnumAssignment: Rule1[EnumDefinition] = rule {
    ParseTypeNameCreation ~ "<=" ~ ParseEnumBody ~> (EnumDefinition(_, _))
  }

  def ParseTypeAssertion: Rule1[VariableTypeAssertion] = rule {
    ParseVariablePlaceName ~ "<-" ~ ParseTypeExpression ~> ((varPlace, typeExpr) =>
    VariableTypeAssertion(typeExpr, varPlace))
  }

  def ParseTypeParameterEvaluation: Rule1[SingleTypeParameterEvaluation] = rule {
    "<-" ~ "(" ~ ParseTypeNameCreation ~ "<-" ~ ParseTypeExpression ~ ")" ~> (
      (typePlace, typeExpr) => SingleTypeParameterEvaluation(typePlace, typeExpr))
  }

  def ParseTypeParameterPack: Rule1[TypeParameterPack] = rule {
    zeroOrMore(ParseTypeParameterEvaluation) ~> ((params: Seq[SingleTypeParameterEvaluation]) => TypeParameterPack(
      mapping = params.map {
        case SingleTypeParameterEvaluation(ty, constraint) => (ty -> constraint)
      }.toMap
    ))
  }

  def ParseTypeExpression: Rule1[TypeExpression] = rule {
    ParseTypeName
  }

  def ParseStructField: Rule1[SingleStructField] = rule {
    "." ~ ParseIdentifier ~ ("<-" ~ ParseTypeExpression).? ~> (
      (varName, maybeTypeExpr) => SingleStructField(varName, maybeTypeExpr))
  }

  def ParseStructBody: Rule1[StructFields] = rule {
    "(" ~ zeroOrMore(ParseStructField).separatedBy(", " ~ WhiteSpace)  ~ ")" ~> (
      (fields: Seq[SingleStructField]) => StructFields(fields.map {
        case SingleStructField(id, constraint) => (id -> constraint)
      }.toMap))
  }

  def ParseEnumCase: Rule1[SingleEnumCase] = rule {
    "+" ~ ParseIdentifier ~ ParseStructBody.? ~> (
      (caseName, innerFields) => SingleEnumCase(caseName, innerFields)
    )
  }

  def ParseEnumBody: Rule1[EnumCases] = rule {
    "(" ~ oneOrMore(ParseEnumCase).separatedBy(", " ~ WhiteSpace) ~ ")" ~> (
      (cases: Seq[SingleEnumCase]) => EnumCases(cases.map {
        case SingleEnumCase(id, fields) => (id -> fields)
      }.toMap))
  }

  def ParseTypeclass: Rule1[Typeclass] = rule {
    "&" ~ ParseIdentifier ~> (Typeclass(_))
  }

  def ParseTypeclassCreation: Rule1[TypeclassPlaceExpression] = rule {
    "&" ~ ParseIdentifier ~> ((id: IdentifierWrapper) => TypeclassPlace(TypeclassWrapper(id), None))
  }

  def ParseTypeclassDefinition: Rule1[TypeclassDefn] = rule {
    ParseTypeclassCreation ~ "<=" ~ ParseTypeclassBody ~> (TypeclassDefinition(_, _))
  }

  def ParseValueParameterCreation: Rule1[SingleValueParameterEvaluation] = rule {
    ParseVariablePlaceName ~ ("<-" ~ ParseTypeExpression).? ~> (
      (varPlace, maybeTypeExpr) => SingleValueParameterEvaluation(varPlace, maybeTypeExpr)
    )
  }

  def ParseMethodTypeAnnotation: Rule1[MethodTypeAnnotation] = rule {
    zeroOrMore("(" ~ ParseValueParameterCreation ~ ")" ~ "=>") ~ ParseTypeExpression ~> (
      (valueParams: Seq[SingleValueParameterEvaluation], returnType: TypeExpression) => MethodTypeAnnotation(
        params = valueParams.map {
          case SingleValueParameterEvaluation(varPlace, constraint) => (varPlace.varWrapper.id -> constraint)
        }.toMap,
        result = returnType)
    )
  }

  def ParseTypeclassMethodDefn: Rule1[MethodSignature] = rule {
    ParseVariablePlaceName ~ "<=" ~ ParseMethodTypeAnnotation ~> (MethodSignature(_, _))
  }

  def ParseTypeclassBody: Rule1[TypeclassDefnFields] = rule {
    "(" ~ oneOrMore(ParseTypeclassMethodDefn).separatedBy(";") ~ ")" ~> (
      (methods: Seq[MethodSignature]) => TypeclassDefnFields(methods.map {
        case MethodSignature(place, annot) => (place -> annot)
      }.toMap)
    )
  }

  def ParseMethodBody: Rule1[MethodBody] = rule {
    "(" ~ oneOrMore(VariableAssignment).separatedBy(";") ~ ")" ~> ((varAssignments: Seq[ValueAssignment]) => MethodBody(varAssignments.toSeq))
  }

  // def HandlePrecedingTypeParameterDecl = rule {
  //   zeroOrMore(HandleTypeParameter) ~> (places => TypeParameterPack(
  //     mapping = places.map {
  //       case TypePlace(ty, constraint) => (ty -> constraint)
  //     }.toMap
  //   ))
  // }

  // def ParseTypeclassConstraint = rule {
  //   (":" ~ ParseIdentifier ~> TypeWrapper) ~ "<-" ~ ParseTypeclass ~> (
  //     (ty, constraint) => TypePlace(ty, Some(constraint)))
  // }

  // def ParseComplexTypePlace = rule {
  //   ParseTypeNameCreation | ParseTypeclassConstraint
  // }

  // def HandleTypeParameter = rule {
  //   ParseComplexTypePlace ~ "->"
  // }

  // def HandleTypeclassSpecialization = rule {
  //   ParseTypeclassCreation ~ "<-" ~ HandlePrecedingTypeParameterDecl ~> ((typeclassPlace, _) => typeclassPlace)
  //   // ~> ((typeclassPlace, tpp) => TypeParameterNameAssignment(typeclassPlace, tpp))
  // }

  def ParseTypeclassMethodDefnAnonymous: Rule1[TypeclassMethodDefnAnonymous] = rule {
    zeroOrMore("(" ~ ParseValueParameterCreation ~ ")" ~ "=>") ~ ParseValueExpression ~> (
      (params: Seq[SingleValueParameterEvaluation], expr: ValueExpression) => TypeclassMethodDefnAnonymous(
        vpp = ValueParameterPack(
          params.map {
            case SingleValueParameterEvaluation(vp, constraint) => (vp -> constraint)
          }.toMap,
        ),
        expr = expr)
    )
  }

  def TypeclassMethodImplementation: Rule1[MethodDefinition] = rule {
    ParseVariablePlaceName ~ "<=" ~ ParseTypeclassMethodDefnAnonymous ~> (
      (vp: VarPlace, defn: TypeclassMethodDefnAnonymous) => MethodDefinition(place = vp, vpp = defn.vpp, expr = defn.expr)
    )
  }

  def TypeclassImplementation: Rule1[TypeclassImpl] = rule {
    ParseTypeclassCreation ~ "<=" ~ "(" ~ oneOrMore(TypeclassMethodImplementation).separatedBy(";") ~ ")" ~> (
      (typ: TypeclassPlaceExpression, methodDefns: Seq[MethodDefinition]) => TypeclassImplNode(
        tyn = typ.getTarget,
        tpp = TypeParameterPack.empty(),
        methods = methodDefns.map {
          case MethodDefinition(place, vpp, expr) => (place -> TypeclassMethodDefnAnonymous(vpp, expr))
        }.toMap
      )
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
