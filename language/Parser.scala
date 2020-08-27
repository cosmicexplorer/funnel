package funnel.language

import org.parboiled.scala._


class FunnelPEG extends Parser {
  // Define AST entities.
  case class IdentifierWrapper(name: String)
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

  sealed abstract class PlaceExpression extends Expression

  sealed abstract class ValuePlaceExpression extends PlaceExpression
  case class VarPlace(id: IdentifierWrapper) extends ValuePlaceExpression

  sealed abstract class TypePlaceExpression(constraint: Option[TypeExpression]) extends PlaceExpression
  case class TypePlace(ty: TypeWrapper, constraint: Option[TypeExpression])
      extends TypePlaceExpression(constraint)
  case class TypePlaceWithParams(
    ty: TypeWrapper,
    constraint: Option[TypeExpression],
    tpp: TypeParameterPack
  ) extends TypePlaceExpression(constraint)

  sealed abstract class TypeclassPlaceExpression(constraint: Option[TypeExpression]) extends PlaceExpression
  case class TypeclassPlace(tyc: TypeclassWrapper, constraint: Option[TypeExpression])
      extends TypeclassPlaceExpression(constraint)
  case class TypeclassPlaceWithParams(
    tyc: TypeclassWrapper,
    constraint: Option[TypeExpression],
    tpp: TypeParameterPack
  ) extends TypeclassPlaceExpression(constraint)

  sealed abstract class BasicValueExpression extends ValueExpression
  case class Variable(id: IdentifierWrapper) extends BasicValueExpression

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
  case class TypeclassDefinition(tyn: TypeclassWrapper, fields: TypeclassDefnFields)
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

  // Define the parsing rules.
  def Funnel = rule { WhiteSpace ~ ReplOrFile ~ EOI }

  def TopLevel: Rule1[AstNode] = rule {

    VariableAssignment | TypeAssignment | ParseTypeclassCreation | TypeclassImplementation
  }

  def ReplOrFile = rule { zeroOrMore(TopLevel) }

  def ParseIdentifier = rule {
    oneOrMore("a" - "z" | "A" - "Z" | "0" - "9" | "-" | "_") ~> IdentifierWrapper
  }

  def ParseTypeName = rule {
    ":" ~ ParseIdentifier ~~> TypeName
  }

  def ParseVariablePlaceName = rule {
    "$" ~ ParseIdentifier ~~> VarPlace
  }

  def ParseVariableRefName = rule {
    "$" ~ ParseIdentifier ~~> Variable
  }

  def VariableAssignment = rule {
    ParseVariablePlaceName ~ "<=" ~ ParseValueExpression ~~> ValueAssignment
  }



  def ParseNumericLiterals: Rule1[NumericLiteral] = rule { oneOrMore("0" - "9") ~> ((s: String) => NumericLiteral(s.toInt))}

  def ParseStringLiterals: Rule1[StringLiteral] = rule { "\"" ~ oneOrMore(!anyOf("\"\\")) ~> ((s) => StringLiteral(s)) ~ "\"" }

  def ParseLiterals: Rule1[Literal] = rule { ParseNumericLiterals | ParseStringLiterals }

  def ParseValueExpression: Rule1[ValueExpression] = rule {
    ParseVariableRefName | ParseLiterals
  }

  def ParseTypeNameCreation = rule {
    ":" ~ ParseIdentifier ~~> TypeWrapper ~~> (wrapper => TypePlace(wrapper, constraint = None))
  }

  def TypeAssignment = rule {
    StructAssignment | EnumAssignment
  }

  def StructAssignment = rule {
    ParseTypeNameCreation ~ "<=" ~ ParseStructBody ~~> StructDefinition
  }

  def EnumAssignment = rule {
    ParseTypeNameCreation ~ "<=" ~ ParseEnumBody ~~> EnumDefinition
  }

  def ParseTypeAssertion = rule {
    ParseVariablePlaceName ~ "<-" ~ ParseTypeExpression ~~> ((varPlace, typeExpr) =>
    VariableTypeAssertion(typeExpr, varPlace))
  }

  def ParseTypeParameterEvaluation: Rule1[SingleTypeParameterEvaluation] = rule {
    "<-" ~ "(" ~ ParseTypeNameCreation ~ "<-" ~ ParseTypeExpression ~ ")" ~~> (
      (typePlace, typeExpr) => SingleTypeParameterEvaluation(typePlace, typeExpr))
  }

  def ParseTypeParameterPack: Rule1[TypeParameterPack] = rule {
    zeroOrMore(ParseTypeParameterEvaluation) ~~> (params => TypeParameterPack(
      mapping = params.map {
        case SingleTypeParameterEvaluation(ty, constraint) => (ty -> constraint)
      }.toMap
    ))
  }

  def ParseTypeExpression: Rule1[TypeExpression] = rule {
    ParseTypeName
  }

  def ParseStructField: Rule1[SingleStructField] = rule {
    "." ~ ParseIdentifier ~ optional("<-" ~ ParseTypeExpression) ~~> (
      (varName, maybeTypeExpr) => SingleStructField(varName, maybeTypeExpr))
  }

  def ParseStructBody: Rule1[StructFields] = rule {
    "(" ~ zeroOrMore(ParseStructField, separator = ", ")  ~ ")" ~~> (
      (fields) => StructFields(fields.map {
        case SingleStructField(id, constraint) => (id -> constraint)
      }.toMap))
  }

  def ParseEnumCase: Rule1[SingleEnumCase] = rule {
    "+" ~ ParseIdentifier ~ optional(ParseStructBody) ~~> (
      (caseName, innerFields) => SingleEnumCase(caseName, innerFields)
    )
  }

  def ParseEnumBody: Rule1[EnumCases] = rule {
    "(" ~ oneOrMore(ParseEnumCase, separator = ", ") ~ ")" ~~> (
      (cases) => EnumCases(cases.map {
        case SingleEnumCase(id, fields) => (id -> fields)
      }.toMap))
  }

  def ParseTypeclass = rule {
    "&" ~ ParseIdentifier ~~> Typeclass
  }

  def ParseTypeclassCreation = rule {
    "&" ~ ParseIdentifier ~~> TypeclassWrapper ~~> ((s) => TypeclassPlace(s, None))
  }

  def TypeclassCreation = rule {
    ParseTypeclassCreation ~ "<=" ~ ParseTypeclassBody
  }

  def ParseValueParameterCreation: Rule1[SingleValueParameterEvaluation] = rule {
    ParseVariablePlaceName ~ optional("<-" ~ ParseTypeExpression) ~~> (
      (varPlace, maybeTypeExpr) => SingleValueParameterEvaluation(varPlace, maybeTypeExpr)
    )
  }

  def ParseMethodTypeAnnotation: Rule1[MethodTypeAnnotation] = rule {
    zeroOrMore("(" ~ ParseValueParameterCreation ~ ")" ~ "=>") ~ ParseTypeExpression ~~> (
      (valueParams, returnType) => MethodTypeAnnotation(
        params = valueParams.map {
          case SingleValueParameterEvaluation(varPlace, constraint) => (varPlace.id -> constraint)
        }.toMap,
        result = returnType)
    )
  }

  def ParseTypeclassMethodDefn: Rule1[MethodSignature] = rule {
    ParseVariablePlaceName ~ "<=" ~ ParseMethodTypeAnnotation ~~> MethodSignature
  }

  def ParseTypeclassBody: Rule1[TypeclassDefnFields] = rule {
    "(" ~ oneOrMore(ParseTypeclassMethodDefn, separator = ";") ~ ")" ~~> (
      (methods) => TypeclassDefnFields(methods.map {
        case MethodSignature(place, annot) => (place -> annot)
      }.toMap)
    )
  }

  def ParseMethodBody: Rule1[MethodBody] = rule {
    "(" ~ oneOrMore(VariableAssignment, separator = ";") ~ ")" ~~> ((varAssignments) => MethodBody(varAssignments.toSeq))
  }

  // def HandlePrecedingTypeParameterDecl: Rule1[TypeParameterPack] = rule {
  //   zeroOrMore(HandleTypeParameter) ~~> (places => TypeParameterPack(
  //     mapping = places.map {
  //       case TypePlace(ty, constraint) => (ty -> constraint)
  //     }.toMap
  //   ))
  // }

  // def ParseTypeclassConstraint: Rule1[TypePlace] = rule {
  //   (":" ~ ParseIdentifier ~~> TypeWrapper) ~ "<-" ~ ParseTypeclass ~~> (
  //     (ty, constraint) => TypePlace(ty, Some(constraint)))
  // }

  // def ParseComplexTypePlace: Rule1[TypePlace] = rule {
  //   ParseTypeNameCreation | ParseTypeclassConstraint
  // }

  // def HandleTypeParameter: Rule1[TypePlace] = rule {
  //   ParseComplexTypePlace ~ "->"
  // }

  // def HandleTypeclassSpecialization: Rule1[TypeclassPlaceExpression] = rule {
  //   ParseTypeclassCreation ~ "<-" ~ HandlePrecedingTypeParameterDecl ~~> ((typeclassPlace, _) => typeclassPlace)
  //   // ~~> ((typeclassPlace, tpp) => TypeParameterNameAssignment(typeclassPlace, tpp))
  // }

  def HandleTypeclassDefinition = rule {
    ParseTypeclassCreation ~ "<=" ~ ParseTypeclassBody ~~> (
      (typ, defn) => TypeclassDefinition(typ.tyc, defn))
  }

  def ParseTypeclassMethodDefnAnonymous: Rule1[TypeclassMethodDefnAnonymous] = rule {
    zeroOrMore("(" ~ ParseValueParameterCreation ~ ")" ~ "=>") ~ ParseValueExpression ~~> (
      (params, expr) => TypeclassMethodDefnAnonymous(
        vpp = ValueParameterPack(
          params.map {
            case SingleValueParameterEvaluation(vp, constraint) => (vp -> constraint)
          }.toMap,
        ),
        expr = expr)
    )
  }

  def TypeclassMethodImplementation: Rule1[MethodDefinition] = rule {
    ParseVariablePlaceName ~ "<=" ~ ParseTypeclassMethodDefnAnonymous ~~> (
      (vp, defn) => MethodDefinition(place = vp, vpp = defn.vpp, expr = defn.expr)
    )
  }

  def TypeclassImplementation: Rule1[TypeclassImpl] = rule {
    ParseTypeclassCreation ~ "<=" ~ "(" ~ oneOrMore(TypeclassMethodImplementation, separator = ";") ~ ")" ~~> (
      (typ, methodDefns) => TypeclassImplNode(
        tyn = typ.tyc,
        tpp = TypeParameterPack.empty(),
        methods = methodDefns.map {
          case MethodDefinition(place, vpp, expr) => (place -> TypeclassMethodDefnAnonymous(vpp, expr))
        }.toMap
      )
    )
  }

  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
}



object Main extends App {
  println("wow")
}
