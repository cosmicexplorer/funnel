package funnel.language

import FunnelPEG._

import org.junit.runner.RunWith
import org.parboiled2._
import org.scalatest._
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class ParserSpec extends FreeSpec with Matchers {

  def parse(input: String): Statement = {
    val parser = new FunnelPEG(input)
    try { parser.TopLevel.run().get } catch {
      case e: ParseError => throw new Exception(e.format(parser))
    }
  }

  "FunnelPEG" - {
    "when parsing top-level expressions" - {
      "should correctly parse variable and type assignments" in {
        parse("$x <= 3") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "x"))),
          IntegerLiteral(3),
        ))
        parse("$x <= (\\.x, \\.y)") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "x"))),
          StructLiteralValue(Map(
            NamedIdentifier(ValueKind, "x") -> TypePlaceholder,
            NamedIdentifier(ValueKind, "y") -> TypePlaceholder,
          ))
        ))
        parse("$x <= (3, \"a\", 0.0)") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "x"))),
          PositionalValueParameterPack(Seq(
            IntegerLiteral(3),
            StringLiteral("a"),
            FloatLiteral(0.0),
          )),
        ))
        parse("$point <= (.x(2)[$Integer], .y <= (3))") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "point"))),
          NamedValuePack(Map(
            NamedIdentifier(ValueKind, "x") -> ValueWithTypeAssertion(
              Some(IntegerLiteral(2)),
              Some(GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Integer"))))),
            NamedIdentifier(ValueKind, "y") -> ValueWithTypeAssertion(Some(IntegerLiteral(3)), None),
          ))
        ))
        parse("$X <- $Y") should be (TypeAssignment(
          GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "X"))),
          GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Y"))),
        ))
        parse("$list <= (\\+empty, \\+cons(\\.car <- $Element, \\.cdr <- $Self))") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "list"))),
          ValueAlternation(Map(
            AlternationCaseName("empty") -> EmptyStructType,
            AlternationCaseName("cons") -> StructLiteralType(Map(
              NamedIdentifier(ValueKind, "car") -> GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Element"))),
              NamedIdentifier(ValueKind, "cdr") -> GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Self"))),
            ))
          ))
        ))
        parse("$X <- <(\\.x[$Integer])>") should be (TypeAssignment(
          GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "X"))),
          StructLiteralType(Map(
            NamedIdentifier(ValueKind, "x") -> GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Integer")))
          ))
        ))
      }

      "should be able to define functions with a variable assignment" in {
        parse("$f <= (\\.x <- $Integer) => .x") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "f"))),
          AnonymousMethod(
            StructLiteralValue(Map(
              NamedIdentifier(ValueKind, "x") -> GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Integer")))
            )),
            LocalValueVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(ValueKind, "x"))))
          )
        ))
      }
    }
  }

  // "FunnelPEG" - {
  //   "when parsing top-level expressions" - {
  //     "should be able to define functions with a variable assignment" in {
  //       parse("$f <= ($x <- :integer) => $x") should be (ValueAssignment(
  //         VarPlace(VarWrapper(IdentifierWrapper("f"))),
  //         AnonymousMethodDefinition(ValueParameterPack(Map(
  //           VarPlace(VarWrapper(IdentifierWrapper("x"))) ->
  //             Some(TypeName(IdentifierWrapper("integer"))))),
  //           Variable(VarWrapper(IdentifierWrapper("x"))))
  //       ))
  //     }

  //     "should respect the associativity of the arrow operators" in {
  //       parse("$f <= $x <- :integer => $x") should be (ValueAssignment(
  //         VarPlace(VarWrapper(IdentifierWrapper("f"))),
  //         AnonymousMethodDefinition(ValueParameterPack(Map(
  //           VarPlace(VarWrapper(IdentifierWrapper("x"))) ->
  //             Some(TypeName(IdentifierWrapper("integer"))))),
  //           Variable(VarWrapper(IdentifierWrapper("x"))))
  //       ))
  //     }

  //     "should allow some or no whitespace after all terminals and identifiers" in {
  //       parse("$f <= ( $x <- :integer ) => $x") should be (ValueAssignment(
  //         VarPlace(VarWrapper(IdentifierWrapper("f"))),
  //         AnonymousMethodDefinition(ValueParameterPack(Map(
  //           VarPlace(VarWrapper(IdentifierWrapper("x"))) ->
  //             Some(TypeName(IdentifierWrapper("integer"))))),
  //           Variable(VarWrapper(IdentifierWrapper("x"))))
  //       ))
  //       parse("$f<=$x<-:integer=>$x") should be (ValueAssignment(
  //         VarPlace(VarWrapper(IdentifierWrapper("f"))),
  //         AnonymousMethodDefinition(ValueParameterPack(Map(
  //           VarPlace(VarWrapper(IdentifierWrapper("x"))) ->
  //             Some(TypeName(IdentifierWrapper("integer"))))),
  //           Variable(VarWrapper(IdentifierWrapper("x"))))
  //       ))
  //     }

  //     "should be able to parse typeclass definitions" in {
  //       parse("&typeclass <= ($f <= :integer; $x <= ($y <- :x) => :z)") should be (TypeclassDefinition(
  //         TypeclassPlace(
  //           TypeclassWrapper(IdentifierWrapper("typeclass")),
  //           None,
  //         ),
  //         TypeclassDefnFields(Map(
  //           VarPlace(VarWrapper(IdentifierWrapper("f"))) ->
  //             MethodTypeAnnotation(Map.empty, TypeName(IdentifierWrapper("integer"))),
  //           VarPlace(VarWrapper(IdentifierWrapper("x"))) ->
  //             MethodTypeAnnotation(Map(
  //               IdentifierWrapper("y") -> Some(TypeName(IdentifierWrapper("x")))
  //             ),
  //               TypeName(IdentifierWrapper("z")))
  //         ))
  //       ))
  //     }
  //   }
  // }
}
