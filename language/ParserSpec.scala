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
      }
    }
  }

  // "FunnelPEG" - {
  //   "when parsing top-level expressions" - {
  //     "should correctly parse variable and type assignments" in {
  //       parse("$point <= (.x, .y <- $Integer)") should be (StructDefinition(
  //         TypePlace(TypeWrapper(IdentifierWrapper("point")), None),
  //         StructFields(Map(
  //           IdentifierWrapper("x") -> None,
  //           IdentifierWrapper("y") -> Some(TypeName(IdentifierWrapper("integer")))
  //         ))))
  //       parse("$x <= 3") should be (ValueAssignment(
  //         VarPlace(VarWrapper(IdentifierWrapper("x"))),
  //         NumericLiteral(3)))
  //       parse(":list <= (+none, +cons(.car <- :element, .cdr <- :Self))") should be (EnumDefinition(
  //         TypePlace(TypeWrapper(IdentifierWrapper("list")), None),
  //         EnumCases(Map(
  //           IdentifierWrapper("none") -> None,
  //           IdentifierWrapper("cons") -> Some(StructFields(Map(
  //             IdentifierWrapper("car") -> Some(TypeName(IdentifierWrapper("element"))),
  //             IdentifierWrapper("cdr") -> Some(TypeName(IdentifierWrapper("Self"))))))))
  //       ))
  //     }

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
