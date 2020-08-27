package funnel.language

import FunnelPEG._

import org.junit.runner.RunWith
import org.parboiled2._
import org.scalatest._
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class ParserSpec extends FlatSpec with Matchers {

  def parse(input: String): Statement = {
    val parser = new FunnelPEG(input)
    try { parser.TopLevel.run().get } catch {
      case e: ParseError => throw new Exception(e.format(parser))
    }
  }

  "FunnelPEG" should "Correctly parse simple top-level statements" in {
    parse(":point <= (.x, .y <- :integer)") should be (StructDefinition(
      TypePlace(TypeWrapper(IdentifierWrapper("point")), None),
      StructFields(Map(
        IdentifierWrapper("x") -> None,
        IdentifierWrapper("y") -> Some(TypeName(IdentifierWrapper("integer")))
      ))))
    parse("$x <= 3") should be (ValueAssignment(
      VarPlace(VarWrapper(IdentifierWrapper("x"))),
      NumericLiteral(3)))
    parse(":list <= (+none, +cons(.car <- :element, .cdr <- :Self))") should be (EnumDefinition(
      TypePlace(TypeWrapper(IdentifierWrapper("list")), None),
      EnumCases(Map(
        IdentifierWrapper("none") -> None,
        IdentifierWrapper("cons") -> Some(StructFields(Map(
          IdentifierWrapper("car") -> Some(TypeName(IdentifierWrapper("element"))),
          IdentifierWrapper("cdr") -> Some(TypeName(IdentifierWrapper("Self"))))))))
    ))
  }

  "FunnelPEG" should "Be able to define functions" in {
    parse("$f <= ($x <- :integer) => $x") should be (ValueAssignment(
      VarPlace(VarWrapper(IdentifierWrapper("f"))),
      AnonymousMethodDefinition(ValueParameterPack(Map(
        VarPlace(VarWrapper(IdentifierWrapper("x"))) -> Some(TypeName(IdentifierWrapper("integer"))))),
        Variable(VarWrapper(IdentifierWrapper("x"))))
      ))
  }

  "FunnelPEG" should "respect the associativity of the arrow operators" in {
    parse("$f <= $x <- :integer => $x") should be (ValueAssignment(
      VarPlace(VarWrapper(IdentifierWrapper("f"))),
      AnonymousMethodDefinition(ValueParameterPack(Map(
        VarPlace(VarWrapper(IdentifierWrapper("x"))) -> Some(TypeName(IdentifierWrapper("integer"))))),
        Variable(VarWrapper(IdentifierWrapper("x"))))
    ))
  }

  "FunnelPEG" should "allow some or no whitespace after all terminals and identifiers" in {
    parse("$f <= ( $x <- :integer ) => $x") should be (ValueAssignment(
      VarPlace(VarWrapper(IdentifierWrapper("f"))),
      AnonymousMethodDefinition(ValueParameterPack(Map(
        VarPlace(VarWrapper(IdentifierWrapper("x"))) -> Some(TypeName(IdentifierWrapper("integer"))))),
        Variable(VarWrapper(IdentifierWrapper("x"))))
    ))
    parse("$f<=$x<-:integer=>$x") should be (ValueAssignment(
      VarPlace(VarWrapper(IdentifierWrapper("f"))),
      AnonymousMethodDefinition(ValueParameterPack(Map(
        VarPlace(VarWrapper(IdentifierWrapper("x"))) -> Some(TypeName(IdentifierWrapper("integer"))))),
        Variable(VarWrapper(IdentifierWrapper("x"))))
    ))
  }
}
