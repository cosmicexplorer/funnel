package funnel.language

import FunnelPEG._

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class ParserSpec extends FlatSpec with Matchers {

  def parse(input: String): Statement = new FunnelPEG(input).TopLevel.run().get

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
}
