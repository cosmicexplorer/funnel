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
          AnonymousMethod(
            StructLiteralValue(Map(
              NamedIdentifier(ValueKind, "x") -> TypePlaceholder,
              NamedIdentifier(ValueKind, "y") -> TypePlaceholder,
            )),
            NamedValuePack(Map(
              NamedIdentifier(ValueKind, "x") -> LocalValueVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(ValueKind, "x")))),
              NamedIdentifier(ValueKind, "y") -> LocalValueVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(ValueKind, "y"))))
            )
            )
          )
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
        parse("$f <= \\.x[$Integer]") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "f"))),
          AnonymousMethod(
            StructLiteralValue(Map(
              NamedIdentifier(ValueKind, "x") -> GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Integer"))),
            )),
            NamedValuePack(Map(
              NamedIdentifier(ValueKind, "x") -> LocalValueVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(ValueKind, "x")))),
            ))
          )
        ))
        parse("$F <- [(\\.x[$Integer]) => <\\.x>]") should be (TypeAssignment(
          GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "F"))),
          MethodType(
            StructLiteralType(Map(
              NamedIdentifier(ValueKind, "x") -> GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Integer")))
            )),
            StructLiteralType(Map(
              NamedIdentifier(ValueKind, "x") -> TypePlaceholder,
            )),
          )
        ))
      }

      "should respect the associativity of the arrow operators" in {
        parse("$f <= \\.x <- $Integer => .x") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "f"))),
          AnonymousMethod(
            StructLiteralValue(Map(
              NamedIdentifier(ValueKind, "x") -> GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Integer")))
            )),
            LocalValueVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(ValueKind, "x")))),
          )
        ))
      }

      "should allow some or no whitespace after all terminals and identifiers" in {
        parse("$f <= ( \\.x <- $Integer ) => .x") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "f"))),
          AnonymousMethod(
            StructLiteralValue(Map(
              NamedIdentifier(ValueKind, "x") -> GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Integer")))
            )),
            LocalValueVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(ValueKind, "x")))),
          )
        ))
        parse("$f<=\\.x<-$Integer=>.x") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "f"))),
          AnonymousMethod(
            StructLiteralValue(Map(
              NamedIdentifier(ValueKind, "x") -> GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Integer")))
            )),
            LocalValueVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(ValueKind, "x")))),
          )
        ))
      }

      // TODO: implement implicit conversions!!!
      "should be able to register and declare implicit conversions with ~>" in {
        // parse("$x <~ 3[$Integer]") should be
        // parse("$f <~ \\.x[$String] => $string-length")
        // parse("")
      }
    }

    // TODO: make type parameter packs work!!!

    // TODO: implement the +{} parser syntax!!!
  }
}
