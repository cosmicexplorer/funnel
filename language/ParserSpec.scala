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
        parse("$list[\\.El] <= (\\+empty, \\+cons(\\.car <- .El, \\.cdr <- .Self))") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "list"))),
          AnonymousMethod(
            params = StructLiteralValue(),
            output = ValueAlternation(Map(
              AlternationCaseName("empty") -> EmptyStructType,
              AlternationCaseName("cons") -> StructLiteralType(Map(
                NamedIdentifier(ValueKind, "car") -> LocalTypeVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(TypeKind, "El")))),
                NamedIdentifier(ValueKind, "cdr") -> LocalTypeVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(TypeKind, "Self")))),
              )))),
            typeParams = TypeParamsBound(Map(
              NamedIdentifier(TypeKind, "El") -> TypePlaceholder,
            ))
          )

        ))
        parse("$X <- <(.x[$Integer])>") should be (TypeAssignment(
          GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "X"))),
          StructLiteralType(Map(
            NamedIdentifier(ValueKind, "x") -> GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Integer")))
          ))))
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
        parse("$F <- [(\\.x[$Integer]) => <.x>]") should be (TypeAssignment(
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

      "should allow value and/or type assertions at the top level" in {
        parse("3 <= 3") should be (ValueAssertion(
          IntegerLiteral(3),
          IntegerLiteral(3)
        ))
        parse("<$f> <- [\\.p => <.p>]") should be (TypeAssertion(
          TypePlaceholder,
          MethodType(
            StructLiteralType(Map(
              NamedIdentifier(ValueKind, "p") -> TypePlaceholder,
            )),
            StructLiteralType(Map(
              NamedIdentifier(ValueKind, "p") -> TypePlaceholder,
            )),
          )
        ))
      }

      // TODO: figure out a syntax for this that isn't the *EXACT* same as the one for type and/or
      // value function calls!!
      "should allow inline type and/or value assertions" in {
        parse("3 <= 3[$Integer]") should be (ValueAssertion(
          IntegerLiteral(3),
          InlineTypeAssertionForValue(IntegerLiteral(3), GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Integer"))))
        ))
        parse("$x <= 3(3)") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "x"))),
          InlineValueAssertion(IntegerLiteral(3), IntegerLiteral(3)),
        ))
        parse("$X <= $Integer[$Y]") should be (TypeAssignment(
          GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "X"))),
          InlineTypeAssertionForType(
            GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Integer"))),
            GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "Y")))
          )
        ))
        parse("$x <= 3(3)[<3>]") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "x"))),
          InlineTypeAssertionForValue(
            InlineValueAssertion(IntegerLiteral(3), IntegerLiteral(3)),
            IntegerTypeLiteral,
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

    "when intermixing type and value parameter packs" - {
      "should correctly handle type parameter packs in sequence" in {
        parse("$f <= \\.T -> \\.x[.T] => .x") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "f"))),
          AnonymousMethod(
            StructLiteralValue(Map(
              NamedIdentifier(ValueKind, "x") -> LocalTypeVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(TypeKind, "T")))),
            )),
            // TODO: is this what we want when returning a bare `.x`?
            LocalValueVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(ValueKind, "x")))),
            // NamedValuePack(Map(
            //   NamedIdentifier(ValueKind, "x") -> LocalValueVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(ValueKind, "x")))),
            // )),
            TypeParamsBound(Map(
              NamedIdentifier(TypeKind, "T") -> TypePlaceholder,
            )),
          )
        ))
        parse("$f[\\.T](\\.x[.T]) <= $plus(.x, 3)") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "f"))),
          AnonymousMethod(
            StructLiteralValue(Map(
              NamedIdentifier(ValueKind, "x") -> LocalTypeVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(TypeKind, "T")))),
            )),
            FunctionCall(
              GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "plus"))),
               PositionalParameterPack(ValueKind, Seq(
                LocalValueVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(ValueKind, "x")))),
                IntegerLiteral(3),
              ))
            ),
            TypeParamsBound(Map(
              NamedIdentifier(TypeKind, "T") -> TypePlaceholder,
            )),
          )
        ))
        parse("$f(\\.x[\\.T]) <= 4") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "f"))),
          AnonymousMethod(
            StructLiteralValue(Map(
              NamedIdentifier(ValueKind, "x") -> LocalTypeVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(TypeKind, "T")))),
            ),
              NamedParameterPack(TypeKind, Map(
                NamedIdentifier(TypeKind, "T") -> TypePlaceholder,
              )),
              TypeParamsDecl(Map(NamedIdentifier(TypeKind, "T") -> TypePlaceholder))),
            IntegerLiteral(4))))
        parse("$f[\\.T] <= \\.x[.T]") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "f"))),
          AnonymousMethod(
            StructLiteralValue(),
            AnonymousMethod(
              StructLiteralValue(Map(
                NamedIdentifier(ValueKind, "x") -> LocalTypeVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(TypeKind, "T")))),
              )),
              // TODO: is this what we want when returning a bare `.x`? I think so!!!
              NamedValuePack(Map(NamedIdentifier(ValueKind, "x") -> LocalValueVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(ValueKind, "x"))))))),
            TypeParamsBound(Map(
              NamedIdentifier(TypeKind, "T") -> TypePlaceholder,
            ))
          )
        ))
        parse("$f <= \\.x[\\.T]") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier(ValueKind, "f"))),
          AnonymousMethod(
            StructLiteralValue(Map(
              NamedIdentifier(ValueKind, "x") -> LocalTypeVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(TypeKind, "T")))),
            ),
              NamedParameterPack(TypeKind, Map(NamedIdentifier(TypeKind, "T") -> TypePlaceholder)),
              TypeParamsDecl(Map(NamedIdentifier(TypeKind, "T") -> TypePlaceholder)),
            ),
            NamedValuePack(Map(NamedIdentifier(ValueKind, "x") -> LocalValueVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(ValueKind, "x")))))))))
        parse("<$f> <- \\.T -> $F[.T]") should be (TypeAssertion(
          TypePlaceholder,
          AnonymousTypeMethod(
            TypeParamsBound(Map(
              NamedIdentifier(TypeKind, "T") -> TypePlaceholder,
            )),
            TypeFunctionCall(
              GlobalTypeVar(GlobalVar(NamedIdentifier(TypeKind, "F"))),
              NamedParameterPack(TypeKind, Map(
                NamedIdentifier(TypeKind, "T") -> LocalTypeVar(LocalVar(LocalNamedIdentifier(NamedIdentifier(TypeKind, "T")))),
              ))
            ),
          )
        ))
      }
    }

    // TODO: make type parameter packs work!!!

    // TODO: implement the +{} parser syntax!!!
  }
}
