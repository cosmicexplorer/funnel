package funnel.language

import NonNodeData._
import FunnelPEG._

import org.junit.runner.RunWith
import org.parboiled2._
import org.scalatest._
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class ParserSpec extends FreeSpec with Matchers {

  def parse(input: String): BaseStatement = {
    val parser = new FunnelPEG(input)
    try { parser.TopLevel.run().get } catch {
      case e: ParseError => throw new Exception(e.format(parser))
    }
  }

  "FunnelPEG" - {
    "when parsing top-level expressions" - {
      "should correctly parse variable and type assignments" in {
        parse("$x <= 3") should be (ValueAssignment(
          GlobalValueVar(GlobalVar[ValueKind.type](NamedIdentifier[ValueKind.type]("x"))),
          IntegerLiteral(3),
        ))
        parse("$x <= (\\.x, \\.y)") should be (ValueAssignment(
          GlobalValueVar(GlobalVar[ValueKind.type](NamedIdentifier[ValueKind.type]("x"))),
          AnonymousMethod(
            output = NamedValuePack(Map(
              NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
              NamedIdentifier[ValueKind.type]("y") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("y"))))
            )),
            functionParams = ParamsDeclaration(NamedValueParamPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> TypePlaceholder,
              NamedIdentifier[ValueKind.type]("y") -> TypePlaceholder,
            ))),
          )
        ))
        parse("$x <= (3, \"a\", 0.0)") should be (ValueAssignment(
          GlobalValueVar(GlobalVar[ValueKind.type](NamedIdentifier[ValueKind.type]("x"))),
          PositionalValueParameterPack(Seq(
            IntegerLiteral(3),
            StringLiteral("a"),
            FloatLiteral(0.0),
          )),
        ))
        parse("$point <= (.x(2)[$Integer], .y <= (3))") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("point"))),
          NamedValuePack(Map(
            NamedIdentifier[ValueKind.type]("x") -> InlineTypeAssertionForValue(
              IntegerLiteral(2),
              GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))),
            NamedIdentifier[ValueKind.type]("y") -> IntegerLiteral(3),
          ))
        ))
        parse("$X <- $Y") should be (TypeAssignment(
          GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("X"))),
          GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Y"))),
        ))
        parse("$list[\\.El] <= (\\+empty, \\+cons(\\.car <- .El, \\.cdr <- .Self))") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("list"))),
          TypeParamsWrapperForValue(
            subject = EnumLiteralValue(Seq(
              AlternationCase(AlternationCaseName("empty"), StructTypeLiteral(ParameterPackKinds.empty)),
              AlternationCase(AlternationCaseName("cons"), StructTypeLiteral(NamedParameterPack(Map(
                NamedIdentifier[ValueKind.type]("car") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("El")))),
                NamedIdentifier[ValueKind.type]("cdr") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("Self")))),
              ))))
            )),
            tpp = NamedTypeParamPack(Map(
              NamedIdentifier[TypeKind.type]("El") -> TypePlaceholder,
            )),
            fromLocation = Left,
          ),
        ))
        parse("$X <- <(.x[$Integer])>") should be (TypeAssignment(
          GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("X"))),
          StructTypeLiteral(NamedParameterPack(Map(
            NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))
          )))))
      }

      "should be able to define functions with a variable assignment" in {
        parse("$f <= (\\.x <- $Integer) => .x") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
          AnonymousMethod(
            output = LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
            functionParams = ParamsDeclaration(NamedValueParamPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))
            ))),
          )
        ))
        parse("$f <= \\.x[$Integer]") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
          AnonymousMethod(
            output = NamedValuePack(Map(
              NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
            )),
            functionParams = ParamsDeclaration(NamedValueParamPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer"))),
            ))),
          )
        ))
        parse("$F <- [(\\.x[$Integer]) => <.x>]") should be (TypeAssignment(
          GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("F"))),
          MethodSignature(
            output = StructTypeLiteral(NamedParameterPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> TypePlaceholder,
            ))),
            functionParams = ParamsDeclaration(NamedParameterPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))
            ))),
          )
        ))
      }

      "should respect the associativity of the arrow operators" in {
        parse("$f <= \\.x <- $Integer => .x") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
          AnonymousMethod(
            output = LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
            functionParams = ParamsDeclaration(NamedValueParamPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))
            ))),
          )
        ))
      }

      "should allow some or no whitespace after all terminals and identifiers" in {
        parse("$f <= ( \\.x <- $Integer ) => .x") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
          AnonymousMethod(
            output = LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
            functionParams = ParamsDeclaration(NamedValueParamPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))
            ))),
          )
        ))
        parse("$f<=\\.x<-$Integer=>.x") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
          AnonymousMethod(
            output = LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
            functionParams = ParamsDeclaration(NamedValueParamPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))
            ))),
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
          MethodSignature(
            output = StructTypeLiteral(NamedParameterPack(Map(
              NamedIdentifier[ValueKind.type]("p") -> TypePlaceholder,
            ))),
            functionParams = ParamsDeclaration(NamedParameterPack(Map(
              NamedIdentifier[ValueKind.type]("p") -> TypePlaceholder,
            ))),
          )
        ))
      }

      // TODO: figure out a syntax for this that isn't the *EXACT* same as the one for type and/or
      // value function calls!!
      "should allow inline type and/or value assertions" in {
        parse("3 <= 3[$Integer]") should be (ValueAssertion(
          IntegerLiteral(3),
          InlineTypeAssertionForValue(IntegerLiteral(3), GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer"))))
        ))
        parse("$x <= 3(3)") should be (ValueAssignment(
          GlobalValueVar(GlobalVar[ValueKind.type](NamedIdentifier[ValueKind.type]("x"))),
          InlineValueAssertion(IntegerLiteral(3), IntegerLiteral(3)),
        ))
        parse("$X <= $Integer[$Y]") should be (TypeAssignment(
          GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("X"))),
          InlineTypeAssertionForType(
            GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer"))),
            GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Y")))
          )
        ))
        parse("$x <= 3(3)[<3>]") should be (ValueAssignment(
          GlobalValueVar(GlobalVar[ValueKind.type](NamedIdentifier[ValueKind.type]("x"))),
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
          GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
          AnonymousMethod(
            // TODO: is this what we want when returning a bare `.x`?
            output = LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
            functionParams = ParamsDeclaration(NamedValueParamPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T")))),
            ))),
            typeFunctionParams = ParamsDeclaration(NamedParameterPack(Map(
              NamedIdentifier[TypeKind.type]("T") -> TypePlaceholder,
            ))),
          )
        ))
        parse("$f[\\.T](\\.x[.T]) <= $plus(.x, 3)") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
          AnonymousMethod(
            output = FunctionCall(
              GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("plus"))),
               PositionalParameterPack(ValueKind, Seq(
                LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
                IntegerLiteral(3),
              ))
            ),
            functionParams = ParamsDeclaration(NamedParameterPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T")))),
            ))),
            typeFunctionParams = ParamsDeclaration(NamedParameterPack(Map(
              NamedIdentifier[TypeKind.type]("T") -> TypePlaceholder,
            ))),
          )
        ))
        parse("$f(\\.x[\\.T]) <= 4") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
          AnonymousMethod(
            output = IntegerLiteral(4),
            functionParams = ParamsDeclaration(NamedValueParamPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T")))),
            ))),
            typeFunctionParams = ParamsDeclaration(NamedParameterPack(Map(NamedIdentifier[TypeKind.type]("T") -> TypePlaceholder))),
          )))
        parse("$f[\\.T] <= \\.x[.T]") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
          TypeParamsWrapperForValue(
            // TODO: is this what we want when returning a bare `.x`? I think so!!!
            subject = NamedValueParamPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T")))),
            )),
            tpp = NamedTypeParamPack(Map(
              NamedIdentifier[TypeKind.type]("T") -> TypePlaceholder,
            )),
            fromLocation = Left,
          ),
        ))
        parse("$f <= \\.x[\\.T]") should be (ValueAssignment(
          GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
          AnonymousMethod(
            output = NamedValuePack(Map(NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))))),
            functionParams = ParamsDeclaration(NamedValueParamPack(Map(
              NamedIdentifier[ValueKind.type]("x") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T")))),
            ))),
            typeFunctionParams = ParamsDeclaration(NamedParameterPack(Map(NamedIdentifier[TypeKind.type]("T") -> TypePlaceholder))))))
        parse("<$f> <- \\.T -> $F[.T]") should be (TypeAssertion(
          TypePlaceholder,
          TypePackWorkaround(AnonymousTypeMethod(
            output = TypeTypeFunctionCall(
              source =  GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("F"))),
              arguments = NamedParameterPack(Map(
                NamedIdentifier[TypeKind.type]("T") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T")))),
              ))
            ),
            typeFunctionParams = ParamsDeclaration(NamedParameterPack(Map(
              NamedIdentifier[TypeKind.type]("T") -> TypePlaceholder,
            ))),
          ))
        ))
      }
    }

    // TODO: make type parameter packs work!!!

    // TODO: implement the +{} parser syntax!!!
  }
}
