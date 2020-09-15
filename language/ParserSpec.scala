package funnel.language

import EntityData._
import FunnelPEG._

import org.junit.runner.RunWith
import org.parboiled2._
import org.scalatest._
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class ParserSpec extends FreeSpec with Matchers {

  def parse(input: String): Seq[Statement] = {
    val parser = new FunnelPEG(input)
    try { parser.Funnel.run().get } catch {
      case e: ParseError => throw new Exception(e.format(parser))
    }
  }

  def parseTypeExpression(input: String): TypeComponent = {
    val parser = new FunnelPEG(input)
    try { parser.ParseAllTypeExpressions.run().get } catch {
      case e: ParseError => throw new Exception(e.format(parser))
    }
  }
  def parseValueExpression(input: String): ValueComponent = {
    val parser = new FunnelPEG(input)
    try { parser.ParseAllValueExpressions.run().get } catch {
      case e: ParseError => throw new Exception(e.format(parser))
    }
  }

  def typeName(name: String) = NamedIdentifier[TypeKind.type](name)
  def valName(name: String) = NamedIdentifier[ValueKind.type](name)

  def globalType(name: String) = GlobalTypeVar(GlobalVar(typeName(name)))
  def globalValue(name: String) = GlobalValueVar(GlobalVar(valName(name)))

  def localType(name: String) = LocalTypeVar(LocalVar(LocalNamedIdentifier(typeName(name))))
  def localVal(name: String) = LocalValueVar(LocalVar(LocalNamedIdentifier(valName(name))))

  def typeGroup(inner: TypeComponent) = GroupingForType(inner)
  def valGroup(inner: ValueComponent) = GroupingForValue(inner)

  "FunnelPEG" - {
    "when parsing non-packed expressions" - {
      "should correctly parse literals" in {
        // Note: there are currently no "type literals".
        parseValueExpression("3") should be (IntegerLiteral(3))
        parseValueExpression("\"a\"") should be (StringLiteral("a"))
        parseValueExpression("0.1") should be (FloatLiteral(0.1))
      }
      "should correctly parse references to global variables" in {
        parseTypeExpression("$G") should be (globalType("G"))
        parseValueExpression("$g") should be (globalValue("g"))
      }
    }
    "when parsing packed expressions" - {
      "should correctly parse positional packs" in {
        parseTypeExpression("[$F, $G]") should be (PositionalTypePackExpression(Seq(
          globalType("F"), globalType("G")
        )))
        parseTypeExpression("[$F,]") should be (PositionalTypePackExpression(Seq(
          globalType("F"))))
        parseValueExpression("(2, 0.1,)") should be (PositionalValuePackExpression(Seq(
          IntegerLiteral(2), FloatLiteral(0.1),
        )))
        parseValueExpression("(3, )") should be (PositionalValuePackExpression(Seq(
          IntegerLiteral(3))))
      }
      "should correctly parse inline named packs" in {
        parseTypeExpression(".T,") should be (InlineNamedTypePack(
          name = typeName("T"),
          value = localType("T")))
        parseTypeExpression(".T[$Y],") should be (InlineNamedTypePack(
          name = typeName("T"),
          value = globalType("Y"),
        ))
        // TODO: make this exception type better?
        // Note that this fails, while [$Y, ] works, because that is a positional parameter
        // pack. The intent of explicitly failing for trailing commas is to avoid any confusion
        // about whether the grouped type/value is a parameter pack (and therefore an inline
        // function call), vs being an inline pack expression.
        val caughtTypeGroup = intercept[Exception] {
          parseTypeExpression(".T[$Y,],")
        }
        assert(caughtTypeGroup.getMessage
          .contains("a trailing comma is not allowed in the definition section of a named type pack"))
        parseValueExpression(".x,") should be (InlineNamedValuePack(
          name = valName("x"),
          value = localVal("x"),
        ))
        parseValueExpression(".x(3),") should be (InlineNamedValuePack(
          name = valName("x"),
          value = IntegerLiteral(3),
        ))
        val caughtValueGroup = intercept[Exception] {
          parseValueExpression(".x(3,),")
        }
        assert(caughtValueGroup.getMessage
          .contains("a trailing comma is not allowed in the definition section of a named value pack"))
      }
      "should correctly parse non-inline named packs" in {
        parseTypeExpression("[.T, .X[.Y]]") should be (NamedTypePackExpressionNoInline(Map(
          typeName("T") -> localType("T"),
          typeName("X") -> localType("Y"),
        )))
        parseValueExpression("(.x, .y(.z))") should be (NamedValuePackExpressionNoInline(Map(
          valName("x") -> localVal("x"),
          valName("y") -> localVal("z"),
        )))
        parseTypeExpression("[.T, .X[.Y,],]") should be (NamedTypePackExpressionNoInline(Map(
          typeName("T") -> localType("T"),
          typeName("X") -> InlineNamedTypePack(typeName("Y"), localType("Y")),
        )))
        parseValueExpression("(.x, .y(.z,),)") should be (NamedValuePackExpressionNoInline(Map(
          valName("x") -> localVal("x"),
          valName("y") -> InlineNamedValuePack(valName("z"), localVal("z")),
        )))
      }
    }

    // "when parsing top-level expressions" - {
    //   "should correctly parse variable and type assignments" in {
    //     parse("$x <= 3") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar[ValueKind.type](NamedIdentifier[ValueKind.type]("x"))),
    //       IntegerLiteral(3),
    //     )))
    //     parse("$x <= (\\.x, \\.y)") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar[ValueKind.type](NamedIdentifier[ValueKind.type]("x"))),
    //       AnonymousMethod(
    //         output = NamedValuePack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
    //           NamedIdentifier[ValueKind.type]("y") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("y"))))
    //         )),
    //         functionParams = ParamsDeclaration(NamedValueParamPack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> TypePlaceholder,
    //           NamedIdentifier[ValueKind.type]("y") -> TypePlaceholder,
    //         ))),
    //       )
    //     )))
    //     parse("$x <= (3, \"a\", 0.0)") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar[ValueKind.type](NamedIdentifier[ValueKind.type]("x"))),
    //       PositionalValueParameterPack(Seq(
    //         IntegerLiteral(3),
    //         StringLiteral("a"),
    //         FloatLiteral(0.0),
    //       )),
    //     )))
    //     parse("$point <= (.x(2 <!- [$Integer]), .y <= (3))") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("point"))),
    //       NamedValuePack(Map(
    //         NamedIdentifier[ValueKind.type]("x") -> InlineTypeAssertionForValue(IntegerLiteral(2), GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))),
    //         NamedIdentifier[ValueKind.type]("y") -> IntegerLiteral(3),
    //       ))
    //     )))
    //     parse("$X <- $Y") should be (Seq(TypeAssignment(
    //       GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("X"))),
    //       GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Y"))),
    //     )))
    //     parse("$list[\\.El] <= (\\+empty, \\+cons(\\.car <- .El, \\.cdr <- .Self))") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("list"))),
    //       TypeParamsWrapperForValue(
    //         subject = EnumLiteralValue(Seq(
    //           AlternationCase(AlternationCaseName("empty"), StructTypeLiteral(ParameterPackKinds.empty)),
    //           AlternationCase(AlternationCaseName("cons"), StructTypeLiteral(NamedParameterPack(Map(
    //             NamedIdentifier[ValueKind.type]("car") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("El")))),
    //             NamedIdentifier[ValueKind.type]("cdr") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("Self")))),
    //           ))))
    //         )),
    //         tpp = NamedTypeParamPack(Map(
    //           NamedIdentifier[TypeKind.type]("El") -> TypePlaceholder,
    //         )),
    //         fromLocation = Left,
    //       ),
    //     )))
    //     parse("$X <- <(.x[$Integer])>") should be (Seq(TypeAssignment(
    //       GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("X"))),
    //       StructTypeLiteral(NamedParameterPack(Map(
    //         NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))
    //       ))))))
    //   }

    //   "should be possible to define functions with a variable assignment" in {
    //     parse("$f <= (\\.x <- $Integer) => .x") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //       AnonymousMethod(
    //         output = NamedValuePack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
    //         )),
    //         functionParams = ParamsDeclaration(NamedValueParamPack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))
    //         ))),
    //       )
    //     )))
    //     parse("$f <= \\.x[$Integer]") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //       AnonymousMethod(
    //         output = NamedValuePack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
    //         )),
    //         functionParams = ParamsDeclaration(NamedValueParamPack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer"))),
    //         ))),
    //       )
    //     )))
    //     parse("$F <- [(\\.x[$Integer]) => <.x>]") should be (Seq(TypeAssignment(
    //       GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("F"))),
    //       MethodSignature(
    //         output = StructTypeLiteral(NamedParameterPack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> TypePlaceholder,
    //         ))),
    //         functionParams = ParamsDeclaration(NamedParameterPack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))
    //         ))),
    //       )
    //     )))
    //   }

    //   "should respect the associativity of the arrow operators" in {
    //     parse("$f <= \\.x <- $Integer => .x") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //       AnonymousMethod(
    //         output = NamedValuePack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
    //         )),
    //         functionParams = ParamsDeclaration(NamedValueParamPack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))
    //         ))),
    //       )
    //     )))
    //   }

    //   "should allow some or no whitespace after all terminals and identifiers" in {
    //     parse("$f <= ( \\.x <- $Integer ) => .x") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //       AnonymousMethod(
    //         output = NamedValuePack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
    //         )),
    //         functionParams = ParamsDeclaration(NamedValueParamPack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))
    //         ))),
    //       )
    //     )))
    //     parse("$f<=\\.x<-$Integer=>.x") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //       AnonymousMethod(
    //         output = NamedValuePack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
    //         )),
    //         functionParams = ParamsDeclaration(NamedValueParamPack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer")))
    //         ))),
    //       )
    //     )))
    //   }

    //   "should allow value and/or type assertions at the top level" in {
    //     parse("3 <!= 3") should be (Seq(ValueAssertion(
    //       IntegerLiteral(3),
    //       IntegerLiteral(3)
    //     )))
    //     parse("<$f> <!- [\\.p => <.p>]") should be (Seq(TypeAssertion(
    //       TypePlaceholder,
    //       MethodSignature(
    //         output = StructTypeLiteral(NamedParameterPack(Map(
    //           NamedIdentifier[ValueKind.type]("p") -> TypePlaceholder,
    //         ))),
    //         functionParams = ParamsDeclaration(NamedParameterPack(Map(
    //           NamedIdentifier[ValueKind.type]("p") -> TypePlaceholder,
    //         ))),
    //       )
    //     )))
    //   }

    //   // TODO: figure out a syntax for this that isn't the *EXACT* same as the one for type and/or
    //   // value function calls!!
    //   "should allow inline type and/or value assertions" in {
    //     parse("3 <!= 3 <!- [$Integer]") should be (Seq(ValueAssertion(
    //       IntegerLiteral(3),
    //       InlineTypeAssertionForValue(IntegerLiteral(3), GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer"))))
    //     )))
    //     parse("$x <= 3 <!= (3)") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar[ValueKind.type](NamedIdentifier[ValueKind.type]("x"))),
    //       InlineValueAssertion(IntegerLiteral(3), IntegerLiteral(3)),
    //     )))
    //     parse("$X <!- $Integer <!- [$Y]") should be (Seq(TypeAssertion(
    //       GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("X"))),
    //       InlineTypeAssertionForType(
    //         GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Integer"))),
    //         GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("Y")))
    //       )
    //     )))
    //     parse("$x <= 3<!=(3)<!-[<3>]") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar[ValueKind.type](NamedIdentifier[ValueKind.type]("x"))),
    //       InlineValueAssertion(
    //         IntegerLiteral(3),
    //         InlineTypeAssertionForValue(
    //           IntegerLiteral(3),
    //           IntegerTypeLiteral,
    //         ),
    //       ),
    //     )))
    //   }

    //   // TODO: implement implicit conversions!!!
    //   "should be (Seqable) to register and declare implicit conversions with ~>" in {
    //     // parse("$x <~ 3[$Integer]") should be
    //     // parse("$f <~ \\.x[$String] => $string-length")
    //     // parse("")
    //   }
    // }

    // "when intermixing type and value parameter packs" - {
    //   "should correctly handle type parameter packs in sequence" in {
    //     parse("$f <= \\.T -> \\.x[.T] => .x") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //       AnonymousMethod(
    //         output = NamedValuePack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
    //         )),
    //         functionParams = ParamsDeclaration(NamedValueParamPack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T")))),
    //         ))),
    //         typeFunctionParams = ParamsDeclaration(NamedParameterPack(Map(
    //           NamedIdentifier[TypeKind.type]("T") -> TypePlaceholder,
    //         ))),
    //       )
    //     )))
    //     parse("$f[\\.T](\\.x[.T]) <= $plus(.x, 3)") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //       AnonymousMethod(
    //         output = FunctionCall(
    //           GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("plus"))),
    //            PositionalParameterPack(ValueKind, Seq(
    //             NamedValuePack(Map(
    //               NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
    //             )),
    //             IntegerLiteral(3),
    //           ))
    //         ),
    //         functionParams = ParamsDeclaration(NamedParameterPack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T")))),
    //         ))),
    //         typeFunctionParams = ParamsDeclaration(NamedParameterPack(Map(
    //           NamedIdentifier[TypeKind.type]("T") -> TypePlaceholder,
    //         ))),
    //       )
    //     )))
    //     parse("$f(\\.x[\\.T]) <= 4") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //       AnonymousMethod(
    //         output = IntegerLiteral(4),
    //         functionParams = ParamsDeclaration(NamedValueParamPack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T")))),
    //         ))),
    //         typeFunctionParams = ParamsDeclaration(NamedParameterPack(Map(NamedIdentifier[TypeKind.type]("T") -> TypePlaceholder))),
    //       ))))
    //     parse("$f[\\.T] <= \\.x[.T]") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //       TypeParamsWrapperForValue(
    //         // TODO: is this what we want when returning a bare `.x`? I think so!!!
    //         subject = AnonymousMethod(
    //           output = NamedValuePack(Map(
    //             NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))),
    //           )),
    //           functionParams = ParamsDeclaration(NamedValueParamPack(Map(
    //             NamedIdentifier[ValueKind.type]("x") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T")))),
    //           ))),
    //         ),
    //         tpp = NamedTypeParamPack(Map(
    //           NamedIdentifier[TypeKind.type]("T") -> TypePlaceholder,
    //         )),
    //         fromLocation = Left,
    //       ),
    //     )))
    //     parse("$f <= \\.x[\\.T]") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //       AnonymousMethod(
    //         output = NamedValuePack(Map(NamedIdentifier[ValueKind.type]("x") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("x")))))),
    //         functionParams = ParamsDeclaration(NamedValueParamPack(Map(
    //           NamedIdentifier[ValueKind.type]("x") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T")))),
    //         ))),
    //         typeFunctionParams = ParamsDeclaration(NamedParameterPack(Map(NamedIdentifier[TypeKind.type]("T") -> TypePlaceholder)))))))
    //     parse("<$f> <!- \\.T -> $F[.T]") should be (Seq(TypeAssertion(
    //       TypePlaceholder,
    //       TypePackWorkaround(AnonymousTypeMethod(
    //         output = TypeTypeFunctionCall(
    //           source =  GlobalTypeVar(GlobalVar[TypeKind.type](NamedIdentifier[TypeKind.type]("F"))),
    //           arguments = NamedParameterPack(Map(
    //             NamedIdentifier[TypeKind.type]("T") -> LocalTypeVar(LocalVar[TypeKind.type](LocalNamedIdentifier[TypeKind.type](NamedIdentifier[TypeKind.type]("T")))),
    //           ))
    //         ),
    //         typeFunctionParams = ParamsDeclaration(NamedParameterPack(Map(
    //           NamedIdentifier[TypeKind.type]("T") -> TypePlaceholder,
    //         ))),
    //       ))
    //     )))
    //   }
    // }

    // def parseCurriedFunction(input: String): CurriedFunctionCall = {
    //   val parser = new FunnelPEG(input)
    //   try { parser.ParseFunctionCallCurrying.run().get } catch {
    //     case e: ParseError => throw new Exception(e.format(parser))
    //   }
    // }

    // // TODO: make curried arguments work!!!
    // "when using curried (,...) operators" - {
    //   "should allow currying across multiple consecutive parameter packs" in {
    //     parseCurriedFunction("$f <= (.x(3),...) <= (.y,...) <= .z,...") should be (CurriedFunctionCall(
    //       source = GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //       arguments = NamedValuePack(Map(
    //         NamedIdentifier[ValueKind.type]("x") -> IntegerLiteral(3),
    //         NamedIdentifier[ValueKind.type]("y") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("y")))),
    //         NamedIdentifier[ValueKind.type]("z") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("z")))),
    //       )),
    //     ))
    //     // parse("$x <= $f <= (.x(3),...) <= (.y,...) <= .z,...") should be (Seq(ValueAssignment(
    //     //   GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("x"))),
    //     //   CurriedFunctionCall(
    //     //     source = GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //     //     arguments = NamedValuePack(Map(
    //     //       NamedIdentifier[ValueKind.type]("x") -> IntegerLiteral(3),
    //     //       NamedIdentifier[ValueKind.type]("y") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("y")))),
    //     //       NamedIdentifier[ValueKind.type]("z") -> LocalValueVar(LocalVar[ValueKind.type](LocalNamedIdentifier[ValueKind.type](NamedIdentifier[ValueKind.type]("z")))),
    //     //     )),
    //     //   )
    //     // )))
    //     parse("$x <= $f <= 3,...") should be (Seq(ValueAssignment(
    //       GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("x"))),
    //       CurriedFunctionCall(
    //         source = GlobalValueVar(GlobalVar(NamedIdentifier[ValueKind.type]("f"))),
    //         arguments = NamedValuePack(Map(
    //           NamedIdentifier[ValueKind.type]("-1") -> PositionalValueParameterPack(Seq(IntegerLiteral(3))),
    //         )),
    //       )
    //     )))
    //   }
    // }

    // TODO: implement destructuring with +(+(...), ...)!!!

    // TODO: implement the +(+{...}) parser syntax!!!
  }
}
