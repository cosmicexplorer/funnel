package funnel.language

import org.parboiled2._

import scala.util.matching.Regex



object Errors {
  import EntityData._
  // import FunnelPEG._
  // import Packs._

  sealed abstract class FunnelParseError(message: String) extends Exception(message)

  case class InvalidIndexError(index: Int, message: String) extends FunnelParseError(message)

  sealed abstract class InternalError(location: String, message: String)
      extends FunnelParseError(s"internal error in $location: $message")

  case class InvalidParameterPackConstruction(
    message: String,
  ) extends InternalError("parameter pack construction", message)

  object InvalidIdentifierKind {
    def apply[Kind <: ExpressionKinds](
      id: NamedIdentifier[Kind],
      pattern: Regex,
    )(implicit kind: Kind): InvalidIdentifierKind[Kind] = InvalidIdentifierKind(kind, id, pattern)
  }
  case class InvalidIdentifierKind[Kind <: ExpressionKinds](
    kind: Kind,
    id: NamedIdentifier[Kind],
    pattern: Regex,
  ) extends FunnelParseError(s"$kind identifier recognition for $id failed: did not match $pattern")

  case class ExceptionHandlingError(message: String)
      extends InternalError("while handling an exception", message)

  case class ChainJoiningError[K <: ExpressionKinds, V <: ExpressionKinds](
    lhs: GeneralizedExpressionChain[K],
    rhs: GeneralizedExpressionChain[V],
    message: String,
  ) extends FunnelParseError(s"could not join expression chains $lhs and $rhs: $message")

  // case class PullOutNewTypeParamsError(
  //   parent: AstNode, typeParams: TypeParamPack, message: String,
  // ) extends InternalError(
  //   s"while trying to pull out type params $typeParams from parent $parent",
  //   message)
}


// Attributes of AST nodes (data they contain in public fields):
object EntityData {
  import Errors._
  import Packs._
  // import FunnelPEG._

  object ExpressionKinds {
    implicit val _v = ValueKind
    implicit val _t = TypeKind
  }
  sealed abstract class ExpressionKinds
  case object ValueKind extends ExpressionKinds
  case object TypeKind extends ExpressionKinds

  // Define classes which are *not* AST nodes.
  case class OneIndexedPosition[Kind <: ExpressionKinds](position: Int) {
    if (position <= 0) {
      throw InvalidIndexError(position, s"position must be greater than 0, but was $position")
    }

    def intoIdentifier(implicit kind: Kind): NamedIdentifier[Kind] = {
      // NB: This is how you can match on a compile time-resolved case object!!!
      val generalKind: ExpressionKinds = kind
      val name = generalKind match {
        case ValueKind => s"-${position}"
        case TypeKind => s"_${position}"
      }
      NamedIdentifier[Kind](name)
    }
  }

  object NamedIdentifier {
    val nonEmptyIdentifier = """.+""".r
    val valueIdentifier = """[a-z\-][a-zA-Z\-_0-9]*""".r
    val typeIdentifier = """[A-Z_][a-zA-Z\-_0-9]*""".r
  }
  case class NamedIdentifier[Kind <: ExpressionKinds](name: String)(implicit kind: Kind) {
    import NamedIdentifier._

    name match {
      case nonEmptyIdentifier() => ()
      case _ => throw InvalidIdentifierKind(this, nonEmptyIdentifier)
    }
    val generalKind: ExpressionKinds = kind
    generalKind match {
      case ValueKind => name match {
        case valueIdentifier() => ()
        case _ => throw InvalidIdentifierKind(this, valueIdentifier)
      }
      case TypeKind => name match {
        case typeIdentifier() => ()
        case _ => throw InvalidIdentifierKind(this, typeIdentifier)
      }
    }
  }

  trait HasNamedIdentifier[Kind <: ExpressionKinds] {
    def namedIdentifier: NamedIdentifier[Kind]
  }

  // Capitalization rules are enforced for types and values, so we need to have the kind parameter
  // available even up here.
  sealed abstract class LocalIdentifierKinds[Kind <: ExpressionKinds]
      extends HasNamedIdentifier[Kind]
  case class LocalNamedIdentifier[Kind <: ExpressionKinds](id: NamedIdentifier[Kind])
      extends LocalIdentifierKinds[Kind] {
    override def namedIdentifier = id
  }
  case class PositionalIdentifier[Kind <: ExpressionKinds](
    position: OneIndexedPosition[Kind],
  )(implicit kind: Kind) extends LocalIdentifierKinds[Kind] {
    override def namedIdentifier = position.intoIdentifier
  }

  // Describes the symbol used to identify an alternation case, e.g. '+a'.
  case class AlternationCaseName[Kind <: ExpressionKinds](id: NamedIdentifier[Kind])
    extends HasNamedIdentifier[Kind] {
    override def namedIdentifier = id
  }

  sealed trait Entity[Kind <: ExpressionKinds]

  sealed abstract class Placeholder[Kind <: ExpressionKinds] extends Entity[Kind]
  case object ValuePlaceholder extends Placeholder[ValueKind.type]
  case object TypePlaceholder extends Placeholder[TypeKind.type]

  sealed abstract class VarKinds[Kind <: ExpressionKinds]
      extends Entity[Kind]
      with HasNamedIdentifier[Kind]
  case class GlobalVar[Kind <: ExpressionKinds](id: NamedIdentifier[Kind])
      extends VarKinds[Kind] {
    override def namedIdentifier = id
  }
  case class LocalVar[Kind <: ExpressionKinds, LocalKind <: LocalIdentifierKinds[Kind]](
    localId: LocalKind,
  ) extends VarKinds[Kind] {
    override def namedIdentifier: NamedIdentifier[Kind] = localId.namedIdentifier
  }

  sealed abstract class LiteralKinds extends Entity[ValueKind.type]
  case class StrLit(s: String) extends LiteralKinds
  case class IntLit(n: Int) extends LiteralKinds
  case class FloatLit(f: Double) extends LiteralKinds

  sealed abstract class CallLike[Kind <: ExpressionKinds] extends Entity[Kind]
  case class ValueValueFunctionCall(
    source: Entity[ValueKind.type],
    arguments: ValuePackPairs,
  ) extends CallLike[ValueKind.type]
  case class ValueTypeFunctionCall(
    source: Entity[ValueKind.type],
    arguments: TypePackPairs,
  ) extends CallLike[ValueKind.type]
  case class TypeTypeFunctionCall(
    source: Entity[TypeKind.type],
    arguments: TypePackPairs,
  ) extends CallLike[TypeKind.type]


  // TODO: Spread and type extraction operators!!!
  // Spread operator. These are their own atoms becaue they don't mix with other params.
  // sealed abstract class SpreadOperatorKinds[Kind <: ExpressionKinds](
  //   source: Entity[Kind],
  // ) extends Entity[Kind]
  // case class ValueSpreadOperator(source: Entity[ValueKind.type])
  //     extends SpreadOperatorKinds[ValueKind.type](source)
  // case class TypeSpreadOperator(source: Entity[TypeKind.type])
  //     extends SpreadOperatorKinds[TypeKind.type](source)
  // sealed abstract class SpreadParamKinds[Kind <: ExpressionKinds](name: NamedIdentifier[Kind])
  //     extends Entity[Kind]
  // case class ValueSpreadParam(name: NamedIdentifier[ValueKind.type])
  //     extends SpreadParamKinds[ValueKind.type](name)
  // case class TypeSpreadParam(name: NamedIdentifier[TypeKind.type])
  //     extends SpreadParamKinds[TypeKind.type](name)

  // sealed abstract class TypeExtractor extends Entity[TypeKind.type]
  // case class TypeValueExtractionOperator(source: Entity[ValueKind.type]) extends TypeExtractor
  // case class TypeTypeExtractionOperator(source: Entity[TypeKind.type]) extends TypeExtractor

  // NB: enum CASES are not considered "atoms"!!
  sealed abstract class EnumCaseLike[Kind <: ExpressionKinds]
  case class EnumValueCaseDecl(
    name: AlternationCaseName[ValueKind.type],
    defn: ValueParamsPairs,
  ) extends EnumCaseLike[ValueKind.type]
  case class EnumTypeCaseDecl(
    name: AlternationCaseName[TypeKind.type],
    defn: TypeParamsPairs,
  ) extends EnumCaseLike[TypeKind.type]
  sealed abstract class EnumLike[Kind <: ExpressionKinds] extends Entity[Kind]
  case class EnumValueDecl(cases: Seq[EnumValueCaseDecl]) extends EnumLike[ValueKind.type]
  case class EnumTypeDecl(cases: Seq[EnumTypeCaseDecl]) extends EnumLike[TypeKind.type]
  sealed abstract class EnumCaseDereference[Kind <: ExpressionKinds] extends Entity[Kind]
  case class EnumValueCaseDereference(
    name: AlternationCaseName[ValueKind.type],
    arguments: ValuePackPairs,
  ) extends EnumCaseDereference[ValueKind.type]
  case class EnumTypeCaseDereference(
    name: AlternationCaseName[TypeKind.type],
    arguments: TypePackPairs,
  ) extends EnumCaseDereference[TypeKind.type]

  sealed abstract class AssertionLike[Kind <: ExpressionKinds] extends Entity[Kind]
  case class ValueValueAssertion(
    lhs: Entity[ValueKind.type],
    rhs: Entity[ValueKind.type],
  ) extends AssertionLike[ValueKind.type]
  case class ValueTypeAssertion(
    lhs: Entity[ValueKind.type],
    rhs: Entity[TypeKind.type],
  ) extends AssertionLike[ValueKind.type]
  case class TypeTypeAssertion(
    lhs: Entity[TypeKind.type],
    rhs: Entity[TypeKind.type],
  ) extends AssertionLike[TypeKind.type]

  sealed abstract class GroupingLike[Kind <: ExpressionKinds] extends Entity[Kind]
  case class ValueGrouping(inner: Entity[ValueKind.type]) extends Entity[ValueKind.type]
  case class TypeGrouping(inner: Entity[TypeKind.type]) extends Entity[TypeKind.type]

  // These need not be atoms!
  sealed abstract class GlobalAssignment[Kind <: ExpressionKinds]
  case class GlobalValueAssignment(
    place: GlobalVar[ValueKind.type],
    source: Entity[ValueKind.type],
  ) extends GlobalAssignment[ValueKind.type]
  case class GlobalTypeAssignment(
    place: GlobalVar[TypeKind.type],
    source: Entity[TypeKind.type],
  ) extends GlobalAssignment[TypeKind.type]

  // Chains!!!
  sealed trait ChainElement[Kind <: ExpressionKinds] {
    def intoChain: GeneralizedExpressionChain[Kind]
  }
  case class ValueChainElement(
    typeParams: TypeParamsPairs,
    valueParams: ValueParamsPairs,
    valuePack: ValuePackPairs,
  ) extends ChainElement[ValueKind.type] {
    override def intoChain = ValueChain(Seq(this))
  }
  case class TypeChainElement(
    typeParams: TypeParamsPairs,
    typePack: TypePackPairs,
  ) extends ChainElement[TypeKind.type] {
    override def intoChain = TypeChain(Seq(this))
  }

  object GeneralizedExpressionChain {
    implicit object MergeValueHelper extends Mer[GV] {
      override def zero(): GV = ValueChain(Seq())
      override def mergeWith(lhs: GV, rhs: GV): GV = lhs.joinValue(rhs)
    }
    implicit object MergeTypeHelper extends Mer[GT] {
      override def zero(): GT = TypeChain(Seq())
      override def mergeWith(lhs: GT, rhs: GT): GT = lhs.joinType(rhs)
    }
    implicit object MergeTypeValueHelper extends MergeSubexpressions[GV, GT] {
      override def zero(): GV = ValueChain(Seq())
      override def mergeWith(lhs: GV, rhs: GT): GV = rhs.joinValue(lhs)
    }
  }
  type GV = GeneralizedExpressionChain[ValueKind.type]
  type GT = GeneralizedExpressionChain[TypeKind.type]
  sealed trait GeneralizedExpressionChain[Kind <: ExpressionKinds] extends Entity[Kind] {
    type TypeOutput <: GT
    type ValueOutput <: GV
    def joinType(other: GT): TypeOutput
    def joinValue(other: GV): ValueOutput
  }
  case class ValueChain(elements: Seq[ValueChainElement]) extends GV {
    type TypeOutput = GT
    type ValueOutput = ValueChain
    override def joinType(other: GT): TypeOutput =
      other match {
        case TypeChain(_) =>
          throw ChainJoiningError(this, other, "value chain cannot be prepended to type chain")
      }
    override def joinValue(other: GV): ValueOutput =
      other match {
        case ValueChain(otherEls) => ValueChain(elements ++ otherEls)
        case TypeIntoValueLink(_, _) =>
          throw ChainJoiningError(this, other, "value chain cannot be prepended to type chain")
      }
  }
  case class TypeChain(elements: Seq[TypeChainElement]) extends GT {
    type TypeOutput = TypeChain
    type ValueOutput = TypeIntoValueLink
    override def joinType(other: GT): TypeOutput =
      other match {
        case TypeChain(otherEls) => TypeChain(elements ++ otherEls)
      }
    override def joinValue(other: GV): ValueOutput =
      other match {
        case other: ValueChain => TypeIntoValueLink(this, other)
        case TypeIntoValueLink(lhs, rhs) => joinType(lhs).joinValue(rhs)
      }
  }
  case class TypeIntoValueLink(lhs: TypeChain, rhs: ValueChain) extends GV {
    type TypeOutput = GT
    type ValueOutput = TypeIntoValueLink
    override def joinType(other: GT): TypeOutput =
      other match {
        case TypeChain(_) =>
          throw ChainJoiningError(this, other, "type-into-value chain cannot be prepended to type chain")
      }
    override def joinValue(other: GV): ValueOutput =
      other match {
        case other: ValueChain => TypeIntoValueLink(lhs, rhs.joinValue(other))
        case TypeIntoValueLink(_, _) =>
          throw ChainJoiningError(this, other, "type-into-value chain cannot be prepended to type chain")
      }
  }

  // Used to canonicalize the process of parsing a sequence of inline expressions (parameter
  // declarations, value/type packs) as a subroutine to parse non-inline (parenthesized/bracketed)
  // forms of the same expression type.
  object MergeSubexpressions {
    implicit class MergeSubHelper[Parent, Sub](tuple: (Parent, Seq[Sub])) {
      private val (init, fields) = tuple
      def foldAlong()(implicit m: MergeSubexpressions[Parent, Sub]): Parent =
        fields.foldLeft(init)(m.mergeWith(_, _))
    }
    implicit class MergeSelves[Sub](fields: Seq[Sub]) {
      def mergeAlong()(implicit m: Mer[Sub]): Sub =
        fields.foldLeft(m.zero())(m.mergeWith(_, _))
    }
  }
  trait MergeSubexpressions[Parent, Sub] {
    def zero(): Parent
    def mergeWith(lhs: Parent, rhs: Sub): Parent
  }
  trait Mer[T] extends MergeSubexpressions[T, T]

  // Define parameter packing types.
  object PackPairs {
    type Protocol[K <: ExpressionKinds, V <: ExpressionKinds] = Seq[(PackName[K], Entity[V])]
    def protocol[K <: ExpressionKinds, V <: ExpressionKinds](
      init: Protocol[K, V] = Seq(),
    ): Protocol[K, V] = init
  }
  sealed abstract class PackPairs[K <: ExpressionKinds, V <: ExpressionKinds](
    protocol: PackPairs.Protocol[K, V],
  )
  // NB: We are intentionally restricting the types of pairs that can go together here because
  // TypeValue "doesn't make sense".
  object ValueValue {
    implicit object PackWrapperHelper
        extends PackMerger[ValueKind.type, ValueKind.type, ValueValue] {
      override def fromGenericPack(
        genericPack: PackPairs.Protocol[ValueKind.type, ValueKind.type],
      ): ValueValue = ValueValue(genericPack)
      override def intoGenericPack(
        self: ValueValue,
      ): PackPairs.Protocol[ValueKind.type, ValueKind.type] = self.protocol
    }
  }
  case class ValueValue(
    protocol: PackPairs.Protocol[ValueKind.type, ValueKind.type] = Seq(),
  ) extends PackPairs[ValueKind.type, ValueKind.type](protocol)
  object ValueType {
    implicit object PackWrapperHelper extends PackMerger[ValueKind.type, TypeKind.type, ValueType] {
      override def fromGenericPack(
        genericPack: PackPairs.Protocol[ValueKind.type, TypeKind.type],
      ): ValueType = ValueType(genericPack)
      override def intoGenericPack(
        self: ValueType,
      ): PackPairs.Protocol[ValueKind.type, TypeKind.type] = self.protocol
    }
  }
  case class ValueType(
    protocol: PackPairs.Protocol[ValueKind.type, TypeKind.type] = Seq(),
  ) extends PackPairs[ValueKind.type, TypeKind.type](protocol)
  object TypeType {
    implicit object PackWrapperHelper extends PackMerger[TypeKind.type, TypeKind.type, TypeType] {
      override def fromGenericPack(
        genericPack: PackPairs.Protocol[TypeKind.type, TypeKind.type],
      ): TypeType = TypeType(genericPack)
      override def intoGenericPack(
        self: TypeType,
      ): PackPairs.Protocol[TypeKind.type, TypeKind.type] = self.protocol
    }
  }
  case class TypeType(
    protocol: PackPairs.Protocol[TypeKind.type, TypeKind.type] = Seq(),
  ) extends PackPairs[TypeKind.type, TypeKind.type](protocol)
  // NB: TypeValue "doesn't make sense"!

  // Define some traits and implicits that make them nice to play with.
  object From {
    implicit class ConvertFromHelper[A](a: A) {
      def convertFrom[B](implicit c: From[A, B]): B = c.from(a)
      def convertFromFrom[B, C](implicit c1: From[A, B], c2: From[B, C]): C = c2.from(c1.from(a))
      def convertFromInto[B, C](implicit c1: From[A, B], c2: Into[C, B]): C = c2.into(c1.from(a))
    }
  }
  trait From[A, B] {
    def from(a: A): B
  }

  object Into {
    implicit class ConvertIntoHelper[C](c: C) {
      def convertInto[A](implicit i: Into[A, C]): A = i.into(c)
      def convertIntoInto[A, B](implicit i1: Into[A, B], i2: Into[B, C]): A = i1.into(i2.into(c))
      def convertIntoFrom[A, B](implicit i1: Into[A, B], i2: From[C, B]): A = i1.into(i2.from(c))
    }
  }
  trait Into[A, B] {
    def into(b: B): A
  }

  trait Converter[A, B] extends From[A, B] with Into[A, B]

  trait PackWrapper[
    K <: ExpressionKinds,
    V <: ExpressionKinds,
    T,
  ] extends Converter[PackPairs.Protocol[K, V], T] {
    def fromGenericPack(genericPack: PackPairs.Protocol[K, V]): T
    override def from(genericPack: PackPairs.Protocol[K, V]): T = fromGenericPack(genericPack)
    def intoGenericPack(self: T): PackPairs.Protocol[K, V]
    override def into(self: T): PackPairs.Protocol[K, V] = intoGenericPack(self)
  }

  trait PackMerger[
    K <: ExpressionKinds,
    V <: ExpressionKinds,
    T,
  ] extends PackWrapper[K, V, T]
      with Mer[T] {
    override def zero() = fromGenericPack(Seq())
    override def mergeWith(lhs: T, rhs: T) =
      fromGenericPack(intoGenericPack(lhs) ++ intoGenericPack(rhs))
  }

  // This trait allows you to enjoy the benefits of going to and from generic packs, while only
  // maintaining a separate Convert[_, _] implicit.
  // trait ExtendedPackWrapper[
  //   K <: ExpressionKinds,
  //   V <: ExpressionKinds,
  //   A,
  //   B,
  // ] extends PackMerger[K, V, B] {
  //   override def fromGenericPack(genericPack: PackPairs.Protocol[K, V])(
  //     implicit c: Converter[A, B], m: PackMerger[K, V, A],
  //   ): B = c.from(m.fromGenericPack(genericPack))
  //   override def intoGenericPack(self: B)(
  //     implicit c: Converter[A, B], m: PackMerger[K, V, A],
  //   ): PackPairs.Protocol[K, V] = m.intoGenericPack(c.into(self))
  // }

  // Create EVEN MORE specific wrappers for use cases of each PackPairs[_, _]!!
  sealed abstract class ParamPackWrapperKinds[
    K <: ExpressionKinds,
    V <: ExpressionKinds,
    PP <: PackPairs[K, V],
  ](inner: PP)
  sealed abstract class ParamPackWrapperCompanion[
    K <: ExpressionKinds,
    V <: ExpressionKinds,
    PP <: PackPairs[K, V],
    W <: ParamPackWrapperKinds[K, V, PP]
  ] {
    type PM = PackMerger[K, V, W]
    type Prot = PackPairs.Protocol[K, V]
    def PackM: PM
    def empty = PackM.zero()
    def ConverterHelper: Converter[PP, W]
    // type EM = ExtendedPackWrapper[K, V, PP, W]
    // implicit object Ext extends EM
  }
  object ValueParamsPairs
      extends ParamPackWrapperCompanion[ValueKind.type, TypeKind.type, ValueType, ValueParamsPairs] {
    implicit object PackM extends PM {
      override def fromGenericPack(genericPack: Prot) = ValueParamsPairs(ValueType(genericPack))
      override def intoGenericPack(self: ValueParamsPairs): Prot = self.inner.protocol
    }
    implicit object ConverterHelper extends Converter[ValueType, ValueParamsPairs] {
      override def from(a: ValueType): ValueParamsPairs = ValueParamsPairs(a)
      override def into(b: ValueParamsPairs): ValueType = b.inner
    }
  }
  case class ValueParamsPairs(inner: ValueType)
      extends ParamPackWrapperKinds[ValueKind.type, TypeKind.type, ValueType](inner)
  object TypeParamsPairs
      extends ParamPackWrapperCompanion[TypeKind.type, TypeKind.type, TypeType, TypeParamsPairs] {
    implicit object PackM extends PM {
      override def fromGenericPack(genericPack: Prot) = TypeParamsPairs(TypeType(genericPack))
      override def intoGenericPack(self: TypeParamsPairs): Prot = self.inner.protocol
    }
    implicit object ConverterHelper extends Converter[TypeType, TypeParamsPairs] {
      override def from(a: TypeType): TypeParamsPairs = TypeParamsPairs(a)
      override def into(b: TypeParamsPairs): TypeType = b.inner
    }
  }
  case class TypeParamsPairs(inner: TypeType)
      extends ParamPackWrapperKinds[TypeKind.type, TypeKind.type, TypeType](inner)
  object ValuePackPairs
      extends ParamPackWrapperCompanion[ValueKind.type, ValueKind.type, ValueValue, ValuePackPairs] {
    implicit object PackM extends PM {
      override def fromGenericPack(genericPack: Prot) = ValuePackPairs(ValueValue(genericPack))
      override def intoGenericPack(self: ValuePackPairs): Prot = self.inner.protocol
    }
    implicit object ConverterHelper extends Converter[ValueValue, ValuePackPairs] {
      override def from(a: ValueValue): ValuePackPairs = ValuePackPairs(a)
      override def into(b: ValuePackPairs): ValueValue = b.inner
    }
  }
  case class ValuePackPairs(inner: ValueValue)
      extends ParamPackWrapperKinds[ValueKind.type, ValueKind.type, ValueValue](inner)
  object TypePackPairs
      extends ParamPackWrapperCompanion[TypeKind.type, TypeKind.type, TypeType, TypePackPairs] {
    implicit object PackM extends PM {
      override def fromGenericPack(genericPack: Prot) = TypePackPairs(TypeType(genericPack))
      override def intoGenericPack(self: TypePackPairs): Prot = self.inner.protocol
    }
    implicit object ConverterHelper extends Converter[TypeType, TypePackPairs] {
      override def from(a: TypeType): TypePackPairs = TypePackPairs(a)
      override def into(b: TypePackPairs): TypeType = b.inner
    }
  }
  case class TypePackPairs(inner: TypeType)
      extends ParamPackWrapperKinds[TypeKind.type, TypeKind.type, TypeType](inner)

  // Function arguments.
  sealed trait InputParams
  object ValueParams {
    implicit def m: ValueParamsPairs.PM = ValueParamsPairs.PackM
  }
  trait ValueParams extends InputParams {
    def asValueParams: ValueParamsPairs = implicitly[ValueParamsPairs.PM].zero()
  }
  object TypeParams {
    implicit def m: TypeParamsPairs.PM = TypeParamsPairs.PackM
  }
  trait TypeParams extends InputParams {
    def asTypeParams: TypeParamsPairs = implicitly[TypeParamsPairs.PM].zero()
  }

  // Function return values.
  sealed trait OutputPack
  object ValueOutputPack {
    implicit def m: ValuePackPairs.PM = ValuePackPairs.PackM
  }
  trait ValueOutputPack extends OutputPack {
    def asValuePack: ValuePackPairs = implicitly[ValuePackPairs.PM].zero()
  }
  object TypeOutputPack {
    implicit def m: TypePackPairs.PM = TypePackPairs.PackM
  }
  trait TypeOutputPack extends OutputPack {
    def asTypePack: TypePackPairs = implicitly[TypePackPairs.PM].zero()
  }

  sealed trait ExpressionComponent[
    Kind <: ExpressionKinds,
    ChainEl <: ChainElement[Kind],
  ] extends ChainElement[Kind] {
    def intoChainElement: ChainEl
    override def intoChain = intoChainElement.intoChain
  }
  trait ValueComponent extends ExpressionComponent[ValueKind.type, ValueChainElement]
      with TypeParams
      with ValueParams
      with ValueOutputPack {
    override def intoChainElement = ValueChainElement(
      typeParams = asTypeParams,
      valueParams = asValueParams,
      valuePack = asValuePack,
    )
  }
  trait TypeComponent extends ExpressionComponent[TypeKind.type, TypeChainElement]
      with TypeParams
      with TypeOutputPack {
    override def intoChainElement = TypeChainElement(
      typeParams = asTypeParams,
      typePack = asTypePack,
    )
  }
}

// Groups of objects.
object Packs {
  // import Errors._
  import EntityData._

  object PackKinds {
    def empty[
      K <: ExpressionKinds,
      V <: ExpressionKinds,
    ](implicit emptyPack: EmptyPack[K, V]) = emptyPack

    sealed abstract class ConvertNicely[
      K <: ExpressionKinds,
      V <: ExpressionKinds,
      PP <: PackPairs[K, V],
      P,
    ](init: PackKinds[K, V])
    sealed abstract class ConvertPacksNicely[
      K <: ExpressionKinds,
      V <: ExpressionKinds,
      PP <: PackPairs[K, V],
      P,
    ](init: PackKinds[K, V]) extends ConvertNicely[K, V, PP, P](init) {
      def convertPack(
        implicit f1: From[PackPairs.Protocol[K, V], PP], f2: From[PP, P],
      ): P = f2.from(f1.from(init.packProtocol))
    }
    sealed abstract class ConvertParamsNicely[
      K <: ExpressionKinds,
      V <: ExpressionKinds,
      PP <: PackPairs[K, V],
      P,
    ](init: PackKinds[K, V]) extends ConvertNicely[K, V, PP, P](init) {
      def convertParams(
        implicit f1: From[PackPairs.Protocol[K, V], PP], f2: From[PP, P],
      ): P = f2.from(f1.from(init.packProtocol))
    }
    implicit class VP1(init: PackKinds[ValueKind.type, ValueKind.type]) extends ConvertPacksNicely[
      ValueKind.type, ValueKind.type,
      ValueValue,
      ValuePackPairs,
    ](init)
    implicit class VP2(init: PackKinds[ValueKind.type, TypeKind.type]) extends ConvertParamsNicely[
      ValueKind.type,
      TypeKind.type,
      ValueType,
      ValueParamsPairs,
    ](init)
    implicit class TP1(init: PackKinds[TypeKind.type, TypeKind.type]) extends ConvertPacksNicely[
      TypeKind.type,
      TypeKind.type,
      TypeType,
      TypePackPairs,
    ](init)
    implicit class TP2(init: PackKinds[TypeKind.type, TypeKind.type]) extends ConvertParamsNicely[
      TypeKind.type,
      TypeKind.type,
      TypeType,
      TypeParamsPairs,
    ](init)
  }
  // Different types of pack naming schemes:
  object PackName {
    implicit val _pvpn: UnnamedPackName[ValueKind.type] = PositionalValuePackName
    implicit val _ptpn: UnnamedPackName[TypeKind.type] = PositionalTypePackName
    implicit def _namedValue(
      id: NamedIdentifier[ValueKind.type],
    ): NamedValuePackName = NamedValuePackName(id)
    implicit def _namedType(
      id: NamedIdentifier[TypeKind.type],
    ): NamedTypePackName = NamedTypePackName(id)
  }
  sealed abstract class PackName[Kind <: ExpressionKinds] {
    def maybeNamedId: Option[NamedIdentifier[Kind]]
  }
  sealed abstract class UnnamedPackName[Kind <: ExpressionKinds] extends PackName[Kind] {
    override def maybeNamedId = None
  }
  case object PositionalValuePackName extends UnnamedPackName[ValueKind.type]
  case object PositionalTypePackName extends UnnamedPackName[TypeKind.type]
  sealed abstract class NamedPackName[Kind <: ExpressionKinds](
    id: NamedIdentifier[Kind]
  ) extends PackName[Kind] {
    override def maybeNamedId = Some(id)
  }
  case class NamedValuePackName(id: NamedIdentifier[ValueKind.type])
      extends NamedPackName[ValueKind.type](id)
  case class NamedTypePackName(id: NamedIdentifier[TypeKind.type])
      extends NamedPackName[TypeKind.type](id)
  // PackKinds!
  sealed trait PackKinds[
    K <: ExpressionKinds,
    V <: ExpressionKinds,
  ] {
    type Prot = PackPairs.Protocol[K, V]
    def packProtocol: Prot
  }

  object EmptyPack {
    implicit val _evv = EmptyValueValuePack
    implicit val _evt = EmptyValueTypePack
    implicit val _ett = EmptyTypeTypePack
  }
  sealed abstract class EmptyPack[
    K <: ExpressionKinds,
    V <: ExpressionKinds,
  ] extends PackKinds[K, V] {
    override def packProtocol = Seq()
  }
  case object EmptyValueValuePack extends EmptyPack[ValueKind.type, ValueKind.type]
  case object EmptyValueTypePack extends EmptyPack[ValueKind.type, TypeKind.type]
  case object EmptyTypeTypePack extends EmptyPack[TypeKind.type, TypeKind.type]

  case class StandalonePack[
    K <: ExpressionKinds,
    V <: ExpressionKinds,
  ](k: K, expr: Entity[V])(implicit un: UnnamedPackName[K])
      extends PackKinds[K, V] {
    override def packProtocol: Prot = Seq(un -> expr)
  }

  case class PositionalPack[
    K <: ExpressionKinds,
    V <: ExpressionKinds,
  ](k: K, exprs: Seq[Entity[V]])(implicit un: UnnamedPackName[K])
      extends PackKinds[K, V] {
    override def packProtocol = exprs.map(un -> _)
  }

  case class NamedPack[
    K <: ExpressionKinds,
    V <: ExpressionKinds,
  ](fulfilled: Map[NamedIdentifier[K], Entity[V]])(
    implicit n: NamedIdentifier[K] => NamedPackName[K],
  ) extends PackKinds[K, V] {
    override def packProtocol = fulfilled.map {
      case (namedId, expr) => (n(namedId) -> expr)
    }.toSeq
  }

  trait IntoFinalNamedPack[
    K <: ExpressionKinds,
    V <: ExpressionKinds,
  ] {
    def intoFinalNamedPack(): FinalNamedPack[K, V]
  }

  // This class is used to allow coalescing arguments which may be provided by name OR by position,
  // e.g.:
  // $f(2, 3, .x(4), 5)
  object MergedPack {
    def empty[
      K <: ExpressionKinds,
      V <: ExpressionKinds,
    ](implicit kind: K): MergedPack[K, V] = MergedPack(Seq())(kind)
  }
  case class MergedPack[
    K <: ExpressionKinds,
    V <: ExpressionKinds,
  ](exprs: PackPairs.Protocol[K, V])(implicit kind: K)
      extends PackKinds[K, V]
      with IntoFinalNamedPack[K, V] {
    def size: Int = exprs.size
    def headIdent: PackName[K] = exprs.head._1

    override def packProtocol = exprs

    override def intoFinalNamedPack(): FinalNamedPack[K, V] = FinalNamedPack(
      mapping = exprs.zipWithIndex.map {
        case ((packName, expr), index) => {
          // If there's no explicit named parameter, fill out the hole with a positional param.
          val namedId = packName.maybeNamedId.getOrElse {
            // NB: We use 1-based indexing -- hence index + 1.
            // TODO: Test this!!!
            OneIndexedPosition[K](index + 1).intoIdentifier
          }
          (namedId -> expr)
        }
      }.toMap
    )
  }

  // This class represents an argument pack after it has subsumed any further arguments from the
  // left or right. Positional and standalone argument packs are converted to identifiers by
  // OneIndexedPosition.intoIdentifier()!
  case class FinalNamedPack[
    K <: ExpressionKinds,
    V <: ExpressionKinds,
  ](mapping: Map[NamedIdentifier[K], Entity[V]])
}

// Define AST entities.
object FunnelPEG {
  // import Errors._
  import EntityData._
  import Packs._

  sealed abstract class AstNode

  sealed abstract class Statement extends AstNode
  case class GlobalAssignmentForValue(
    place: GlobalValueVar,
    source: ValueComponent,
  ) extends Statement
  case class GlobalAssertionForValue(
    inner: AssertionLike[ValueKind.type],
  ) extends Statement
  case class GlobalAssignmentForType(
    place: GlobalTypeVar,
    source: TypeComponent,
  ) extends Statement
  case class GlobalAssertionForType(
    inner: AssertionLike[TypeKind.type],
  ) extends Statement

  sealed abstract class Expression[Kind <: ExpressionKinds] extends AstNode

  sealed abstract class PackedExpression extends Expression
  sealed abstract class PositionalPackedExpression extends PackedExpression
  sealed abstract class NamedPackedExpression extends PackedExpression
  sealed abstract class InlineNamedPackedExpression extends PackedExpression

  sealed abstract class CurryPackedExpression extends PackedExpression
  sealed abstract class CurryNamedPackedExpression extends CurryPackedExpression
  sealed abstract class CurryPositionalPackedExpression extends CurryPackedExpression
  sealed abstract class InlineCurryPackedExpression extends CurryPackedExpression
  sealed abstract class InlineCurryNamedPackedExpression extends InlineCurryPackedExpression
  sealed abstract class InlineCurryPositionalPackedExpression extends InlineCurryPackedExpression

  sealed abstract class CurryChain extends PackedExpression
  sealed abstract class IncompleteCurryChain extends CurryChain
  sealed abstract class CompletedCurryChain extends CurryChain

  sealed abstract class ParamPackExpression extends Expression
  sealed abstract class InlineParamPackExpression extends ParamPackExpression

  sealed abstract class NonPackedExpression extends Expression
  sealed abstract class Term extends NonPackedExpression
  sealed abstract class VariableReference extends Term
  sealed abstract class Literal extends Term
  sealed abstract class FunctionCallLike extends NonPackedExpression
  sealed abstract class CurriedCall extends FunctionCallLike

  sealed abstract class EnumDeclCase extends AstNode
  sealed abstract class EnumDecl extends NonPackedExpression
  sealed abstract class EnumDeref extends NonPackedExpression

  sealed abstract class Assertion extends NonPackedExpression
  sealed abstract class Grouping extends NonPackedExpression

  sealed abstract class PlaceholderNode extends AstNode
  import Packs.PackKinds._
  case object PlaceholderForValue extends PlaceholderNode with ValueComponent {
    override def asValuePack = StandalonePack(ValueKind, ValuePlaceholder).convertPack
  }
  case object PlaceholderForType extends PlaceholderNode with TypeComponent {
    override def asTypePack: TypePackPairs = StandalonePack(TypeKind, TypePlaceholder).convertPack
  }

  // Individual terms that don't imply a name pack (unlike local vars, which when dereferenced will
  // form an inline value/type pack).
  case class GlobalValueVar(g: GlobalVar[ValueKind.type]) extends VariableReference
      with ValueComponent {
    override def asValuePack = StandalonePack(ValueKind, g).convertPack
  }
  case class GlobalTypeVar(g: GlobalVar[TypeKind.type]) extends VariableReference
      with TypeComponent {
    override def asTypePack = StandalonePack(TypeKind, g).convertPack
  }

  // Local variable values (imply a named pack when used inline):
  case class LocalValueVar[L <: LocalIdentifierKinds[ValueKind.type]](
    l: LocalVar[ValueKind.type, L],
  ) extends VariableReference
      with ValueComponent
      with HasNamedIdentifier[ValueKind.type] {
    override def asValuePack = StandalonePack(ValueKind, l).convertPack
    override def namedIdentifier = l.namedIdentifier
  }
  case class LocalTypeVar[L <: LocalIdentifierKinds[TypeKind.type]](
    l: LocalVar[TypeKind.type, L],
  ) extends VariableReference
      with TypeComponent
      with HasNamedIdentifier[TypeKind.type] {
    override def asTypePack = StandalonePack(TypeKind, l).convertPack
    override def namedIdentifier = l.namedIdentifier
  }

  // Literals (right now, these are only values, not types).
  case class IntegerLiteral(numberValue: Int) extends Literal with ValueComponent {
    override def asValuePack = StandalonePack(ValueKind, IntLit(numberValue)).convertPack
  }
  case class FloatLiteral(numberValue: Double) extends Literal with ValueComponent {
    override def asValuePack = StandalonePack(ValueKind, FloatLit(numberValue)).convertPack
  }
  case class StringLiteral(inner: String) extends Literal with ValueComponent {
    override def asValuePack = StandalonePack(ValueKind, StrLit(inner)).convertPack
  }

  // Parameter declarations!
  object ValueParamDeclNoInline {
    def mergeInline(fields: Traversable[ValueParamDeclInline]) = ValueParamDeclNoInline(fields.map {
      case ValueParamDeclInline(name, constraint) => (name -> constraint)
    }.toMap)
  }
  case class ValueParamDeclNoInline(
    unfulfilled: Map[NamedIdentifier[ValueKind.type], TypeComponent],
  ) extends ParamPackExpression with ValueComponent {
    override def asValueParams = NamedPack(unfulfilled.mapValues(_.intoChain)).convertParams
  }
  case class ValueParamDeclInline(
    name: NamedIdentifier[ValueKind.type],
    constraint: TypeComponent,
  ) extends InlineParamPackExpression with ValueComponent {
    override def asValueParams = NamedPack(Map(name -> constraint.intoChain)).convertParams
  }
  object TypeParamDeclNoInline {
    def mergeInline(fields: Traversable[TypeParamDeclInline]) = TypeParamDeclNoInline(fields.map {
      case TypeParamDeclInline(name, constraint) => (name -> constraint)
    }.toMap)
  }
  case class TypeParamDeclNoInline(
    unfulfilled: Map[NamedIdentifier[TypeKind.type], TypeComponent],
  ) extends ParamPackExpression with TypeComponent {
    override def asTypeParams = NamedPack(unfulfilled.mapValues(_.intoChain)).convertParams
  }
  case class TypeParamDeclInline(
    name: NamedIdentifier[TypeKind.type],
    constraint: TypeComponent,
  ) extends InlineParamPackExpression with TypeComponent {
    override def asTypeParams = NamedPack(Map(name -> constraint.intoChain)).convertParams
  }

  // Curry chain expressions.
  import EntityData.MergeSubexpressions._
  case class IncompleteCurryValueChain(components: Seq[ValueComponent])
      extends IncompleteCurryChain with ValueComponent {
    override def asValuePack = components.map(_.asValuePack).mergeAlong()
  }
  case class CompletedCurryValueChain(components: Seq[ValueComponent])
      extends CompletedCurryChain with ValueComponent {
    override def asValuePack = components.map(_.asValuePack).mergeAlong()
  }
  case class IncompleteCurryTypeChain(components: Seq[TypeComponent])
      extends IncompleteCurryChain with TypeComponent {
    override def asTypePack = components.map(_.asTypePack).mergeAlong()
  }
  case class CompletedCurryTypeChain(components: Seq[TypeComponent])
      extends CompletedCurryChain with TypeComponent {
    override def asTypePack = components.map(_.asTypePack).mergeAlong()
  }

  // ALL the packing expressions!!!
  // First, values:
  case class PositionalValuePackExpression(exprs: Seq[ValueComponent])
      extends PositionalPackedExpression with ValueComponent {
    override def asValuePack = PositionalPack(ValueKind, exprs.map(_.intoChain)).convertPack
  }
  object NamedValuePackExpressionNoInline {
    def mergeInline(fields: Seq[InlineNamedValuePack]) = NamedValuePackExpressionNoInline(fields.map {
      case InlineNamedValuePack(name, value) => (name -> value)
    }.toMap)
  }
  case class NamedValuePackExpressionNoInline(
    fulfilled: Map[NamedIdentifier[ValueKind.type], ValueComponent],
  ) extends NamedPackedExpression with ValueComponent {
    override def asValuePack = NamedPack(fulfilled.mapValues(_.intoChain)).convertPack
  }
  case class InlineNamedValuePack(
    name: NamedIdentifier[ValueKind.type],
    value: ValueComponent,
  ) extends NamedPackedExpression with ValueComponent {
    override def asValuePack = NamedPack(Map(name -> value.intoChain)).convertPack
  }
  case class CurryNamedValuePackExpressionNoInline(
    fulfilled: Map[NamedIdentifier[ValueKind.type], ValueComponent],
  ) extends CurryNamedPackedExpression with ValueComponent {
    override def asValuePack = NamedPack(fulfilled.mapValues(_.intoChain)).convertPack
  }
  case class InlineCurryNamedValuePack(
    name: NamedIdentifier[ValueKind.type],
    value: ValueComponent,
  ) extends InlineCurryNamedPackedExpression with ValueComponent {
    override def asValuePack = NamedPack(Map(name -> value.intoChain)).convertPack
  }
  case class CurryPositionalValuePackExpressionNoInline(exprs: Seq[ValueComponent])
      extends CurryPositionalPackedExpression with ValueComponent {
    override def asValuePack = PositionalPack(ValueKind, exprs.map(_.intoChain)).convertPack
  }
  case class InlineCurryPositionalValuePack(expr: ValueComponent)
      extends InlineCurryPositionalPackedExpression with ValueComponent {
    override def asValuePack = PositionalPack(ValueKind, Seq(expr.intoChain)).convertPack
  }
  // Second, types:
  case class PositionalTypePackExpression(exprs: Seq[TypeComponent])
      extends PositionalPackedExpression with TypeComponent {
    override def asTypePack = PositionalPack(TypeKind, exprs.map(_.intoChain)).convertPack
  }
  object NamedTypePackExpressionNoInline {
    def mergeInline(fields: Seq[InlineNamedTypePack]) = NamedTypePackExpressionNoInline(fields.map {
      case InlineNamedTypePack(name, value) => (name -> value)
    }.toMap)
  }
  case class NamedTypePackExpressionNoInline(
    fulfilled: Map[NamedIdentifier[TypeKind.type], TypeComponent],
  ) extends NamedPackedExpression with TypeComponent {
    override def asTypePack = NamedPack(fulfilled.mapValues(_.intoChain)).convertPack
  }
  case class InlineNamedTypePack(
    name: NamedIdentifier[TypeKind.type],
    value: TypeComponent,
  ) extends NamedPackedExpression with TypeComponent {
    override def asTypePack = NamedPack(Map(name -> value.intoChain)).convertPack
  }
  case class CurryNamedTypePackExpressionNoInline(
    fulfilled: Map[NamedIdentifier[TypeKind.type], TypeComponent],
  ) extends CurryNamedPackedExpression with TypeComponent {
    override def asTypePack = NamedPack(fulfilled.mapValues(_.intoChain)).convertPack
  }
  case class InlineCurryNamedTypePack(
    name: NamedIdentifier[TypeKind.type],
    value: TypeComponent,
  ) extends InlineCurryNamedPackedExpression with TypeComponent {
    override def asTypePack = NamedPack(Map(name -> value.intoChain)).convertPack
  }
  case class CurryPositionalTypePackExpressionNoInline(exprs: Seq[TypeComponent])
      extends CurryPositionalPackedExpression with TypeComponent {
    override def asTypePack = PositionalPack(TypeKind, exprs.map(_.intoChain)).convertPack
  }
  case class InlineCurryPositionalTypePack(expr: TypeComponent)
      extends InlineCurryPositionalPackedExpression with TypeComponent {
    override def asTypePack = PositionalPack(TypeKind, Seq(expr.intoChain)).convertPack
  }

  // Function call-like things.
  // First, (Value, Value):
  // NB: Curried calls are included by the "no inline" call!
  case class FunctionCallValueValueNoInline(
    source: ValueComponent,
    arguments: ValuePackPairs,
  ) extends FunctionCallLike with ValueComponent {
    override def asValuePack =
      StandalonePack(ValueKind, ValueValueFunctionCall(source.intoChain, arguments)).convertPack
  }
  case class InlineFunctionCallValueValue(
    source: ValueComponent,
    arguments: ValuePackPairs,
  ) extends FunctionCallLike with ValueComponent {
  override def asValuePack =
    StandalonePack(ValueKind, ValueValueFunctionCall(source.intoChain, arguments)).convertPack
  }
  // Second, (Value, Type):
  // NB: Curried calls are included in the "no inline" call!
  case class FunctionCallValueTypeNoInline(
    source: ValueComponent,
    arguments: TypePackPairs,
  ) extends FunctionCallLike with ValueComponent {
    override def asValuePack =
      StandalonePack(ValueKind, ValueTypeFunctionCall(source.intoChain, arguments)).convertPack
  }
  case class InlineFunctionCallValueType(
    source: ValueComponent,
    arguments: TypePackPairs,
  ) extends FunctionCallLike with ValueComponent {
    override def asValuePack =
      StandalonePack(ValueKind, ValueTypeFunctionCall(source.intoChain, arguments)).convertPack
  }
  // Third, (Type, Type):
  // NB: Curried calls are included in the "no inline" call!
  case class FunctionCallTypeTypeNoInline(
    source: TypeComponent,
    arguments: TypePackPairs
  ) extends FunctionCallLike with TypeComponent {
    override def asTypePack =
      StandalonePack(TypeKind, TypeTypeFunctionCall(source.intoChain, arguments)).convertPack
  }
  case class InlineFunctionCallTypeType(
    source: TypeComponent,
    arguments: TypePackPairs
  ) extends FunctionCallLike with TypeComponent {
    override def asTypePack =
      StandalonePack(TypeKind, TypeTypeFunctionCall(source.intoChain, arguments)).convertPack
  }

  // Enum declarations:
  // First, cases:
  case class ValueEnumDeclCase(
    name: AlternationCaseName[ValueKind.type],
    defn: ValueParamsPairs,
  ) extends EnumDeclCase
  case class TypeEnumDeclCase(
    name: AlternationCaseName[TypeKind.type],
    defn: TypeParamsPairs,
  ) extends EnumDeclCase
  // Second, whole enum decls:
  case class ValueEnumDecl(cases: Seq[ValueEnumDeclCase])
      extends EnumDecl with ValueComponent {
    override def asValuePack = StandalonePack(ValueKind, EnumValueDecl(
      cases.map { case ValueEnumDeclCase(name, defn) => EnumValueCaseDecl(name, defn) }
    )).convertPack
  }
  case class TypeEnumDecl(cases: Seq[TypeEnumDeclCase])
      extends EnumDecl with TypeComponent {
    override def asTypePack = StandalonePack(TypeKind, EnumTypeDecl(
      cases.map { case TypeEnumDeclCase(name, defn) => EnumTypeCaseDecl(name, defn) }
    )).convertPack
  }
  // Third, enum case dereferences:
  case class ValueEnumDeref(
    name: AlternationCaseName[ValueKind.type],
    arguments: ValuePackPairs,
  ) extends EnumDeref with ValueComponent {
    override def asValuePack =
      StandalonePack(ValueKind, EnumValueCaseDereference(name, arguments)).convertPack
  }
  case class TypeEnumDeref(
    name: AlternationCaseName[TypeKind.type],
    arguments: TypePackPairs,
  ) extends EnumDeref with TypeComponent {
    override def asTypePack =
      StandalonePack(TypeKind, EnumTypeCaseDereference(name, arguments)).convertPack
  }

  // Assertions:
  sealed abstract class AssertionNode[Kind <: ExpressionKinds] extends Assertion {
    def intoAtom: AssertionLike[Kind]
  }
  sealed abstract class AssertionForSomeValue
      extends AssertionNode[ValueKind.type] with ValueComponent {
    override def asValuePack = StandalonePack(ValueKind, intoAtom).convertPack
  }
  sealed abstract class AssertionForSomeType
      extends AssertionNode[TypeKind.type] with TypeComponent {
    override def asTypePack = StandalonePack(TypeKind, intoAtom).convertPack
  }
  case class AssertionForValueValue(
    dest: ValueComponent,
    source: ValueComponent,
  ) extends AssertionForSomeValue {
    override def intoAtom = ValueValueAssertion(dest.intoChain, source.intoChain)
  }
  case class AssertionForValueType(
    dest: ValueComponent,
    source: TypeComponent,
  ) extends AssertionForSomeValue {
    override def intoAtom = ValueTypeAssertion(dest.intoChain, source.intoChain)
  }
  case class AssertionForTypeType(
    dest: TypeComponent,
    source: TypeComponent,
  ) extends AssertionForSomeType {
    override def intoAtom = TypeTypeAssertion(dest.intoChain, source.intoChain)
  }

  // Groupings:
  case class GroupingForValue(inner: ValueComponent) extends Grouping with ValueComponent {
    override def asValuePack = StandalonePack(ValueKind, ValueGrouping(inner.intoChain)).convertPack
  }
  case class GroupingForType(inner: TypeComponent) extends Grouping with TypeComponent {
    override def asTypePack = StandalonePack(TypeKind, TypeGrouping(inner.intoChain)).convertPack
  }
}

class FunnelPEG(override val input: ParserInput) extends Parser {
  import EntityData._
  // import Packs._
  import FunnelPEG._

  // Define the parsing rules.
  def Funnel: Rule1[Seq[Statement]] = rule { WhiteSpace ~ ReplOrFile ~ WhiteSpace ~ EOI }

  def TopLevel: Rule1[Statement] = rule {
    ParseGlobalAssignmentForValue |
    ParseGlobalAssertionForValue |
    ParseGlobalAssignmentForType |
    ParseGlobalAssertionForType
  }

  def ReplOrFile: Rule1[Seq[Statement]] = rule {
    TopLevel.*(NewLine) ~> ((s: Seq[Statement]) => s)
  }

  // Parse global value statements:
  // First, assignment:
  def ParseGlobalAssignmentForValue: Rule1[GlobalAssignmentForValue] = rule {
    ParseGlobalValueVar ~ ParseAllValueExpressions ~> (
      (gv: GlobalValueVar, ve: ValueComponent) =>
      GlobalAssignmentForValue(gv, ve))
  }
  // def ParseGlobalAssignmentForValue: Rule1[GlobalAssignmentForValue] = rule {
  //   (ParseGlobalValueVar ~ ParseTypeParamsCreateNoInline.? ~ ParseStructDeclNoInline.? ~ WhiteSpace ~ "<=" ~ ParseValueExpression) ~> (
  //     (v: GlobalValueVar, tpc: Option[NamedTypeParamPack], slv: Option[NamedValueParamPack],
  //       ve: ValueExpression) => {
  //       val valExpr = (tpc, slv) match {
  //         case (Some(tpc), Some(slv)) => AnonymousMethod(
  //           output = ve,
  //           functionParams = ParamsDeclaration(NamedPack(slv.unfulfilled)),
  //           typeFunctionParams = ParamsDeclaration(NamedPack(tpc.unfulfilled))).pullOutAnyNewTypeParams
  //         case (Some(tpc), None) => TypeParamsWrapperForValue(
  //           subject = ve,
  //           tpp = NamedTypeParamPack(tpc.unfulfilled),
  //           fromLocation = Left,
  //         )
  //         case (None, Some(slv)) => AnonymousMethod(
  //           output = ve,
  //           functionParams = ParamsDeclaration(NamedPack(slv.unfulfilled))).pullOutAnyNewTypeParams
  //         case (None, None) => ve
  //       }
  //       GlobalAssignmentForValue(v, valExpr)
  //     }
  //   )
  // }
  // Second, assertion:
  def ParseGlobalAssertionForValue: Rule1[GlobalAssertionForValue] = rule {
    ParseAllValueAssertions ~> ((afsv: AssertionForSomeValue) =>
      GlobalAssertionForValue(afsv.intoAtom))
  }

  // Parse global type statements:
  // First, assignment:
  def ParseGlobalAssignmentForType: Rule1[GlobalAssignmentForType] = rule {
    (ParseGlobalTypeVar ~ "<-" ~ ParseAllTypeExpressions) ~> (
      (t: GlobalTypeVar, te: TypeComponent) =>
      GlobalAssignmentForType(t, te)
    )
  }
  // Second, assertion:
  def ParseGlobalAssertionForType: Rule1[GlobalAssertionForType] = rule {
    ParseAllTypeAssertions ~> ((afst: AssertionForSomeType) =>
    GlobalAssertionForType(afst.intoAtom))
  }

  // Parse *all* value/type expressions.
  // NB: The ordering matters because parboiled2 is a PEG parser!!!
  def ParseAllValueExpressions: Rule1[ValueComponent] = rule {
    ParseAllValueParamDecls |
    ParseAllValuePacks |
    ParseValueEnumDecl |
    ParseValueEnumDeref |
    ParseAllValueValueFunctionCalls |
    ParseAllValueTypeFunctionCalls |
    ParseGroupingForValue |
    ParseGlobalValueVar |
    ParseAllLiterals |
    ParseAllValueAssertions
  }
  def ParseAllTypeExpressions: Rule1[TypeComponent] = rule {
    ParseAllTypeParamDecls |
    ParseAllTypePacks |
    ParseTypeEnumDecl |
    ParseTypeEnumDeref |
    ParseAllTypeTypeFunctionCalls |
    ParseGroupingForType |
    ParseGlobalTypeVar |
    ParseAllTypeAssertions
  }

  // Parse identifiers:
  def ParseRestOfIdentifier: Rule0 = rule {
    zeroOrMore(CharPredicate.AlphaNum | anyOf("-_")) ~
    WhiteSpace
  }
  def ParseValueIdentifier: Rule1[NamedIdentifier[ValueKind.type]] = rule {
    capture((CharPredicate.LowerAlpha | anyOf("-")) ~ ParseRestOfIdentifier) ~> (
      (s: String) => NamedIdentifier[ValueKind.type](s))
  }
  def ParseTypeIdentifier: Rule1[NamedIdentifier[TypeKind.type]] = rule {
    capture((CharPredicate.UpperAlpha | anyOf("_")) ~ ParseRestOfIdentifier) ~> (
      (s: String) => NamedIdentifier[TypeKind.type](s))
  }
  def ParseGlobalValueVar: Rule1[GlobalValueVar] = rule {
    ("$" ~ ParseValueIdentifier) ~> ((namedIdentifier: NamedIdentifier[ValueKind.type]) => {
      GlobalValueVar(GlobalVar(namedIdentifier))
    })
  }
  def ParseGlobalTypeVar: Rule1[GlobalTypeVar] = rule {
    "$" ~ ParseTypeIdentifier ~> ((namedIdentifier: NamedIdentifier[TypeKind.type]) =>
      GlobalTypeVar(GlobalVar(namedIdentifier))
    )
  }
  def ParseNamedLocalValueVar: Rule1[LocalValueVar[LocalNamedIdentifier[ValueKind.type]]] = rule {
    "." ~ ParseValueIdentifier ~> ((namedIdentifier: NamedIdentifier[ValueKind.type]) =>
      LocalValueVar(LocalVar(LocalNamedIdentifier(namedIdentifier))))
  }
  def ParseNamedLocalTypeVar: Rule1[LocalTypeVar[LocalNamedIdentifier[TypeKind.type]]] = rule {
    "." ~ ParseTypeIdentifier ~> ((namedIdentifier: NamedIdentifier[TypeKind.type]) =>
      LocalTypeVar(LocalVar(LocalNamedIdentifier(namedIdentifier))))
  }
  def ParseValueAlternationCaseName: Rule1[AlternationCaseName[ValueKind.type]] = rule {
    "+" ~ ParseValueIdentifier ~> ((namedIdentifier: NamedIdentifier[ValueKind.type]) =>
      AlternationCaseName(namedIdentifier))
  }
  def ParseTypeAlternationCaseName: Rule1[AlternationCaseName[TypeKind.type]] = rule {
    "+" ~ ParseTypeIdentifier ~> ((namedIdentifier: NamedIdentifier[TypeKind.type]) =>
      AlternationCaseName(namedIdentifier))
  }

  // Parse literals (right now, these are only values, not types):
  def ParseIntegerLiteral: Rule1[IntegerLiteral] = rule {
    capture(oneOrMore(CharPredicate.Digit)) ~> ((s: String) => IntegerLiteral(s.toInt))
  }
  def ParseStringLiteral: Rule1[StringLiteral] = rule {
    ("\"" ~ capture(oneOrMore(noneOf("\"\\"))) ~ "\"") ~> ((s: String) => StringLiteral(s))
  }
  def ParseFloatLiteral: Rule1[FloatLiteral] = rule {
    ((capture(oneOrMore(CharPredicate.Digit)) ~ str(".") ~ capture(oneOrMore(CharPredicate.Digit))) ~> (
      (lhs: String, rhs: String) => FloatLiteral(s"$lhs.$rhs".toDouble)
    )
      | ((str(".") ~ capture(oneOrMore(CharPredicate.Digit))) ~> (
        (rhs: String) => FloatLiteral(s"0.$rhs".toDouble)
      )))
  }
  def ParseAllLiterals: Rule1[ValueComponent] = rule {
    ParseFloatLiteral | ParseIntegerLiteral | ParseStringLiteral
  }

  // Parse groupings:
  def ParseGroupingForValue: Rule1[GroupingForValue] = rule {
    "(" ~ ParseAllValueExpressions ~ ")" ~> ((ve: ValueComponent) => GroupingForValue(ve))
  }
  def ParseGroupingForType: Rule1[GroupingForType] = rule {
    "[" ~ ParseAllTypeExpressions ~ "]" ~> ((te: TypeComponent) => GroupingForType(te))
  }

  // These two rules are crucial. They define "atomic" splits of chains of value/type expressions
  // which don't demonstrate "positional ambiguity", aka the possibility that "$f <= 3" is parsed as
  // just "$f". I believe is an extremely unfortunate limitation of the PEG parsing algorithm.
  def ParseAllValueExpressionsWithoutPositionalAmbiguity: Rule1[ValueComponent] = rule {
    // Param declarations begin with a backslash -- this is unique.
    ParseAllValueParamDecls |
    // The below line was changed from ParseAllValuePacks, from ParseAllValueExpressions.
    ParseAllNonCurriedValuePacks |
    // Enum case declarations begin with a backslash, then a +, which is unique.
    ParseValueEnumDecl |
    // Enum derefs are the only thing to start with a + FOR NOW! until destructuring is implemented.
    ParseValueEnumDeref |
    // Function calls must use only inline forms.
    ParseInlineFunctionCallValueValue |
    ParseInlineFunctionCallValueType |
    // There is nothing below the grouping here that uses parens or brackets, so this is finally
    // unambiguous.
    ParseGroupingForValue |
    // Global values begin with a $ -- this is unique.
    ParseGlobalValueVar |
    ParseAllLiterals
    // Assertions require grouping to avoid ambiguity, so they are removed.
  }
  def ParseAllTypeExpressionsWithoutPositionalAmbiguity: Rule1[TypeComponent] = rule {
    // Param declarations begin with a backslash -- this is unique.
    ParseAllTypeParamDecls |
    // The below line was changed from ParseAllTypePacks, from ParseAllExpressions.
    ParseAllNonCurriedTypePacks |
    // Enum case declarations begin with a backslash, then a +, which is unique.
    ParseTypeEnumDecl |
    // Enum derefs are the only thing to start with a + FOR NOW! until destructuring is implemented.
    ParseTypeEnumDeref |
    // Function calls must use only inline forms.
    ParseInlineFunctionCallTypeType |
    // There is nothing below the grouping here that uses parens or brackets, so this is finally
    // unambiguous.
    ParseGroupingForType |
    // Global values begin with a $ -- this is unique.
    ParseGlobalTypeVar
    // Assertions require grouping to avoid ambiguity, so they are removed.
  }

  // Parse parameter declarations:
  // First, value:
  def ParseValueParamDeclNoInline: Rule1[ValueParamDeclNoInline] = rule {
    "(" ~ CommaSeparatedPack(() => ParseValueParamDeclInline) ~ ")" ~> (
      (fields: Seq[ValueParamDeclInline]) => ValueParamDeclNoInline.mergeInline(fields)
    )
  }
  def _ParseValueParamDeclInlineReduced: Rule1[ValueParamDeclInline] = rule {
    ParseNamedLocalValueVar ~ ParseGroupingForType.? ~> (
      (lv: LocalValueVar[LocalNamedIdentifier[ValueKind.type]], constraint: Option[GroupingForType]) =>
      ValueParamDeclInline(lv.namedIdentifier, constraint.getOrElse(PlaceholderForType))
    )
  }
  def ParseValueParamDeclInline: Rule1[ValueParamDeclInline] = rule {
    "\\" ~ _ParseValueParamDeclInlineReduced
  }
  def ParseAllValueParamDecls: Rule1[ValueComponent] = rule {
    ParseValueParamDeclNoInline |
    ParseValueParamDeclInline
  }
  // Second, type:
  def ParseTypeParamDeclNoInline: Rule1[TypeParamDeclNoInline] = rule {
    "[" ~ CommaSeparatedPack(() => ParseTypeParamDeclInline) ~ "]" ~> (
      (fields: Seq[TypeParamDeclInline]) => TypeParamDeclNoInline.mergeInline(fields)
    )
  }
  def _ParseTypeParamDeclInlineReduced: Rule1[TypeParamDeclInline] = rule {
    ParseNamedLocalTypeVar ~ ParseGroupingForType.? ~> (
      (lv: LocalTypeVar[LocalNamedIdentifier[TypeKind.type]], constraint: Option[GroupingForType]) =>
      TypeParamDeclInline(lv.namedIdentifier, constraint.getOrElse(PlaceholderForType))
    )
  }
  def ParseTypeParamDeclInline: Rule1[TypeParamDeclInline] = rule {
    "\\" ~ _ParseTypeParamDeclInlineReduced
  }
  def ParseAllTypeParamDecls: Rule1[TypeComponent] = rule {
    ParseTypeParamDeclNoInline |
    ParseTypeParamDeclInline
  }

  // Parse curry chains.
  // First, values:
  def ParseIncompleteCurryValueChain: Rule1[IncompleteCurryValueChain] = rule {
    (ParseAllCurriedValuePacks ~ "=>" ~ ParseAllCurriedValuePacks ~> (
      (lhs: ValueComponent, rhs: ValueComponent) => IncompleteCurryValueChain(Seq(lhs, rhs)))
      | ParseAllCurriedValuePacks ~ "<=" ~ ParseAllCurriedValuePacks ~> (
        (rhs: ValueComponent, lhs: ValueComponent) => IncompleteCurryValueChain(Seq(lhs, rhs)))
    )
  }
  def ParseCompletedCurryValueChain: Rule1[CompletedCurryValueChain] = rule {
    ((ParseIncompleteCurryValueChain ~ "=>" ~ ParseAllNonCurriedValuePacks ~> (
      (lhs: IncompleteCurryValueChain, rhs: ValueComponent) =>
      CompletedCurryValueChain(lhs.components :+ rhs))
      | ParseAllNonCurriedValuePacks ~ "<=" ~ ParseIncompleteCurryValueChain ~> (
          (rhs: ValueComponent, lhs: IncompleteCurryValueChain) =>
          CompletedCurryValueChain(lhs.components :+ rhs)
        )
      | ParseAllCurriedValuePacks ~> (
        (single: ValueComponent) => CompletedCurryValueChain(Seq(single)))))
  }
  def ParseAllCurryValueChains: Rule1[ValueComponent] = rule {
    ParseIncompleteCurryValueChain |
    ParseCompletedCurryValueChain
  }
  // Second, types:
  def ParseIncompleteCurryTypeChain: Rule1[IncompleteCurryTypeChain] = rule {
    (ParseAllCurriedTypePacks ~ "->" ~ ParseAllCurriedTypePacks ~> (
      (lhs: TypeComponent, rhs: TypeComponent) => IncompleteCurryTypeChain(Seq(lhs, rhs)))
      | ParseAllCurriedTypePacks ~ "<-" ~ ParseAllCurriedTypePacks ~> (
        (rhs: TypeComponent, lhs: TypeComponent) => IncompleteCurryTypeChain(Seq(lhs, rhs)))
    )
  }
  def ParseCompletedCurryTypeChain: Rule1[CompletedCurryTypeChain] = rule {
    ((ParseIncompleteCurryTypeChain ~ "->" ~ ParseAllNonCurriedTypePacks ~> (
      (lhs: IncompleteCurryTypeChain, rhs: TypeComponent) =>
      CompletedCurryTypeChain(lhs.components :+ rhs))
      | ParseAllNonCurriedTypePacks ~ "<-" ~ ParseIncompleteCurryTypeChain ~> (
          (rhs: TypeComponent, lhs: IncompleteCurryTypeChain) =>
          CompletedCurryTypeChain(lhs.components :+ rhs)
      )
      | ParseAllCurriedTypePacks ~> (
        (single: TypeComponent) => CompletedCurryTypeChain(Seq(single)))))
  }
  def ParseAllCurryTypeChains: Rule1[TypeComponent] = rule {
    ParseIncompleteCurryTypeChain |
    ParseCompletedCurryTypeChain
  }

  // Parse ALL the packing expressions!!!!
  // First, values:
  def ParsePositionalValuePackExpression: Rule1[PositionalValuePackExpression] = rule {
    "(" ~ CommaSeparatedPack(() => ParseAllValueExpressions) ~ ")" ~> (
      (exprs: Seq[ValueComponent]) => PositionalValuePackExpression(exprs)
    )
  }
  def ParseNamedValuePackExpressionNoInline: Rule1[NamedValuePackExpressionNoInline] = rule {
    "(" ~ CommaSeparatedPack(() => ParseInlineNamedValuePack) ~ ")" ~> (
      (exprs: Seq[InlineNamedValuePack]) => NamedValuePackExpressionNoInline.mergeInline(exprs)
    )
  }
  def ParseInlineNamedValuePack: Rule1[InlineNamedValuePack] = rule {
    ParseNamedLocalValueVar ~ ParseGroupingForValue.? ~> (
      (lv: LocalValueVar[LocalNamedIdentifier[ValueKind.type]], value: Option[GroupingForValue]) =>
      InlineNamedValuePack(lv.namedIdentifier, value.getOrElse(lv))
    )
  }
  def ParseCurryNamedValuePackExpressionNoInline: Rule1[CurryNamedValuePackExpressionNoInline] = rule {
    ("(" ~
      ParseInlineNamedValuePack.*(SpacedComma) ~
      ParseInlineCurryNamedValuePack ~
      ")") ~> (
      (firstFew: Seq[InlineNamedValuePack], last: InlineCurryNamedValuePack) => {
        val firstMerged = NamedValuePackExpressionNoInline.mergeInline(firstFew)
        CurryNamedValuePackExpressionNoInline(firstMerged.fulfilled + (last.name -> last.value))
      }
    )
  }
  def ParseInlineCurryNamedValuePack: Rule1[InlineCurryNamedValuePack] = rule {
    ParseNamedLocalValueVar ~ ParseGroupingForValue.? ~ "," ~ "..." ~> (
      (lv: LocalValueVar[LocalNamedIdentifier[ValueKind.type]], value: Option[GroupingForValue]) =>
      InlineCurryNamedValuePack(lv.namedIdentifier, value.getOrElse(lv))
    )
  }
  def ParseCurryPositionalValuePackExpressionNoInline: Rule1[CurryPositionalValuePackExpressionNoInline] = rule {
    ("(" ~
      ParseAllValueExpressionsWithoutPositionalAmbiguity.*(SpacedComma) ~
      ParseInlineCurryPositionalValuePack ~
      ")") ~> (
      (firstFew: Seq[ValueComponent], last: InlineCurryPositionalValuePack) =>
      CurryPositionalValuePackExpressionNoInline(firstFew :+ last.expr)
    )
  }
  def ParseInlineCurryPositionalValuePack: Rule1[InlineCurryPositionalValuePack] = rule {
    ParseAllValueExpressionsWithoutPositionalAmbiguity ~ "," ~ "..." ~> (
      (expr: ValueComponent) => InlineCurryPositionalValuePack(expr)
    )
  }
  // NB: These are subsumed into ParseIncompleteCurryValueChain, and should NOT be part of
  // ParseAllValuePacks!!!
  def ParseAllCurriedValuePacks: Rule1[ValueComponent] = rule {
    ParseCurryNamedValuePackExpressionNoInline |
    ParseInlineCurryNamedValuePack |
    ParseCurryPositionalValuePackExpressionNoInline |
    ParseInlineCurryPositionalValuePack
  }
  // These are all packs which *definitely* have parentheses around them.
  def ParseAllNonCurriedNonInlineValuePacks: Rule1[ValueComponent] = rule {
    ParsePositionalValuePackExpression |
    ParseNamedValuePackExpressionNoInline
  }
  def ParseAllNonCurriedValuePacks: Rule1[ValueComponent] = rule {
    ParseAllNonCurriedNonInlineValuePacks |
    ParseInlineNamedValuePack
  }
  def ParseAllValuePacks: Rule1[ValueComponent] = rule {
    ParseAllNonCurriedValuePacks |
    ParseAllCurryValueChains
  }
  // Second, types:
  def ParsePositionalTypePackExpression: Rule1[PositionalTypePackExpression] = rule {
    "[" ~ CommaSeparatedPack(() => ParseAllTypeExpressions) ~ "]" ~> (
      (exprs: Seq[TypeComponent]) => PositionalTypePackExpression(exprs)
    )
  }
  def ParseNamedTypePackExpressionNoInline: Rule1[NamedTypePackExpressionNoInline] = rule {
    "[" ~ CommaSeparatedPack(() => ParseInlineNamedTypePack) ~ "]" ~> (
      (exprs: Seq[InlineNamedTypePack]) => NamedTypePackExpressionNoInline.mergeInline(exprs)
    )
  }
  def ParseInlineNamedTypePack: Rule1[InlineNamedTypePack] = rule {
    ParseNamedLocalTypeVar ~ ParseGroupingForType.? ~> (
      (lv: LocalTypeVar[LocalNamedIdentifier[TypeKind.type]], value: Option[GroupingForType]) =>
      InlineNamedTypePack(lv.namedIdentifier, value.getOrElse(lv))
    )
  }
  def ParseCurryNamedTypePackExpressionNoInline: Rule1[CurryNamedTypePackExpressionNoInline] = rule {
    ("[" ~
      ParseInlineNamedTypePack.*(SpacedComma) ~
      ParseInlineCurryNamedTypePack ~
      "]") ~> (
      (firstFew: Seq[InlineNamedTypePack], last: InlineCurryNamedTypePack) => {
        // Param declarations begin with a backslash -- this is unambiguous.
        val firstMerged = NamedTypePackExpressionNoInline.mergeInline(firstFew)
        // Param declarations begin with a backslash -- this is unambiguous.
        CurryNamedTypePackExpressionNoInline(firstMerged.fulfilled + (last.name -> last.value))
      //Enum declarations
      }
    )
  }
  def ParseInlineCurryNamedTypePack: Rule1[InlineCurryNamedTypePack] = rule {
    ParseNamedLocalTypeVar ~ ParseGroupingForType.? ~ "," ~ "..." ~> (
      (lv: LocalTypeVar[LocalNamedIdentifier[TypeKind.type]], value: Option[GroupingForType]) =>
      // Param declarations begin with a backslash -- this is unambiguous.
      InlineCurryNamedTypePack(lv.namedIdentifier, value.getOrElse(lv))
        // Param declarations begin with a backslash -- this is unambiguous.
    )
    //Enum declarations
  }
  def ParseCurryPositionalTypePackExpressionNoInline: Rule1[CurryPositionalTypePackExpressionNoInline] = rule {
    ("[" ~
      ParseAllTypeExpressionsWithoutPositionalAmbiguity.*(SpacedComma) ~
      ParseInlineCurryPositionalTypePack ~
      "]") ~> (
      (firstFew: Seq[TypeComponent], last: InlineCurryPositionalTypePack) =>
      CurryPositionalTypePackExpressionNoInline(firstFew :+ last.expr)
    )
  }
  def ParseInlineCurryPositionalTypePack: Rule1[InlineCurryPositionalTypePack] = rule {
    ParseAllTypeExpressionsWithoutPositionalAmbiguity ~ "," ~ "..." ~> (
      (expr: TypeComponent) => InlineCurryPositionalTypePack(expr)
    )
  }
  // NB: These are subsumed into ParseIncompleteCurryTypeChain, and should NOT be part of
  // ParseAllTypePacks!!!
  def ParseAllCurriedTypePacks: Rule1[TypeComponent] = rule {
    ParseCurryNamedTypePackExpressionNoInline |
    ParseInlineCurryNamedTypePack |
    ParseCurryPositionalTypePackExpressionNoInline |
    ParseInlineCurryPositionalTypePack
  }
  // These are all packs which *definitely* have parentheses around them.
  def ParseAllNonCurriedNonInlineTypePacks: Rule1[TypeComponent] = rule {
    ParsePositionalTypePackExpression |
    ParseNamedTypePackExpressionNoInline
  }
  def ParseAllNonCurriedTypePacks: Rule1[TypeComponent] = rule {
    ParseAllNonCurriedNonInlineTypePacks |
    ParseInlineNamedTypePack
  }
  def ParseAllTypePacks: Rule1[TypeComponent] = rule {
    ParseAllNonCurriedTypePacks |
    ParseAllCurryTypeChains
  }
  // def ParseAllTypeNonPacks: Rule1[TypeComponent] = rule {
  //   Parse
  // }

  // Function call-like things.
  // NB: We special-case function sources when called inline, e.g.:
  // $f(1, 2, 3)
  // as we can expect at parse time that any pack expressions
  // are not likely to be callable.
  // Note that non-inline function calls, e.g.:
  // $f <= (1, 2, 3)
  // may use any value/type expression as the source.
  def _ParseInlineFunctionSourceValueExpression: Rule1[ValueComponent] = rule {
    ParseGroupingForValue |
    ParseGlobalValueVar |
    ParseAllValueParamDecls
  }
  def _ParseInlineFunctionSourceTypeExpression: Rule1[TypeComponent] = rule {
    ParseGroupingForType |
    ParseGlobalTypeVar |
    ParseAllTypeParamDecls
  }
  // For an inline function call, we require that parentheses are used to denote the arguments pack,
  // and we don't allow curried packs, since they would never mean anything.
  def _ParseInlineFunctionArgumentsValueExpression: Rule1[ValueComponent] = rule {
    ParsePositionalValuePackExpression |
    ParseNamedValuePackExpressionNoInline
  }
  def _ParseInlineFunctionArgumentsTypeExpression: Rule1[TypeComponent] = rule {
    ParsePositionalTypePackExpression |
    ParseNamedTypePackExpressionNoInline
  }
  // First, (Value, Value):
  // NB: Curried calls are included in the "no inline" call!
  def ParseFunctionCallValueValueNoInline: Rule1[FunctionCallValueValueNoInline] = rule {
    ((ParseAllValueExpressionsWithoutPositionalAmbiguity ~ "<=" ~ ParseAllValuePacks ~> (
      (source: ValueComponent, arguments: ValueComponent) => FunctionCallValueValueNoInline(
        source = source,
        arguments = arguments.asValuePack,
      )))
      | (ParseAllValuePacks ~ "=>" ~ ParseAllValueExpressionsWithoutPositionalAmbiguity ~> (
        (arguments: ValueComponent, source: ValueComponent) => FunctionCallValueValueNoInline(
          source = source,
          arguments = arguments.asValuePack,
        )
      )))
  }
  // NB: While non-inline function calls can flow from right-to-left or left-to-right, inline
  // function calls only go left-to-right.
  def ParseInlineFunctionCallValueValue: Rule1[InlineFunctionCallValueValue] = rule {
    (_ParseInlineFunctionSourceValueExpression ~ _ParseInlineFunctionArgumentsValueExpression) ~> (
      (source: ValueComponent, arguments: ValueComponent) => InlineFunctionCallValueValue(
        source = source,
        arguments = arguments.asValuePack,
      ))
  }
  def ParseAllValueValueFunctionCalls: Rule1[ValueComponent] = rule {
    ParseFunctionCallValueValueNoInline |
    ParseInlineFunctionCallValueValue
  }
  // Second, (Value, Type):
  // NB: Curried calls are included in the "no inline" call!
  def ParseFunctionCallValueTypeNoInline: Rule1[FunctionCallValueTypeNoInline] = rule {
    ((ParseAllValueExpressionsWithoutPositionalAmbiguity ~ "<-" ~ ParseAllTypePacks ~> (
      (source: ValueComponent, arguments: TypeComponent) => FunctionCallValueTypeNoInline(
        source = source,
        arguments = arguments.asTypePack,
      )
    ))
      | (ParseAllTypePacks ~ "->" ~ ParseAllValueExpressionsWithoutPositionalAmbiguity ~> (
        (arguments: TypeComponent, source: ValueComponent) => FunctionCallValueTypeNoInline(
          source = source,
          arguments = arguments.asTypePack,
        )
    )))
  }
  // NB: While non-inline function calls can flow from right-to-left or left-to-right, inline
  // function calls only go left-to-right.
  def ParseInlineFunctionCallValueType: Rule1[InlineFunctionCallValueType] = rule {
    // Param declarations begin with a backslash -- this is unambiguous.
    (_ParseInlineFunctionSourceValueExpression ~ _ParseInlineFunctionArgumentsTypeExpression) ~> (
      // Param declarations begin with a backslash -- this is unambiguous.

      (source: ValueComponent, arguments: TypeComponent) => InlineFunctionCallValueType( //Enum
                                                                                         //declarations
        source = source,
        arguments = arguments.asTypePack,
      ))
    // Param declarations begin with a backslash -- this is unambiguous.
  }
  // Param declarations begin with a backslash -- this is unambiguous.
  def
    ParseAllValueTypeFunctionCalls: Rule1[ValueComponent] = rule { //Enum declarations
    ParseFunctionCallValueTypeNoInline |
    ParseInlineFunctionCallValueType
  }
  // Third, (Type, Type):
  // NB: Curried calls are included in the "no inline" call!
  def ParseFunctionCallTypeTypeNoInline: Rule1[FunctionCallTypeTypeNoInline] = rule {
    ((ParseAllTypeExpressionsWithoutPositionalAmbiguity ~ "<-" ~ ParseAllTypePacks ~> (
      (source: TypeComponent, arguments: TypeComponent) => FunctionCallTypeTypeNoInline(
        source = source,
        arguments = arguments.asTypePack,
      )))
      | (ParseAllTypePacks ~ "->" ~ ParseAllTypeExpressionsWithoutPositionalAmbiguity ~> (
        (arguments: TypeComponent, source: TypeComponent) => FunctionCallTypeTypeNoInline(
          source = source,
          arguments = arguments.asTypePack,
        )
      )))
  }
  // NB: While non-inline function calls can flow from right-to-left or left-to-right, inline
  // function calls only go left-to-right.
  def ParseInlineFunctionCallTypeType: Rule1[InlineFunctionCallTypeType] = rule {
    (_ParseInlineFunctionSourceTypeExpression ~ _ParseInlineFunctionArgumentsTypeExpression) ~> (
      (source: TypeComponent, arguments: TypeComponent) => InlineFunctionCallTypeType(
        source = source,
        arguments = arguments.asTypePack,
      ))
  }
  def ParseAllTypeTypeFunctionCalls: Rule1[TypeComponent] = rule {
    ParseFunctionCallTypeTypeNoInline |
    ParseInlineFunctionCallTypeType
  }

  // Enum declarations:
  // First, cases:
  def ParseValueEnumDeclCase: Rule1[ValueEnumDeclCase] = rule {
    "\\" ~ ParseValueAlternationCaseName ~ ParseValueParamDeclNoInline.? ~> (
      (name: AlternationCaseName[ValueKind.type], decl: Option[ValueParamDeclNoInline]) =>
      ValueEnumDeclCase(
        name = name,
        defn = decl.map(_.asValueParams).getOrElse(ValueParamsPairs.empty))
    )
  }
  def ParseTypeEnumDeclCase: Rule1[TypeEnumDeclCase] = rule {
    "\\" ~ ParseTypeAlternationCaseName ~ ParseTypeParamDeclNoInline.? ~> (
      (name: AlternationCaseName[TypeKind.type], decl: Option[TypeParamDeclNoInline]) =>
      TypeEnumDeclCase(
        name = name,
        defn = decl.map(_.asTypeParams).getOrElse(TypeParamsPairs.empty))
    )
  }
  // Second, whole enum decls:
  def ParseValueEnumDecl: Rule1[ValueEnumDecl] = rule {
    ("(" ~ CommaSeparatedPack(() => ParseValueEnumDeclCase) ~ ")") ~> (
      (cases: Seq[ValueEnumDeclCase]) => ValueEnumDecl(cases)
    )
  }
  def ParseTypeEnumDecl: Rule1[TypeEnumDecl] = rule {
    ("[" ~ CommaSeparatedPack(() => ParseTypeEnumDeclCase) ~ "]") ~> (
      (cases: Seq[TypeEnumDeclCase]) => TypeEnumDecl(cases)
    )
  }
  // Third, enum case dereferences:
  def ParseValueEnumDeref: Rule1[ValueEnumDeref] = rule {
    ParseValueAlternationCaseName ~ ParseAllNonCurriedNonInlineValuePacks.? ~> (
      (name: AlternationCaseName[ValueKind.type], arguments: Option[ValueComponent]) =>
      ValueEnumDeref(
        name = name,
        arguments = arguments.map {
          args => args.asValuePack,
        }.getOrElse(ValuePackPairs.empty),
      )
    )
  }
  def ParseTypeEnumDeref: Rule1[TypeEnumDeref] = rule {
    ParseTypeAlternationCaseName ~ ParseAllNonCurriedNonInlineTypePacks.? ~> (
      (name: AlternationCaseName[TypeKind.type], arguments: Option[TypeComponent]) =>
      TypeEnumDeref(
        name = name,
        arguments = arguments.map {
          args => args.asTypePack,
        }.getOrElse(TypePackPairs.empty),
      )
    )
  }

  // Assertions:
  def ParseAssertionForValueValue: Rule1[AssertionForValueValue] = rule {
    (ParseAllValueExpressionsWithoutPositionalAmbiguity ~ "<!=" ~ ParseAllValueExpressions ~> (
      (dest: ValueComponent, source: ValueComponent) => AssertionForValueValue(dest, source)
    )
      | ParseAllValueExpressionsWithoutPositionalAmbiguity ~ "=!>" ~ ParseAllValueExpressions ~> (
        (source: ValueComponent, dest: ValueComponent) => AssertionForValueValue(dest, source)
      ))
  }
  def ParseAssertionForValueType: Rule1[AssertionForValueType] = rule {
    (ParseAllValueExpressionsWithoutPositionalAmbiguity ~ "<!=" ~ ParseAllTypeExpressions ~> (
      (dest: ValueComponent, source: TypeComponent) => AssertionForValueType(dest, source)
    )
      | ParseAllTypeExpressionsWithoutPositionalAmbiguity ~ "=!>" ~ ParseAllValueExpressions ~> (
        (source: TypeComponent, dest: ValueComponent) => AssertionForValueType(dest, source)
      ))
  }
  def ParseAllValueAssertions: Rule1[AssertionForSomeValue] = rule {
    ParseAssertionForValueValue |
    ParseAssertionForValueType
  }
  def ParseAssertionForTypeType: Rule1[AssertionForTypeType] = rule {
    (ParseAllTypeExpressionsWithoutPositionalAmbiguity ~ "<!-" ~ ParseAllTypeExpressions ~> (
      (dest: TypeComponent, source: TypeComponent) => AssertionForTypeType(dest, source)
    )
      | ParseAllTypeExpressionsWithoutPositionalAmbiguity ~ "-!>" ~ ParseAllTypeExpressions ~> (
        (source: TypeComponent, dest: TypeComponent) => AssertionForTypeType(dest, source)
      ))
  }
  def ParseAllTypeAssertions: Rule1[AssertionForSomeType] = rule {
    ParseAssertionForTypeType
  }

  def NewLine: Rule0 = rule { oneOrMore("\n") }
  def WhiteSpace: Rule0 = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  // See https://github.com/cosmicexplorer/parboiled2#handling-whitespace for recommendations on
  // handling whitespace with PEG parsers (namely, by matching whitespace strictly after every
  // terminal).
  def StickyToken(s: String): Rule0 = rule { s }
  def SpacedToken(s: String): Rule0 = rule { s ~ WhiteSpace }

  def SpacedComma: Rule0 = rule { SpacedToken(",") }
  def CommaSeparatedPack[T](r: () => Rule1[T]): Rule1[Seq[T]] = rule {
    r().+(SpacedComma) ~ SpacedComma.?
  }
}
