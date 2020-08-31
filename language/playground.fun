# -*- mode: zsh -*-

# Syntax:
# (1) <=, =>, <-, and -> are operators. <= and => imply that their source is a *value*, while <- and
#     -> imply that their source is a *type*. <= and <- *assign*, while -> and => create a named
#     parameter.
# (2) <- and -> have higher precedence than <= and =>. Parentheses are used to make meaning more
#     obvious.
# (3) Structs and enums are defined with $type-name <= ($x, $y, ...). See the definitions of :point
#     and :list (which has a type parameter) below. Note that structs CANNOT be declared using the
#     <- arrow at all right now. Enum cases are denoted with +, and struct fields with the dot `.`.
# (4) Variables (including functions) are declared with $var <= (...). See the definition of $mean
#     below. Note that variables may NOT have any type declarations on a separate line than the
#     value assignment.
# (5) Typeclasses are declared with &typeclass <= (:x ; :y; ...). See the definition of &countable
#     below. Typeclasses are implemented with the <= arrow as well -- see &countable:integer
#     below.
# (6) Type signatures can require a type implement a typeclass with (:x <- &typeclass). Type
#     signatures can explicitly get the type of $x with (:x-type <- &typeclass) -> ($x <- :x-type).
# (7) $x:y is shorthand for ($x <- :y). $x:y is shorthand for ($x <- :y), where :y is a type
#     variable.
# (8) Identifiers may contain hyphens.
# (9) ($x, $y) -> $z is shorthand for $x -> $y -> $z.
# (10) $f($x, $y) is shorthand for $f <= $x <= $y.

# TODO:
# (1) Consider whether this can be implemented with common lisp + reader macros (or something???)!
# (2) Consider whether simultaneous-productions can be used directly as a *runtime* for this logic!
#   - Similarly, consider whether then that this language leads to a very direct encoding of
#     simultaneous-productions as a Turing-complete computation mechanism!

# FIXME: Functions shouldn't return values (except MAYBE) -- they can bind values to variables! We
# can perhaps establish a convention of '$_' or something as a single positional arg / single
# positional return arg (everything else is accessed by keyword). This would also mean that
# "function application" and "destructuring" basically become the same.

# :integer is a builtin.

# For example, a '$mean' method could bind not just the arithmetic mean of a series of integers, but
# could also export the sum of those integers which was used to calculate it!
# This is the basic type signature that will only export the single return value.
$mean <- ($input <- :integer...) -> :integer
# This signature will export the sum of the values, as well as the number of values in the stream!
# NB: A type signature like this is *required* to export any non-'$_' variable from a function!
# It's also possible to export types from a function by assigning to a ':'-prefixed symbol (instead
# of a '$'-prefixed symbol). More experimentation upcoming!!
$mean <- ($input <- :integer...) -> (
  $sum <- :integer,
  $num-values <- :integer,
  $_ <- :integer
)
# Note that in Smalltalk, the ^ is used to mean "return" -- see
# https://en.wikipedia.org/wiki/Smalltalk.
$mean <= ($input <- :integer...) => (
  $sum:integer <= 0 ;
  $num-values:integer <= 0 ;
  # This performs the following statements for all elements in the $input stream. Each element is
  # assigned to the temporary variable $_.
  $input...(
    # This is the same as `$sum <= ($plus($_ <= $_))`.
    $sum <= $plus($sum, $_) ;
    # This is the same as `$num-values <= ($num-values+plus($_ <= 1))`.
    $num-values <= $plus($num-values, 1)
  );
  $_ <= $sum+divide($num-values)
)

# How to specify a typeclass (or something???): use &!
# Specify a type parameter $operand-type satisfying the &countable typeclass. Then specify a method
# parameter $input, which is a stream of $operand-type.
$mean <= (:operand-type <- &countable) -> ($input <- $operand-type...) => (
  $sum <= ($zero <- $operand-type);
  $num-values:integer <= 0;
  $input...(
    $sum <= $plus($sum, $_);
    $num-values <= $plus($num-values, 1)
  );
  $_ <= $divide($sum, $num-values)
)
# Could be shortened to:
$mean <= ($input <- &countable...) => (
  # (type inference)
  $sum <= $zero;
  $num-values:natural <= 0;
  $input...(
    $sum @<+= $_;
    $num-values @<+= 1
  );
  $_ <= $divide($sum, $num-values)
)

# One alternative for the "typeclass" idea: instead using struct definitions with function
# members. This allows specifying "trait" or "typeclass" constraints the same as specifying concrete
# type constraints (the conversion into the struct however is *explicit* instead of implicit -- this
# avoids any scoping weirdness plaguing other typeclass systems (by cirtue of the fact of them
# having multiple type subsystems doing the same thing)).

# NB: I *think* that both inheritance and typeclasses are forms of dependency injection. How?
# Inheritance: the parent type's methods are the interface. You want to construct your own
#              implementation of those methods. The up/downcasting rules are the dependency
#              injection framework that lets you get a parent class from a child class and vice
#              versa.
# Typeclasses: the typeclass's methods are similar to the "canonical struct" `:countable` (for what
#              used to be the &countable typeclass) below. When the typeclass is implemented, that
#              says it is safe to inject the implementing type as an instance of the typeclass.
$countable <= \.type -> (
  .zero <- .type,
  .plus <- \(.lhs[.type] .rhs[.type]) => .type,
)
$count-integers <- [$countable <- $Integer] <= (
  .zero <= 0,
  .plus <= \(.lhs, .rhs) => ($Integer-plus <= (.lhs, .rhs)),
)
# Could be shortened to:
$count-integers[$countable[$Integer]] <= (
  .zero <= 0,
  .plus <= \(.lhs, .rhs) => $Integer-plus(.lhs, .rhs)
)

# NB: The struct definition :countable above is just a function with a type parameter. We can remove
# the ":" prefix here to remove some confusion.
$countable <= \.Type -> (
  .zero[.Type],
  .plus[(\.lhs[.Type], \.rhs[.Type]) => .Type],
)

$countable-instance <= $countable[$Integer](
  .zero(0),
  .plus((\.lhs, \.rhs) => $integer-plus(.lhs, .rhs)),
)

$add <= [\.T] -> (\.instance <~ $countable[.T]) => (\.x, \.y) => .instance.plus(.x, .y)
# Or:
$add <= (\.instance{$countable[\.T]}) => .instance.plus(.lhs(\.x), .rhs(\.y))
# Where (explicitly providing the implicit argument):
$add(.instance($countable-instance), .x(3), .y(4)) <= 7
# Or, implicitly:
# (1) Defining $countable-instance as an implicit provider for a $countable[$Integer].
$countable-instance <~ $countable[$Integer](
  .zero(0),
  .plus((\.lhs, \.rhs) => $integer-plus(.lhs, .rhs)),
)
# (2) After the above statement, (\.instance <~ $countable[$Integer]) is now satisfied implicitly:
$add(3, 4) <= 7
$add(.x(3), .y(4)) <= 7



# Or:
$countable-instance[$countable[$Integer]] <= (
  .zero <= 0,
  .plus <= \(.lhs, .rhs) => $plus(.lhs, .rhs),
)

$greater-than <= \(.a[$Integer], .b[$Integer]) => (
  +($Integer-gt(.a, .b)) => $Boolean+true,
  +() => $Boolean+false,
)
# Or:
$greater-than <= \(.a, .b) => $Boolean -> (
  # Using named instead of positional arguments this time:
  +($Integer-gt((.a), (.b))) => +true,
  +() => +false,
)
# Or (where `(...) x $ident` applies the type $ident over all elements of (...)!!)!:
$greater-than <= \(.a, .b) x [$Integer] => (
  +($Integer-gt(_, _)) => +true,
  +() => +false,
) <- $Boolean

# Example of computing a type $Type-Result by invoking a method $greater-than, which is only defined
# as a value function!!
$Type-Result <- \[.k[$Integer]] -> [
  +[$greater-than(_, 4)] -> $Integer,
  +[] -> $Boolean,
]
# Both of the following statements will type-check correctly!
$x[$Type-Result[5]] <= 3
$x[$Type-Result[4]] <= +true

# It's possible to declare a parameter then dereference it in a single expression, since \.k and .k
# are distinct in the syntax!
$Type-Result <- $greater-than[\.k[$Integer], 4] -> [
  +[+true] -> $Integer,
  +[+false] -> $Boolean,
]

$Enum-Like-Type <- [
  +a,
  +b[\.c[@hasImplicitConversion[\.S, \.T] -> [
            +[+some] -> .S,
            +[] -> .T,
          ]]],
]
# At runtime, there are no fields, so this is the only allowable value assignment for both type
# variants.
# Because the type parameters \.S and \.T don't affect the definition of the +a type variant, we are
# allowed to omit them in the type call below.
$x[$Enum-Like-Type+a] <= ()
$x[$Enum-Like-Type[$Integer, $Boolean]+b[.c[$Integer]]]] <= ()

$y <= (+a)
$z <= (+b(.c <= 3))
# Or:
$z <= (+b(.c(3)))
$x <= ($y, $z) => (
  +(+a, +b(.c)) => .c,
)

# Using (+() => ...) as a match statement.
$result <= $x => (
  # Like magrittr, this curried comparison $greater-than($b <= 5) is applying a function.
  # $greater-than(_, 5) would also have worked (a positional curry).
  +($greater-than($b <= 5)) => ...,
  # "else", "_", or "default" clause is +().
  +() => ...,
)

# Using -> and [+() -> ...] to match at the type level.
$result <- $x -> [
  +a,
  +b[.x <- $y],
]
$some-type <- $result -> [
  +a -> $Integer,
  +b[.x] -> .x,
]

# Contemplating removing the final remaining $ sigil for types and values:
some-type <- result -> [
  +a -> integer,
  +b[.x] -> .x,
]


# Scala-like dependency-injected "implicit" parameters do not need to be specially declared in the
# method definition. Rather, the *caller* may designate values (including functions) which may be
# consumed to satisfy an "explicit implicit" conversion as well as to provide defaults for
# parameters not given in the call to a specific function.
# E.g. given $f defined as below:
$f <= \.x => (\.x <= $x)
# Note the EXTREME similarity to the below statement to define $f as a struct:
$f <= (\.x)
# Equivalent to:
$f <= \.x => (.x)

# Note that the below is a parse error:
$f <= \.x

# This statement sets the parameter \.x from the function/struct $f to 3, unless it is explicitly
# provided (by name or position) when calling $f.
@addImplicit($f\.x) <= 3
# This statement deregisters the implicit binding for $f\.x:
@removeImplicit($f\.x)

# This can be done for all parameters of a given name, but this is obviously much more likely to
# lead to a compile error.
@addImplicit(\.x) <= 3

# Whenever any *single* $Integer param in a parameter pack is not provided, this sets the value to
# 3.
@addImplicit[$Integer] <= 3
# The type and value parameters can be composed for this macro (covering all $Integer parameters
# named \.x:
@addImplicit[$Integer](\.x) <= 3

# This statement will convert any $Integer argument for the @convert() macro to $Boolean.
# NB: The @convert() macro will apply subsequent conversions transitively until it reaches the
# desired result (in this case, $Boolean).
# NB: If setting the parameter $f\.x or the converter [$Integer -> $Boolean] results in multiple
# values specified for a parameter, or multiple transitive pathways from some type A to some type B
# in the universe of implicit conversions, a compile error is raised.
@addImplicit[$Integer -> $Boolean] <= (
  +(0) => +false,
  +() => +true,
)

# The below sets the value $ret to the result of @convert(0 <- $Integer), and verifies that the
# result is a $Boolean.
$ret[$Boolean] <= @convert(0[$Integer])


$optional <= (+some(\.inner[\.innerType]), +none)
# Or:
$optional <= \.innerType -> \(+some(\.inner[.innerType]), +none)
# Or:
$optional <= \[.innerType] -> (
  +some <= (\.inner <- .innerType),
  +none <= (),
)
# Or:
$optional <= (
  +some(\.inner[\.innerType]),
  +none,
)

# Use this macro as a type bound for type parameters!
@hasImplicitConversion <- \[.fromType, .toType] -> $optional[.fromType -> .toType]
# Could also use an anonymous enum class:
@hasImplicitConversion <- \[.fromType, .toType] -> [
  +has-conversion(.converter[.fromType -> .toType]),
  +no-conversion-available,
]

$z <- [
  +A,
]

$y <= \.x => {
  +SomeProduction,
  +OtherProd,
}

$x <= (\.input[$Integer...]) =~> {
  +{1 ~ 3 ~ $EOI} ! "oops!",
  +{1 ~ 3} ~> "asdf",
} ~=> (
  +("asdf") => -[3],
) =-> [
  +[$Integer] -> "wow"
] -=> (
  +("hey") => =(3)
)

# Using <= or => on two rvalues checks for equality!!
"134" => {+{1 ~ 3 ~ 4} ~> "asdf"} => "asdf" !

# \.s[\.S] is equivalent to (\.S -> \.s[.S])
$some-value <= \.s[\.S] => @hasImplicitConversion[.S, $Boolean] => (
  +some($converter <= .inner) => $converter(.s),
  # Can also do:
  +some(.inner => $converter) => $converter(.s),
  # If no
  +none => +false,
) <- $Boolean

# $some-value[$Integer] is a runtime value, but we can evaluate it at compile time either by writing
# `$some-value[$Integer][$Boolean]`, or `$some-value[$Integer] <- [$Boolean]`, to get the would-be
# runtime value, but in the type context (allowing us to set it as a type to $x, and to match with:
# `[+[<runtime value>] -> <some type to return and set $x to>]`
$x <- $some-value[$Integer][$Boolean] -> [
  +[+true] -> $float,
  +[+false] -> $double,
]

# Declaring a typeclass &countable, with a type parameter $type, with several operations (note that
# the <= arrow is used to declare a typeclass -- the -> arrow here declares a type parameter $type).
&countable <= \.type -> (
  # Note that there is no difference between a value and a zero-argument function (???)???
  $zero <- .type;
  $plus <- \(.lhs <- .type) => \(.rhs <- .type) => .type
)
# Could be shortened to:
&countable <= \.type -> (
  $zero <- .type;
  $plus <- \(.lhs[.type], .rhs[.type]) => .type
)

# Implementing &countable for the type :integer (note that the <= arrow is used to implement a
# typeclass -- the <- arrow binds :integer to the type parameter $type).
# Note that <- has higher precedence than <=, so parens around (&countable <- :integer) are not
# needed.
&countable <- :integer <= (
  $zero <= 0L;
  # NB: Assumed that $Integer-add is somehow provided by the prelude.
  $plus <= ($lhs:integer, $rhs:integer) => $Integer-add($lhs, $rhs)
)
# Could be shortened to:
&countable:integer <= (
  $zero <= 0L;
  # (due to type inference)
  $plus <= ($lhs, $rhs) => $Integer-add($lhs, $rhs)
)

# Bikesheddable struct/enum class declaration syntax. All constructor arguments automatically become
# the named struct fields!
$point <= \(.x <- $Integer, .y <- $Integer)
# Could be shortened to:
$point <= \(.x[$Integer], .y[$Integer])

# NB: :element is a type variable!! Hence being manipulated with ->!
$list <= \.Element -> \(
  +none <= (),
  # NB: :Self is a type variable!! Hence being dereferenced with <-!
  +cons <= (.car <- $element, .cdr <- $Self)
)
# Could be shortened to:
$list <= \.Element -> (
  \+none,
  \+cons(\.car[.Element], \.cdr[.Self]),
)
# Or:
$list <= [\.Element](...)
# Or:
$list[\.Element] <= (...)

# Note that this infers .Element <- $Integer
$args <= $list+cons(.car(3), .cdr(+none))
# Note that this expression:
-[$args] <- $list[$Integer]
# Is equivalent to (dereferencing the free type variable $Integer and then assigning it to the
# scoped type variable .Element of the $list struct):
-[$args] <- [$list <- $Integer]
# Or, with named arguments:
-[$args] <- $list[.Element[$Integer]]

# TODO: require that all types have an initial capital letter!!
$named-list[\.El-type] <= (
  +none,
  +cons(.car[.El-type], .car-name[$Identifier], .cdr[.Self])
)

$Enum-Type <- [
  \+a,
  # Type and value fields can be intermixed in the same struct. Initial capitals always refers to a
  # type.
  \+b[\.d, \.D],
]
$y <= [$Enum-Type <- +a]
$y <= $Enum-Type[+a]
$y <= $Enum-Type+a
$z <= $Enum-Type+b <- [\.D -> \.d => $Enum-Type]

$Enum-Type[+b] <- [\.d, \.D]

$Enum-Type <- [
  \+a <- [] <= (),
  \+b <- \.D <= \.d,
]
$Enum-Type <- [
  \+a,
  \+b[\.D](\.d),
]
$Enum-Type[+b][$Integer](.d <= 3) <= (.d(3)) <- [.d[$Integer]]

$Nested-Enum-Type <- [
  \+a[
    \+s(\.x),
    \+t,
  ],
  \+b,
]

$Nested-Enum-Type[+a] <- [+s(\.x), \+t]
$Nested-Enum-Type[+a][+s] <= \.x
$Nested-Enum-Type[+a][+s] <= \.x => .x

$set <= (
  \+(1),
  \+(2),
) <- [+()]

$map <= (
  \+(1) => "a",
  \+(2) => "b",
) <- [+() => $String]

$typeSet <= [
  \+[$Integer],
  \+[$Boolean],
] <- [+[]]
# [+[]] is equivalent to [+[] -> $Boolean]

$typeMap <= [
  \+[$Integer] -> "a",
  \+[$Boolean] -> "b",
] <- [+[] -> $String]

$typeMap[+[$Integer]] <= "a"

$Enum-Type <- [\+a, \+b[\.D -> \.d]]
# The following two are equivalent!!
$Enum-Type[+a] <- ()
$Enum-Type[+a] <= ()
# The following three are equivalent!!
$Enum-Type <- \+b -> \.D -> (\.d) => (.d)
$Enum-Type <- [\+b -> +b] -> [\.D -> ((\.d) => (.d))]
$Enum-Type[+b] <= (.d) <= \.d <- \.D
$Enum-Type[+b] <- \.D <= (.d) <= \.d
$Enum-Type[+b][.D <- $Integer] <= (.d) <= \.d
$Enum-Type[+b][.D <- $Integer] <= (\.d)

$Struct-type <= \.d <- \.D
$Struct-type <- \.D <= \.d

$X <- $Enum-Type+b -> [
  # NB: note that we do not need to provide a placeholder `_` for the `.d` field.
  # NB: note that while `.D` can be matched at "typing time" (or "compile time"), binding `.d`
  # *cannot* be done inside of a `... -> [+...]` block, since
  (\.D) -> .D
]
$Enum-Type+b <- [\.D] -> ((\.d) => ()) !


$X <= $Enum-Type+b => (
  +a
)

# @make-list is an attempt to define a macro syntax????
# (.args / ";") expands (at macro time) a parameter pack to match a sequence of inputs separated
# according to the arguments (in this case, by a semicolon). If $args is a $list[_] like here, the
# macro accepts only positional, and not named arguments.
# NB: `=[_]` takes the value expression at `_` and inserts it as a literal into a value context (due
# to the use of `<=`), replacing the @make-list() macro call in the generated code.
# replacing where
@make-list <= \.args$list[\.Element] ~> {+{.args / ";"} ~> =[.args]}
@make-list(1 ; 3) ! $list[$Integer]+cons(.car(1), .cdr(+cons(.car(3), .cdr(+none))))

$Integer-values <- $list <- [.Element <- $Integer]
# Could be shortened to:
$Integer-values <- $list[$Integer]
# Invoke the macro @make-list<&element, &Args...<- &element>, inferring parameter pack matching.
$Integer-values <= @make-list(1 ; 3 ; 5 ; 2 ; 1)

# Attempted infix @<+= operator:
@`<+=` <~ ((:operand-type <- &countable) -> ($lhs:operand-type, $rhs:operand-type)
  # NB: People say type-safe macros are VERY HARD TO IMPLEMENT!!!!
  # @=> attempts to bind a possibly-typechecked parameter specification to the arguments provided to
  # the macro. In this case, there will be two arguments since it is an infix operator.
  @=> ($lhs <= $plus($lhs, $rhs)))
# Could be shortened to:
@`<+=` <~ ((:op&countable, $lhs:op, $rhs:op) @=> ($lhs <= $plus($lhs, $rhs)))
# Could be turned non-typechecked with:
@`<+=` <~ (($lhs, $rhs) @=> ($lhs <= $plus($lhs, $rhs)))
# Usage:
$x <= 4    # x is now 4
$x @<+= 1  # x is now 5

# Another example method:
$f <= (:op&countable, $x:op, $y:op) => $plus($x, $y)
# While this is often done implicitly, we can index into the specific type parameter $op as needed.
$x <= ($f <- :integer) <= (1, 2)  # => x is now 3


@a-macro <~ (
  ($x:integer => $y:integer) @=> $Integer-plus($x, $y);
  $z:string @=> $concat($z, $z);
  dynamic => $parse
)


:boolean <= (
  +true,
  +false
)

&hashable <= $type -> (
  $hash <- $type -> :integer;
  $equals <- ($lhs$type, $rhs$type) -> :boolean
)

&hashable:integer <= (
  $hash <= $_:integer => $_;
  $equals <= ($lhs:integer, $rhs:integer) => $Integer-equals($lhs, $rhs)
)

# This *could* work as a map, but doesn't have any working methods yet.
:map <= [:k&hashable, :v] -> (.pairs:list(:element <- (:k, :v)))

$the-map <= :map[:integer, :integer]((1L, 1L) ; (2L, 4L))

$get <= [:k&hashable, :v] -> $map:map[:k, :v] => $key:k => (
  $the-hash <= $hash($key);
  ($_, $value) <= @list-get($map.pairs $the-hash);
  ^$value:v
)

$value <= $get($the-map, 2L)  # => 4L

# Example of destructuring to bind the result of a method which has exported non-'$_' variables!
($Integer-sum <= $sum, $num-elements <= $num-values, $mean <= $_) <= $Integer-values+mean
# '!' is still-experimental syntax to force an "assertion" for '?'-typed output (is '?' a type????)!
$Integer-sum+equals(12) !
$num-elements+equals(5) !
# This is integer division!
$mean+equals(2) !

# If not destructured by opening a scope (using parentheses, as above), the value of '$_' is assumed
# to be used!
$Integer-values+mean+equals(2) !


# importing symbols!!
# NB: 'import' is provided by the runtime!
(:bc, $printf-specs) <= import('common')

# defining functions (different syntax than variables and productions!!!)
# wrapping shell functions (redefining :bc from above):
# NB: 'call-cmd' is provided by the runtime!
# NB: Only intrinsics can be referenced without any sigil (???)!
# TODO: do all intrinsics only accept positional/keyword args (instead of being a production too?)?
# ^I think actually, they can just be productions too! Just ones with hidden definitions (?)!
:bc <= ($arg <= :string) => call-cmd(bc ; "$arg")

# TODO: are productions the same as functions??? (YES!!! SEE THIS!!!)
# [OUTDATED; INCORRECT] NB: '$a <= :integer' is the same as '$a <= :integer'!
# '<match-expr> <= <match-expr type="lazy-eval" [<match-expr> => <eval-body>]> => <match-expr>'
# NB[OUTDATED; INCORRECT]: USING THE BACKSLASH AS IMPLICIT XARGS IS...I N C R E D I B L E!!!!!
# NB: This '=>' construction can compile almost directly to a `while read -r`!
:calculate <= ($a <= :integer ; $op ; $b <= :integer) => :bc("$a$op$b")
  => :integer
# equivalent:
:calculate <= :integer <= ($a <= :integer ; $op ; $b <= :integer) => \:bc("${a}${op}${b}")
# equivalent in "shell syntax":
function calculate() {
  # FIXME: if any of these fail, the function should fail with an error!
  local -r a="$(parse-arg :integer "$1")" # `parse-arg` is from the prelude/runtime!
  local -r op="$2"
  local -r b="$(parse-arg :integer "$3")"

  # NB: The `$(extract-printf-spec-from-type ...)` nodes should just get compiled to a literal '%d%s%d' string here.
  local -r __generated_printf_spec_1="$(extract-printf-spec-from-type '$a')$(... '$op')$(... '$b')"
  printf "$__generated_printf_spec_1" "$a" "$op" "$b" \
    | bc \
    | parse :integer            # `parse` is from the prelude/runtime!
  # TODO: in reality, the `parse :integer` will just be an intermediate node in the compiler, and will filter for /[-+]?[0-9]+/.
}
# start defining productions!

# TODO: are productions the same as types??? (WHY NOT???)
# /.../ has the type (str => str?). That's an optional `str?`, not a question mark.
:integer <= /[-+]?[0-9]+/ => (
  # NB: => will have lower precedence than ;, and ',' will have lower precedence than anything (?).
  # NB: first usage of '+<case-name>($params...)? <= <expr>,' (bind case) and
  # '<variable-reference> => <eval-body>' (evaluate body after local variable is bound)!
  # NB: `calculate()` is declared to return `:integer`, so this type-checks ok!
  # NB: the backslash means calling the production with specified arguments!
  +plus($x <= :integer) => \:calculate($_ ; '+' ; $x) ;
  # equivalent to the above:
  # $_ declares a matcher which accepts anything and passes it on unchanged, '-' and $x declare
  # "constant matchers" which accept nothing and echo their literal output. So the expression
  # ($_ ; '-' ; $x) has the type (T => (T, str, integer)), where T is inferred from the input.
  +minus($x <= :integer) => ($_ ; '-' ; $x) => :calculate
)
# NB: The above is shorthand for the below!!
# TODO: is ':string' optional (is it assumed unless specified?)?
:integer <- (:string?)
:integer <= :string => /[-+]?[0-9]+/
# This is a type declaration for the below member functions.
:integer <- (
  +plus($x <- :integer) -> :integer ;
  +minus($x <- :integer) -> :integer
)
:integer <= (
  +plus($x <= :integer) => :calculate($_ ; '+'; $x) ;
  +minus($x <= :integer) => :calculate($_ ; '-'; $x) ;
  +divide($x <= :integer) => :calculate($_ ; '/' ; $x)
)
# ^Note that this also specifies a parseable declaration (and/or a curryable function!!!)!

# The below asserts that there must be some value declaration somewhere (somewhere in this file?)
# which satisfies (:float ->? :integer).
:integer <- (:float?)
# The parens here are redundant.
:integer <= (
  :float => ???
)

# Note with the above that:
# 1. '($_ => calculate(...))' is a <match-expr>.
# 2. '$_' is a <bind-expr> which returns the input unmodified.
# 3. <bind-expr> \subsetneq <match-expr>
# 4. '($_ => calculate(...))' can be abstracted into '<bind-expr> => <function-call>'.

# NB: attempt at a typed production manipulating typed subproductions!
# NB: The '<= :integer' type annotation here is optional, as it is inferred from the ':integer'
# return type of ':integer+plus($x <= :integer)'!
# NB: '+' is a valid "type declaration" as well as a valid "value", because constants are singleton
# types!!! (???)
# NB: The below is an attempt at a "type declaration" for the "function" (?) ':add'!
# NB: The '$_' in type declarations refers to an argument that's only relevant for parsing (aka
# (right now) a constant value).
:add <- ($a <- :integer ; $_ <- '+'; $b <- :integer) -> :integer
:add <= :integer <= ($a:integer ; '+' ; $b:integer) => $a+plus($b)

# TODO: constant values are ignored when calling a method as a function (and therefore only used in
# parsing) -- is this what we want?

# Equivalent formulations.
$six <= \:add($a <= '2' ; $b <= '4')
$six <= \:add('2' ; '4')
$six <= ($a <= '2' ; $b <= '4') => :add
# This type annotation is checked at compile time!
$six <- :integer
$six <= :add <= ($a <= '2' ; $b <= '4')
# TODO: this uses integer literals, which are already tagged as an integer!
$six <= \:add(2 ; 4)
# This is also a valid way to write it!
$six <= (2 ; 4):add

# Modifying existing variables (associative array):
$printf-specs+integer <= 'd'
# equivalent:
# NB: first usage of ':<production-name> => <eval-body>'.
# This is probably better described as '<bind-expr> => <eval-body>'.
# TODO: do this in a way such that only functions in this file are affected by this.
# TOOD: make this syntax usable such that e.g. $printf-specs+integer can be *temporarily overridden within a production!!!*
$printf-specs <= (
  # This nested initialization is like groovy!
  +integer => 'd' ;
)

# The $six variable is defined in whatever the current scope is, and (without a type annotation)
# implicitly typed :integer (in this case, since that's what '+plus' returns!)!
$six <= '4':integer+plus('3')

:port_num <- (:string? -> :integer)
:port_num <= /[0-9]/:integer
# equivalent to above:
# NB: the below is *not* ambiguous, specifically because we explicitly do not define any
# "assignment" for the => operator. The <= only "assigns" when used at the leftmost of an
# expression.
:port_num <= /[0-9]+/ => :integer
# Equivalent to above (demonstrating how destructuring/typing/everything goes together so
# naturally):
# TODO: is '<=' the inverse of '|'?
# TO-DONE! yes! in fact, we have just removed '|' in favor of '=>'!!!
:port_num <= :integer <= /[0-9]+/
# equivalent to above
function port_num() {
  sed -n -E -e '/^[0-9]+$/ p' | parse :integer # Where `parse` is from the prelude.
}

:port <- ($_ <- :string? ; $num <- (:port_num <- (:string? -> :integer)))
:port <- ($_ <- :string? ; $num <- (:string? -> :integer))
# the following line is in "match syntax"
:port <= ':' ; $num <= :port_num
# equivalent to above, but in "shell syntax":
function port() {
  sed -n -E -e 's#^:(.*)$#\1#gp' | port_num
}

:file_or_https_scheme <= (
  +file <= /file/ ;
  +https <= /https/
)
# equivalent to above, but in "shell syntax":
function __generated_method_1() {
  local -r ctx="${1}+file"
  sed -n -E -e '/^file$/ p' | while read -r __generated_arg_1; do
    # NB: This is a command for an interpreter for a (stack machine?)!
    echo "${ctx}=${__generated_arg_1}"
  done
}
function __generated_method_2() {
  local -r ctx="${1}+https"
  sed -n -E -e '/^https$/ p' | while read -r __generated_arg_2; do
    echo "${ctx}=${__generated_arg_2}"
  done
}
function file_or_https_scheme() {
  local -r ctx="$1:file_or_https_scheme<$(uuid)>" # Function `uuid` is from the runtime.
  tee >(__generated_method_1 "$ctx") | __generated_method_2 "$ctx"
}

# NB: second usage of '<variable-reference> => <eval-body>',
# first usage of '+<case-name>($params...)? => <expr>,'.
:file_or_https_url <= :url =>
  ($validated_schema <= $scheme:file_or_https_scheme) ;
  $host <= /[^\/]+/ ;
  # `$validated_schema` will definitely have been defined already, as it must have been previously
  # satisfied within a sequential match ordering (via ';'), so we are allowed to dereference/use its
  # value here.
  # '=>' has lower precedence than '<='.
  ($some_port_num <= $validated_schema => (
    +file => :port$num ;
    # NB: Each of the below are equivalent formulations of a type annotation for this case!
    +https -> ($_ <- :string? ; $num <- (:string? -> :integer)) -> (? -> :integer) ;
    +https -> (:string? ; (:string? -> :integer)) -> (? -> :integer) ;
    # NB: Note that the ':string?' is "canceled" by the '(? -> :integer)', which recovers from match
    # failures (!!!!!!).
    +https -> :string -> :integer ;
    +https => :port$num ? 443
  )) ;
  # The below is only used for parsing, not when calling it as a function!
  # (:string? ; :string) -> (? -> '')
  ('/' ; :string...)?


:add-all <- ($x <- :integer...) -> ($sum <- :integer) -> :integer
# This below type signature is a little simplified -- it doesn't have the complete signature of the
# method (since it doesn't describe the type of '$sum', a variable defined within the method), but
# it *would* pass typechecking to add this type annotation, since it is a subset of the real
# signature.
:add-all <- ($x <- :integer...) -> :integer
:add-all <= ($x <= :integer...) => ($sum <= 0 ; $sum <= $sum+plus($x)...) => $sum

$seven <= \:add-all(1 ; 2 ; 4)

# '+(:integer -> :integer)' is how to represent an associative mapping (???)!
(:count-occurrences <- (:k)) <- (:k... -> +(:k -> :integer))
# The below type annotation is also correct, but more general.
(:count-occurrences <- (:k)) <- (:k... -> (:k -> :integer))
# '+()' creates an empty mapping.
# We declare a type parameter ':k', which we then assign to the '+()'. We could have written
# '+() <= (:k <- :k)' -- this is just shorthand for that because ':k' and ':k' are named the same.
# We know that ':count-occurrences <- (:k)' declares a type parameter, while '+() <- (:k)'
# dereferences that type parameter, because type parameters can only be declared at the top level
# (...maybe. We may be missing some cases but I think this is a good start.).
:count-occurrences <- (:k) <= ($result <= +() <- (:k ; :v <- :integer);
                               ($result+($x) <= $result+($x <= :k...)+plus(1) ? 1)...)
  => $result
# This is equivalent to the above!
:count-occurrences <- (:k) <= (
  # NB: note the inline type annotation here!
  $result <= +() <- (:k <- :k ; :v <- :integer) ;
  $x <= :k... ;
  ($result+($x) <= $result+($x)+plus(1) ? 1)...
  )
  => $result

$occurrences <- +(:integer -> :integer)
$occurrences <= \:count-occurrences(1 ; 2 ; 4 ; 5 ; 1 ; 4; 2 ; 1)

# Just an idle thought -- a mapping is a function with a finite input domain.
# Not sure if this definition is provided by the prelude, or whether users can define such a
# relation...
# FIXME: is declaring a datatype and a function the same thing???? could it be???
# NB: This function takes two type parameters!!! ISN'T THIS IT????
:map <- (:k ; :v) -> ($f <- (:k -> :v) ; $keys <- +(:k...))
# '+(:k...)' says "take a stream of ':k', and convert it into a set!"! The exact same expression
# works above on the type level with '$keys <- +(:k...)'!
:map <= ($f <= (:k => :v) ; $keys <= +(:k...))
:map <= (
  +($k <= $v)... <= $keys => ($k <= :k ; $v <= \.f($k))...
)

:map => (
  +partial-function -> (:k -> :v?) <= :k => ($keys => (
    +($_)
  ))
)

# This could be a possible way to allow users to define the destructuring operation '+($k <= $v)'
# (used below in the '+get-single-occurrences' case!!!).
:map <= (
  # NB: the '<=' arrow is used here, which indicates an "unapply" (destructuring) is being defined
  # instead of an "apply" (function application).
  # ^!!!!!!!!! REALLY GOOD IDEA!!!!
  # '$keys' and '$f' were brought into scope by virtue of being declared in the above definition of
  # +(:integer -> :integer).
  +($k <= $v)... <= $keys => ($k <= :k ; $v <= \.f($k))...
)

# This declares a method only on the specialization of :map for the type parameter
# ':v <- :integer'. This specialization is intended to look like a function call (which is only
# being partially evaluated -- the ':k' is still unbound (i.e. curried)).
:map(:v <- :integer) <= (
  # These type annotations are equivalent, because '...' subsumes '?' here.
  +get-single-occurrences -> :_ -> ? -> :integer...
  +get-single-occurrences -> :_ -> :integer...
  # FIXME: figure out how to define ':integer+equals(:integer)'!!! (which returns '?')!?
  +get-single-occurrences => +($k <= $v)... => $v+equals(1) => $k...
)
# Alternative type annotation for the above.
# ':_' here means "whatever type I'm currently assigning to at the top level".
:map(:v <- :integer)+get-single-occurrences <- (:_ -> :integer...)

$single-occurences <= $occurrences+get-single-occurrences
# Where (:integer...)+equals(:integer...) <- ?
# NB: The '!' is very important here! It creates an error if there is no match (instead of quietly
# continuing with trying to match something).
# TODO: what kind of error? Is this fatal? I think yes, since we're going for an "assert!()" kind of
# vibe. The exclamation point is a good token to use here, but it's not clear whether we want
# first-class support for this "assert!()" kind of vibe.
$single-occurrences+equals(5) !


# TODO: can we just use '+' for all conditional logic????????????


# code?
($schema <= $validated_schema ; $port_num <= $some_port_num) <= $1:file_or_https_scheme

# https://blog.golang.org/a-new-go-api-for-protocol-buffers
# go uses f().(*X) to "type assert" that function f returns a pointer of type X at runtime.

$runtimeType <= \.T -> (
  # The below two declarations are the same:
  .T -> \.T,
  [.T]\.T,
  # This one does not require \.T to be declared as a type parameter in the preceding expression.
  [\.T],
  \.x <- [...],
  \.y <- [...],
  \.z <- [...],
  ...
)
# Such that the below two are equivalent:
$x <- $type
$x <- $runtimeType[$type].T

$regex <= (.pattern$string) => (...)
$matchRegex <= (.regex$regex) => {.input$string} ~> {
  +{"A" ~ "B" ~ "C"} ~> (.a <= "wow"),
  +{"A" ~> \.a$string} ~> (.a),
}
# This doesn't seem to make any sense backwards.
# {
#   +(.a("wow")){"A" ~ "B" ~ "C"},
#   +(.a) <~ {"A" ~> \.a$string},
# }

$paramName <= {.id$string} ~> {
  +{"A" | "B"} ~> $Boolean+true,
}

$varPlace <= ???

$implicitConversion <= \[.S, .T] -> (
  \.type <- $optional[$runtimeType[.T]],
  \.method <- $optional[.S -> .T],
  \.paramName <- $optional[$paramName],
  \.containingMethod <- $optional[$varPlace],
)

$implicitConversionsTable <= $map[$implicitConversion, \.V]()


$hasImplicitConversion <= \[.S, .T] -> $optional[.S, .T] => (

)
