# -*- mode: zsh -*-

# Syntax:
# (1) There are two main expression kinds, each with their own paired delimiters for grouping
# expressions of that kind:
#     [] groups "type"s.
#     () groups "value"s.
#     These two kinds of expressions interspersed throughout a single top-level statement can often
#     be commuted in order to group together consecutive expressions of the same kind.
# (2) t <= s, s => t means the "source expression" s is of kind *value*.
#     T <- S, S -> T means the "source expression" S is of kind *type*.
#     t or T is then a corresponding "place expression" for either kind.
#     There are inline conjoiners to abuse associativity. s or S is a place expression of the
#     corresponding kind:
#     S[T] expands to S <- [T], and [T]S expands to [S] -> S.
#     s(t) expands to s <= (t), and (t)s expands to (t) => s.
#     [A, B, C] -> T expands to A -> B -> C -> T.
#     (a, b, c) => t expands to a => b => c => t.
# (3) Every identifier (matching "[a-zA-Z0-9_-]+") is prefaced with a sigil:
#     $x implies that x is a "free" variable, of kind *value*.
#     .x implies that x is a "scoped" variable, of kind *value*.
#     +x implies that x is an statically-determined enum case.
#     .X or $X capitalized refers to a corresponding variable of kind *type* (this capitalization
#     rule does not apply to enum cases, which must all be lowercase).
# (4) Prefacing any dot sigil identifier with a backslash \.x, \.X declares the identifier as a
#     parameter of the corresponding kind. Prefacing a delimited grouping with a backslash will
# convert the group into a type or value positional and/or (?) named parameter pack:
#     \[.X, .Y] -> T expands to \.X -> \.Y -> T.
#     (.x, .y) => t expands to \.x => \.y => t.
#     Any source expression containing an inline parameter declaration will have the declaration
#     moved out of the expression:
#     $my-function(\.x, .y(3)) expands to \.x => $my-function(.x <= .x, .y <= 3)
#     Note that \.x => $f(.x <= .x) can be shortened to \.x => $f(.x, .y(3)), because scoped
#     parameters are *never* allowed to shadow another that is still in scope, so normally,
#     \.x => $f(.x) would cause a compile error at type resolution time! But the use of the inline
#     $f(\.x) allows us to avoid that restriction!
#     However, also note that a function can have just one of its arguments set by name, converting
#     it into a function with one fewer argument. So in this case:
#     $my-function(.y(3)) is equivalent to \.x => $my-function(.x <= .x, y <= 3).
# (5) Struct and enum literals can be *source expressions* in value or type contexts:
#     $X <- \[\.x] declares a free type $X and binds it to a struct definition with a field .x.
#     $x <= (\.x) declares a free value $x and binds it to a struct literal with a field .x.
#     $X <- \[\+a] declares a free type $X and binds it to an enum definition with a variant +a.
#     $x <= (\+a) (using +a as a source expression of value kind) IS A PARSE ERROR!
# (6) Enum and struct literals can also act as *place expressions* in
#     value or type contexts to perform conditional logic with a destructuring match. This
#     destructuring can be chained in order to bind deeply nested data:
#     3 => (
#       +($equals(3))+true => ...; <matching on the equality of the expression in parens. note that
#                                   we do not "specially" handle any enums or structs here --
#                                   returning $Boolean is however still easily handled>
#       +($other-fun)+(.x) => ...; <.x is a field in the struct returned by $other-fun(3), and
#                                   bound in the expression ...>
#       +($equals(4))+true => ...; <this is an ordered match, so we use semicolons.>
#       +(\.-) => ...; <not putting any destructuring match expression after the parens will cause
#                       the condition to be unconditionally accepted, but the inside of the +(...)
#                       must still be a function accepting one positional argument.>
#     )
#     Note that (\.-) is a valid expression that expands to (\.-) => \.-, the anonymous identity
#     function. \._ is instead used for a placeholder scoped type parameter.
#     $My-Struct-Type(.x(3)) => ( <this destructures a struct to bind some or all of its fields in
#                                  an expression>
#       +(.x) => ... <optional semicolon -- this binds the scoped field .x in the expression ...>
#     )
#     $My-Enum-Type+a(.x(3)) => (
#       +a(.x) => ...; <this binds the scoped field .x to the value 3 from the constructor.>
#       +b => ...;   <the final semicolon or comma is always optional.>
#     )
#     $Integer -> [
#       +($type-equals[\._, $Integer])+true => [...];
#       +[\._ => $Boolean+true => ( <we are evaluating a value expression and passing it through
#                             an enum destructuring in order to return a type>
#         +true => $Boolean; <this is computed at "type resolution" or "compile" time!>
#         +false => $Integer;
#        )] -> ...;
#     ]
# (7) Comments are specified within the paired delimiters <...>. These can be nested.

-[3] <- $Integer

$X <- [
  \+A,           <$X+A is a newly created type>
  \+B(\.x[\.X]), <$X+B is a type with a type field .X and a value field .x with type .X>
  # Or:
  \+B[\.X](\.x <- .X),
]
$X+A <- $X+A
$X+B[$Integer](3).x <= 3
$X+B[$Integer](3).X <- $Integer
$X+B[$Integer].X <- $Integer

# $X <- [\.T] -> [\.x]
$X <- [\.T] -> (\.x) => (.x)[
  +()
]
# Or:
$X <- (\.x[\.T])[
  +($equals(3))+true => $Integer;
  +(\.-) => $Boolean;
]
# (which expands to):
$X <- [\.T] -> (\.x <- .T) => (.x) => [
  +($equals(\.-, 3))+true => $Integer;
  +(\.-) => $Boolean;
]
# Where:
$Integer <- -[3]
$X[$Integer](3) <- $Integer
$X[$Integer](2) <- $Boolean

$equatable <= [\.X, \.Y] -> (
  \.equal[(\.x[.X], \.y[.Y]) => $Boolean],
)

### new stuff:
# a *type* is a specification which may *match* a *value*. "matching" refers to type resolution.
# the [(...)] operator produces such a specification from a subset of the struct literal syntax:
$A <- [(.x[$Integer])]

# define a typeclass:
$Equatable[\.X\.Y] <- [(
  # \.equal <- [(\.x[.X]\.y[.Y]) => [(.cmp[$Boolean])]],
  # [(...)] does not accept <= or =>, so we use / instead
  \.equal[(\.x[.X]\.y[.Y] / .cmp[$Boolean])]
)]

# define an instance of the typeclass (not assigned to any named value, but now available in
# implicit search scope):
# $Equatable[.X[$Integer]/.Y[$Integer]] -> (
$Equatable[$Integer, $Integer] -> (
  .equal(\.x[$Integer]\.y[$Integer] => .cmp($primitive$integer-equals(.x/.y)))
)

# value assertion:
3 <!= 3
# type assertion:
$Integer <!- [(3)]

# define a method which accepts an implicit instance of the typeclass:
$equals[\.X\.Y] <=
  # (\~inst[$Equatable[.X/.Y]], \.a[.X], \.b[.Y]}) =>
  # (\.inst <~ [$Equatable[.X, .Y]]) =>
  (\~inst[$Equatable[.X/.Y]]) =>
  (\.a[.X]\.b[.Y]) =>
    .inst.equal(.a/.b)
    # .inst.equal(.a, .b)[(.cmp[$Boolean])]

# semicolon/slash/colon used to sequence operations:
# (the $Numeric[.T] instance is added in scope but without any name)
$f[\.T] <= \~inst[$Numeric[.T]] => \.x[.T]\.y[.T] => [.T];(
# $f[\.T -> \~$Numeric[.T]] <= \.x[.T]\.y[.T] => [.T];(
# "[x]y" or "(x)y" is necessary to use any variables bound in "x" in "y"; otherwise variables are
# bound in parallel (?)
# $f[[\.T]\~$Numeric[.T]] <= \.x[.T]\.y[.T] => [.T];(
  ; .z <= $plus(.x/.y) /
    .a <= $times(.x/.y)
  ; .b <= $pow(.z/.a)
  # : .ret($xor(.z/.b))
  : $xor(.z/.b)
)

$g(\.x[$Integer]) <= [$Boolean](
  # \+0 => $boolean+true
  \+0 => +true
  \+1 => +false
  # "$equals(...)+!true" here is asserting that the result of $equals(...) is the +true variant. If
  # that assertion fails, the case fails, and we move on to the next one.
  \+($mod(\.-, 3) => $equals(\.-, 0)+!true) => +true
  # use "+!" as shorthand for "+!true"? maybe introduce a default enum case and use it for this?
  \+($mod(\.-, 3) => $equals(\.-, 0)+!) => +true
  # "x$f(...)" is converted into "$f(x, ...)":
  \+($mod(\.-, 3)$equals(0)+!) => +true
  # we can take this to the extreme:
  \+(\.-$mod(3)$equals(0)+!) => +true
  # now we look just as good as this fake infix code!
  # \+(\.- % 3 == 0) => +true
  # catch-all case:
  \+- => +false
)

# NB: right now, +cases with an initial capital are not allowed, as it does not seem to correspond
# to the values/types analogy we have for ".[aA]" and "$[aA]".
$Boolean <- [(
  \+true
  \+false
)]


# implicit rule graph search (for conversions .T => $Integer):
$f[\.T](\.x[.T]) <= \~convert[(\.x[.T] / .y[$Integer])] => [$Integer](convert(.x)$plus(3))
# using shorthand for directly converting arguments:
$f[\.T](\.x[.T] ~> .y[$Integer]) <= [$Integer](.y$plus(3))

$C <- [(
  .x
)]
###

$equals <= [\.X, \.Y] -> {\.inst <~ $equatable[.X, .Y]} => (\.x, \.y) => .inst.equal(.x(.x), .y(.y))
# Or:
$equals <= [\.X, \.Y] -> {\.inst[$equatable[.X, .Y]]} => (\.a[.X], \.b[.Y]) => .inst.equal(.a, .b)
# Where:
-[$equals] <- [\.X, \.Y] -> [(\.inst[$equatable[.X, .Y]], \.a[.X], \.b[.Y]) => $Boolean]
# Or:
-[$equals][\.X, \.Y] -> [(\.inst[$equatable[.X, .Y]], \.a[.X], \.b[.Y]) => $Boolean]

# I think this method might need to be provided by the runtime!!
$type-equals <= [\.X, \.Y] -> (...)
-[$type-equals] <- [[\.X, \.Y] => $Boolean]

# [\.X][...] expands to \.X -> .X -> [...].
$F <- [\.X][
  +($type-equals[.Y[$Integer]])+true => $Integer;
  +[\._] -> ._;          # <The inside of +[...] inside a match must be a type function
#                           accepting a single positional argument.>
]
$F[$Integer] <- $Integer
# -[(+a)] gets the type of an anonymous enum literal with a single static case +a.
$F[-[(\+a)]] <- -[(\+a)]

$x <= (
  \+a(\.a[$Integer]),
  \+b(\.x[\.X])
)
# This assignment still compiles, because $x was previously defined as an enum with a compatible
# field!
$x <= (\+a(\.a[$Integer]))
# This assignment also succeeds (and does nothing at runtime)!
$x <= (\+a(\.a))
# This assignment *FAILS* (at type resolution time) because \.a[$Boolean] is not compatible with the
# previously-declared \.a[$Integer] field of $x+a.
$x <= (\+a(\.a[$Boolean]))
# This assignment also *FAILS* (at type resolution time) because \+c was not previously declared:
$x <= (\+c)

$f <= (
  \.a[$Integer],
  \.b,
)
$f <= (\.a, \.b)
-[$f] <- [\.a => \.b => -[(\.a, \.b)]]
# The type of a statically-known literal includes the literal field values.
-[$f(3, 4)] <- -[(\.a(3), \.b(4))]
-[$f(3)] <- [\.b => -[(\.a(3), \.b)]

# `\.x => t` expands to `\.x => (.x(.x)) => t`

$x <= (\.x[\.X])
$x <- [[\.X] -> (\.x) => -[(\.x <- .X])]]

# $X is a "type struct", or a struct in type context.
$X <- [\.x[\.X]]
$X <- \.X -> [\.x[.X]]
# We can take the type of $X itself -- it's a function returning a type.
# Note that -[\[\.x]] takes the type of a anonymous type struct!!!
# NB: We can probably nest -[-[-[...]]] endlessly to get more type levels!!!
-[$X] <- [[\.X] -> (\.x) => -[[\.x(.x)]]]
# NB: Proposing =(...) as an inverse of -[...]!
=(-[$f]) <= $f
# Because (applying the inverse to both sides):
-[$f] <- -[$f]
# They can be alternated:
-[=(-[$f])] <- -[$f]

$optional[\.T] <= (
  \+some(\.t[.T]),
  \+none,
)

$streamable[\.StreamType, \.ElementType] <= (
  \.get-next[\.object[.StreamType]
            => $optional[-[(\.new-obj[.StreamType], \.stream-element[.ElementType])]]],
)

$list[\.Element] <= (
  \+none,
  \+cons(\.car[.Element], \.cdr[.Self]),
)
$list[\.El] <= (
  \+none,
  \+cons(\.car[.El], \.cdr[.El]),
)

$stream-ints <~ \.input[$list[$Integer]] => $streamable[-[.input], $Integer](
  .get-next(\.object => +(
              +none => +none,
              +cons.(\.car, \.cdr) => +some(.t(.new-obj(.cdr), .stream-element(.car))),
              # Or:
              +cons => +some(.t(.new-obj(\.cdr), .stream-element(\.car))),
)))

$countable[\.T] <= (
  \.zero[.T],
  \.plus[(\.lhs[.T], \.rhs[.T]) => .T],
)
-[$countable[\.T]] <- -[(\.zero[.T], \.plus[(\.lhs[.T], \.rhs[.T]) => .T])]

$sum-stream <= [\.StreamType, \.ElementType]
  -> (\.counter <~ $countable[.ElementType])
  => (\.streamer <~ $streamable[.StreamType, .ElementType])
  => (\.input[.StreamType])
  => {\.&cur-state(.input), \.&cur-sum(.counter.zero), \.&cur[.ElementType]}
  => {
    loop {
      .streamer.get-next(.&cur-state) => +(
        +none => break,
        +some.(.t(\.new-obj, \.stream-element)) => {
          .&cur-state <= .new-obj;
          .&cur <= .stream-element;
          .&cur-sum <= .counter.plus(.&cur-sum, .&cur);
        },
      );
    };
    ^&cur-sum
  }
# Therefore:
-[$sum-stream] <- [\.StreamType, \.ElementType] -> [
  (\.counter[$countable[.ElementType]],
   \.streamer[$streamable[.StreamType, .ElementType]],
   \.input[.StreamType])
  => .ElementType]

$F <- [\.x]
=($F) <= =([\.x])

\.seq[.El...] => ...
# expands to:
\.seq => (\.streamer <~ $streamable[-[.seq], .El]) => ...

(.seq)...{
  .&ret <= .fun(.&ret, .-);
}
# expands to:
{\.&cur-state(.seq)}            # <{\.&ret(.init)} was already declared>
  => loop {
    .streamer.get-next(.&cur-state) => +(
      +none => break,
      +some.(.t(\.new-obj, \.stream-element)) => {
        .&cur-state <= .new-obj;
        (.- <= .stream-element) => {
          .&ret <= .fun(.&ret, .-);
        };
      },
    )
}

@stream[\.El] <= +{
  +{\.@seq-arg{$ScopedParamDecl+value} ;
    "@stream" ;
    } ~> @(
    =(.@seq-arg) => (\.- <~ $streamable[-[.@seq-arg], .El])
  ),
  +{\.@seq-arg{$ScopedParamDecl+value} ;
    "@stream" ;
    \.@streamer-arg{$ScopedParamDecl+value}
   } ~> @(
    =(.@seq-arg) => (=(.@streamer-arg) <~ $streamable[-[.@seq-arg], .El])
  ),
}

\.seq <=

\.seq[@...[.El]] =>
\.seq[@stream[.El]] =>


@...(\.seq[.El])

\.seq@...[.El] => ???
(\.seq) <= @...[.El] => ???

\.seq(@stream[.El]) => ...
\.seq => (\.- <~ $streamable[-[.seq], .El])

\.seq(\.s <~ $streamable[-[.seq], .El])
\.seq <= (\.s <~ $streamable[-[.seq], .El])

\.seq@stream[.El](\.streamer)

\.seq <= @stream[.El](\.streamer)

(\.seq)@stream[.El]

\.seq <= @stream[.El]
\.seq(@stream[.El](\.streamer))

\.seq[@stream[.El](\.s)] => ...
(\.seq <- @stream[.El](\.s)) => ...
\.seq => (\.streamer <~ $streamable[-[.seq], .El]) => ...
\.seq => (\.s <~ $streamable[-[.seq], .El]) => ...

\.seq[@...[.El]] # <implies that>
\.seq <- [\.T] # <such that there exists a \.s such that the below holds:>
\.s <~ $streamable[.T, .El] # <if there is not *exactly* one such \.s, COMPILE ERROR!!>

\.seq[.El]@...
\.seq[.El@...]

\.seq ~> $streamable[-[.seq], .El]

# \.seq[.El...] => (\.s <~ $streamable[-[.seq], .El]) => ...

-[$arr[\.T]] <- [+($Size) => .T]
# $arr[\.T] <= () x

$empty-arr[\.T] <= (\.n[$Size] => $native-array[.T](.n))

$some-arr <= $empty-arr[$Integer](5)
$some-arr+(0) <= 3

# \.seq[.El...],
# \.seq[.El@...],
# \.seq(@[.El...])
# \.seq(@[.El]...)
# \.seq@[.El...]
# \.@seq[.El...]
$reduce[\.Ret, \.El] <= +(
  +(\.init[.Ret], \.seq <~ $streamable[-[.seq], .El], \.fun[(\.acc[.Ret], \.cur[.El]) => .Ret])
    => {\.&ret(.init)} ~> {     # <\.&ret is now a *mutable* scoped local variable with the value of
#                                  .init>
      .seq@...{
        .&ret <= .fun(.&ret, .-); # <.- is implicitly bound in the body of every ...{} block>
      };
    } ~> .&ret
)



# Declaration of infix @... macro:
# The @+{@-(...)} means that we will describe a macro which matches a value expression `...`.
@+{@-(\.input <~ (\.streamer <- $streamable[-[.input], \.El]))}
  @> @...
  <@ @{
    # +{} in place context will parse!
    @+{\.@brace-body[$BracedSemicolonSequence]}
      # But @+{} as a source expression will quote!
      @> @+{
        # Declare .&- as a mutable scoped variable!
        {\.&-[.El],
         # Declare .&cur-state to have the type of whatever UNQUOTED .input is, at type time!!
         \.&cur-state[-[@-{.input}]]} => {
              loop {
                # ABSOLUTE GENIUS: USE @-{} to unquote!!!!!!!!
                @-(.streamer).get-next(.&cur-state) => +(
                  +none => break,
                  +some.((\.new-obj, \.stream-element)) => {
                    \.&cur-state <= .new-obj;
                    \.&- <= .stream-element;
                    @-{
                      # Bind \.- to the value of \.&-.
                      # NB: Variables bound in the containing QUOTED +{} section are NOT visible to
                      # the body of an UNQUOTED -{} section without an explicit binding here!!!
                      (\.- <= @+(.&-)) => @+{.brace-body}
                      # TODO: what does +{.&-} mean? Is it necessary to dereference a variable from
                      # a quoted context? It's unclear if this syntax distinction is
                      # necessary. However, it's not unreasonable that it could be *un*ambiguous!
                    };
                  }
                )
              }
            }

      }}
# NB: $BracedSemicolonSequence is presumably a method provided by an AST library.

-[$boolean-and] <- [(\.x[$Boolean], \.y[$Boolean]) => $Boolean]


-[$and] <- [(\.input[$Boolean...]) => $Boolean]
$and <= (\.input[$Boolean...]) => $reduce(+true, .input, $boolean-and)
# Or:
$and <= $reduce($Boolean+true, \.input[$Boolean...], $boolean-and)

$f <= (\.x, \.y) => (
  +(.x, .y)
    +($and($equals(.x, 4), $equals(.y, 4)))
    +true => 3;
  +? => 4;
)

# To evaluate a type function with non-type arguments, place the non-type arguments in a second
# argument list using parentheses.
$X[$Integer](3).x <= 3
$X[.X[$Integer]](.x(3)).x <= 3

-[$X[.X[$Integer]]] <- [(\.x) => -[[\.x[$Integer]]]]
$X[.X[$Integer]] <- [(\.x) => [\.x[$Integer]]]

$X <- [\.x]
$X <- [\.x => [\.x <= .x]]

$X(.x(3)).x <= 3

$x <= \+(
  \+a,                        # <$x+a is a value, an enum instance>
  \+b(\.x[\.X]),              # <$x+b is a value, a function returning a struct with value field .x>
)
# Does <- -[(\+a, ...)] make any sense? I think so: it's clearly the type of an object with static
# case +a.
-[$x] <- -[\+(
  \+a,
  \+b(\.x[\.X]),
)]
-[$x+a] <- -[\+(\+a)]
-[$x+b] <- [\.X -> \.x[.X] => -[$x+b]] # <it would be great if this could successfully typecheck!!>

$x+a <= $x[+a]                  # <these are the same expression>
$x+b[$Integer](3) <= $x+b[$Integer](3)
$x+b[$Integer](3).x <= 3
$x+b[$Integer].X <= $x+b[$Integer](3)

# Declared with <= because it returns a value.
$type-equals <= \[.X, .Y] -> (
  +[...] -> $Boolean+true
)

# (n - 1) All top-level type and value variable binding statements are evaluated in the order they
#         are declared in the source file, on top of any existing bindings for that variable. If a
#         top-level variable binding statement is processed which conflicts with any previous
#         binding statements for that same identifier and kind.
# (n) ??? may stand in for any type or value expression, and will raise a fatal error if evaluated.
