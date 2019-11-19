# -*- mode: zsh -*-

# TODO:
# (1) Consider whether this can be implemented with common lisp + reader macros (or something???)!
# (2) Consider whether simultaneous-productions can be used directly as a *runtime* for this logic!
#   - Similarly, consider whether then that this language leads to a very direct encoding of
#     simultaneous-productions as a Turing-complete computation mechanism!

# FIXME: Functions shouldn't return values (except MAYBE) -- they can bind values to variables! We
# can perhaps establish a convention of '$_' or something as a single positional arg / single
# positional return arg (everything else is accessed by keyword). This would also mean that
# "function application" and "destructuring" basically become the same.

# For example, a '+mean' method could bind not just the arithmetic mean of a series of integers, but
# could also export the sum of those integers which was used to calculate it!
# This is the basic type signature that will only export the single return value.
:integer...+mean <- :integer
# This signature will export the sum of the values, as well as the number of values in the stream!
# NB: A type signature like this is *required* to export any non-'$_' variable from a function!
# It's also possible to export types from a function by assigning to a ':'-prefixed symbol (instead
# of a '$'-prefixed symbol). More experimentation upcoming!!
:integer...+mean <- ($sum <- :integer ; $num-values <- :integer ; $_ <- :integer)
:integer...+mean <= (
  $sum <= 0 ;
  $num-values <= 0;
  ($sum <= $sum+plus($_) ;
   $num-values <= $num-values+plus(1)
  )... ;
  $_ <= $sum+divide($num-values)
)
# Alternative nested definition, same as above.
:integer... <= (
  +mean => ???
)

$integer-values <- :integer...
$integer-values <= (1 ; 3 ; 5 ; 2 ; 1)

# Example of destructuring to bind the result of a method which has exported non-'$_' variables!
($integer-sum <= $sum ; $num-elements <= $num-values ; $mean <= $_) <= $integer-values+mean
# '!' is still-experimental syntax to force an "assertion" for '?'-typed output (is '?' a type????)!
$integer-sum+equals(12) !
$num-elements+equals(5) !
# This is integer division!
$mean+equals(2) !

# If not destructured by opening a scope (using parentheses, as above), the value of '$_' is assumed
# to be used!
$integer-values+mean+equals(2) !


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
  +($k <= $v)... <= $keys => ($k <= :k ; $v <= \$f($k))...
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
  +($k <= $v)... <= $keys => ($k <= :k ; $v <= \$f($k))...
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
