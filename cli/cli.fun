# importing symbols!!
# NB: 'import' is provided by the runtime!
${:bc, $printf-specs} <= import('common')

# defining functions (different syntax than variables and productions!!!)
# wrapping shell functions (redefining :bc from above):
# NB: 'call-cmd' is provided by the runtime!
# NB: Only intrinsics can be referenced without any sigil!
# TODO: do all intrinsics only accept positional/keyword args (instead of being a production too?)?
:bc <= $arg => call-cmd(bc ; "$arg")

# TODO: are productions the same as functions??? (YES!!! SEE THIS!!!)
# NB: '$a: integer' is the same as '$a <= :integer'!
# '<match-expr> = <match-expr type="lazy-eval" [<match-expr> => <eval-body>]> | <match-expr>'
# NB: USING THE BACKSLASH AS IMPLICIT XARGS IS...I N C R E D I B L E!!!!!
:calculate = ($a <= :integer ; $op ; $b: integer) => \:bc("$a$op$b") \
  | :integer
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
:integer <= /[-+]?[0-9]+/ | (
  # NB: | will have lower precedence than ;, and ',' will have lower precedence than anything (?).
  # NB: first usage of '+<case-name>($params...)? <= <expr>,' (bind case) and '<variable-reference> => <eval-body>' (evaluate body after local variable is bound)!
  # NB: `calculate()` is declared to return `:integer`, so this type-checks ok!
  # NB: the backslash means calling the production with
  +plus($x: integer) <= $_ => \:calculate($_ ; '+' ; $x),
  # NB: The final trailing comma is non-optional!!!
  +minus($x: integer) <= $_ => \:calculate($_ ; '-' ; $x),
)

# Note with the above that:
# 1. '($_ => calculate(...))' is a <match-expr>.
# 2. '$_' is a <bind-expr> which returns the input unmodified.
# 3. <bind-expr> \subsetneq <match-expr>
# 4. '($_ => calculate(...))' can be abstracted into '<bind-expr> => <function-call>'.

# Modifying existing variables (associative array):
$printf-specs+integer <= 'd'
# equivalent:
# NB: first usage of ':<production-name> => <eval-body>'.
# This is probably better described as '<bind-expr> => <eval-body>'.
# TODO: do this in a way such that only functions in this file are affected by this.
# TOOD: make this syntax usable such that e.g. $printf-specs+integer can be *temporarily overridden within a production!!!*
$printf-specs => (
  # This nested initialization is like groovy!
  +integer <= 'd',
)

# The $six variable is defined in whatever the current scope is, and implicitly typed :integer!
$six <= '4':integer+plus('3')

:port_num <= /[0-9]/:integer
# equivalent to above:
:port_num <= /[0-9]+/ | :integer
# equivalent to above (demonstrating how destructuring/typing/everything goes together so naturally):
# TODO: is '<=' the inverse of '|'?
:port_num <= :integer <= /[0-9]+/
# equivalent to above
function port_num() {
  sed -n -E -e '/^[0-9]+$/ p' | :integer # Where `:integer` is from the prelude.
}

# the following line is in "match syntax"
:port = ':' ; :port_num
# equivalent to above, but in "shell syntax":
function port() {
  sed -n -E -e 's#^:(.*)$#\1#gp' | port_num
}

:file_or_https_scheme <= (
  # Note the clever (unique?) usage of the comma here! These parentheses are optional!
  # Because the comma is only used here, it can have super low precedence.
  +was_file <= /file/,
  +was_https <= /https/,
)
# equivalent to above, but in "shell syntax":
function __generated_method_1() {
  local -r ctx="${1}+was_file"
  sed -n -E -e '/^file$/ p' | while read -r __generated_arg_1; do
    # NB: This is a command for an interpreter for a (stack machine?)!
    echo "${ctx}=${__generated_arg_1}"
  done
}
function __generated_method_2() {
  local -r ctx="${1}+was_https"
  sed -n -E -e '/^https$/ p' | while read -r __generated_arg_2; do
    echo "${ctx}=${__generated_arg_2}"
  done
}
function file_or_https_scheme() {
  local -r ctx="$1:file_or_https_scheme<$(uuid)>" # Function `uuid` is from the runtime.
  tee >(__generated_method_1 "$ctx") | __generated_method_2 "$ctx"
}

# NB: second usage of '<variable-reference> => <eval-body>', first usage of '+<case-name>($params...)? => <expr>,'.
:file_or_https_url <= $validated_schema <= :url$scheme:file_or_https_scheme ; $host <= /[^\/]+/ ; $some_port_num <= $validated_schema => (
  +was_file => :port:port_num,
  +was_https => :port:port_num ? 443,
) ; ('/' ; ...)?



# code?
${$schema <= $validated_schema, $port_num <= $some_port_num} <= $1:file_or_https_scheme
