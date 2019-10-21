# productions

:port_num = /[0-9]/:integer
# equivalent to above:
:port_num = /[0-9]+/ | :integer
# equivalent to above (demonstrating how destructuring/typing/everything goes together so naturally):
:port_num: integer = /[0-9]+/

# the following line is in "match syntax"
:port = /:/ ; :port_num
# equivalent to above, but in "shell syntax":
function port() {
  sed -n -E -e 's#^:(.*)$#\1#gp' | port_num
}

:file_or_https_scheme = (
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

:file_or_https_url = $validated_schema <= :url$scheme:file_or_https_scheme ; $host <= /[^\/]+/ ; $some_port_num <= $validated_schema => (
  +was_file => :port:port_num,
  +was_https => :port:port_num ? 443
) ; (/\/ ; ...)?
