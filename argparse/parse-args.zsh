# Compile an XML (?) specification into a zsh script.
#
# The zsh script will contain a single method which will accept a command line and write to a
# specified associative array variable.

function parse-args {
  local -r xml_spec_file="$1"
  local -r output_file_path="$2"
  local -r parse_method_name="$3"

  generated_parse_method_body="$(???)"

  cat >"$output_file_path" <<EOF
function $parse_method_name() {

}
  EOF
}
