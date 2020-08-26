language
========

Description of the ["funnel"](../README.md) *language* which is intended to extend the POSIX shell syntax to make scripts more maintainable, less brittle, and more fun to write! See [Extending the Language](../README.md#extending-the-language) for more context on the motivation for a separate language.

# Goals of this Language
1. Don't pull into the language what could be done by `awk` or `sed`!
2. Focus on *error-prone* and/or *lengthy/confusing* idioms which are necessary with bash.
3. Allow features which are recognizable from other languages (e.g. types).
  - But don't necessarily just use things verbatim. Adapt it to bash!
4. Remember that this could actually matter to people. Don't undersell it.

# Potential Features
## Typing


# Parsing
One of the ways to determine whether a language is capable of bootstrapping the compiler itself (see [Goals](../README.md#goals)) is to see how painful it is to write a parser in it!

*unstructured thoughts:*
- It would be **super** cool to be able to write parsers *using existing bash scripting knowledge*!
  - *see [Goals](../README.md#goals): "empowers people with existing experience in some shell environment"*.
  - Could we extend shell pipelines to cover control flow for a parallel parser?
    - Could we make each "node" in the parse tree its own bash command (e.g. a `sed` script, or maybe a `while read -r` loop)?
- Can we extend this capability to ensure that the language we design can also make writing *command line parsers* easier and more robust?
  - *see [Prior Art/CoffeeScript](../cli/README.md#cofeescript) for considerations regarding robust argument parsing in real command-line tools!*
