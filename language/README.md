language
========

Description of the ["funnel"](../README.md) *language* which is intended to extend the POSIX shell syntax to make scripts more maintainable, less brittle, and more fun to write! See [Extending the Language](../README.md#extending-the-language) for more context on the motivation for a separate language.

# Parsing
One of the ways to determine whether a language is capable of bootstrapping the compiler itself (see [Goals](../README.md#goals)) is to see how painful it is to write a parser in it!

*unstructured thoughts:*
- It would be **super** cool to be able to write parsers *using existing bash scripting knowledge*!
  - *see [Goals](../README.md#goals): "empowers people with existing experience in some shell environment"*.
  - Could we extend shell pipelines to cover control flow for a parallel parser?
    - Could we make each "node" in the parse tree its own bash command (e.g. a `sed` script, or maybe a `while read -r` loop)?
