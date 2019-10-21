funnel
===========

*The idea behind the name "funnel" is that it should allow writing code at a higher level of abstraction, which all gets compiled down to the lowest common denominator of bash scripting.*

A transpiler, standard library, and self-bootstrapping tool environment to write more portable and maintainable bash scripts.

# Getting Started

*Doesn't work yet -- see [TODO](#TODO)!*

# Motivation

The project owes immense inspiration to [CoffeeScript](https://coffeescript.org), which demonstrated it was possible to write more-complex code with an extended feature set and still work in all browser environments through transpilation, which then inspired the incorporation of those exact features into JavaScript at large.

## Bash Splintering

It seems very unfortunate that "bash" is likely still synonymous with "shell" for many people, only because bash has also splintered in versions and feature sets across environments, and a lot of this may be due to the fact that [macOS won't update its preinstalled version of bash to 4 or higher, due to concerns about the GPLv3 license used for bash 4](https://apple.stackexchange.com/a/197172). This means users writing bash often have to manually write lowest-common-denominator bash scripts (scripts which work on the lowest bash version they need to support) to ensure portability, which tends to make these scripts more difficult to write and maintain.

## Toolchain Bootstrapping

The more obvious error and detriment to portable bash scripting is simply not having the desired tools. For tools that users may have installed by default, there are still e.g. incompatibilities for macOS again (such as `sed` not accepting the `-r` flag). In general, though, versions of many shell tools may also be splintered across Linux distributions, depending on how often distributions update their toolchains and how often users upgrade their OS. This can lead to a tradeoff that developers make between maintaining a complete toolchain on all shell environments, and writing lowest-common-denominator bash again, without being able to use tools such as `sed` or `grep` as expected.

## Avoiding Bash Pitfalls

Separate from toolchains, many bash semantics can tend to confuse users, even experienced ones. Last week I learned that `set -e` doesn't exit on a failed command if it's within the body of a `function`! Many other shells such as [zsh](https://zsh.sourceforge.net) fix issues with e.g. variable declarations, but those other shells are even less likely to be installed by default. [ShellCheck](https://www.shellcheck.net) is often used in codebases to avoid these pitfalls, but custom checks may still have to be written -- the [pants](https://pantsbuild.org) repo required this separate [check for broken `readonly` statements which don't cause `set -e` to fail](https://github.com/pantsbuild/pants/blob/4a19087e42ff05608a997b3b5f372420eaaeeb33/build-support/bin/check_shell.sh#L2). This checking requires effort to maintain and still may be incomplete.

While ShellCheck *can* capture pitfalls and style errors, it seems that the number of pitfalls is so great that we might consider looking at a whitelisting approach instead -- not allowing these pitfalls to be expressable at all, perhaps by writing a new language, which transpiles to lowest-common-denominator bash scripts!

## Extending the Language

One thing that transpilation also allows you to do is insert an arbitrary amount of code before and/or after the compiled script itself. [CoffeeScript](https://coffeescript.org), for example, will monkey-patch some array prototype methods before executing the script, to ensure that its compiled output will be able to rely on those array methods (see [Prelude / Runtime](#prelude--runtime)). In our case, we can consider adding to that prelude a layer which ensures up-to-date versions of not just *familiar* tools like `sed` and `grep`, but also *extremely useful and portable tools* such as [gnu parallel](https://www.gnu.org/software/parallel) (which isn't very well-known, possibly due to not being installed by default (unlike `xargs`, which is less featureful but does some of the same things)).

Also of note is that the CoffeeScript compiler will wrap the output in an anonymous function to ensure it won't pollute the global JavaScript namespace. Analogously, we can also consider introducing a better module system to bash, and perhaps a package manager (?).

# Goals
Provide a shell scripting interface which:
- [ ] transpiles to highly portable bash.
- [ ] ensures modern shell builtins such as `readarray` can be invoked, using polyfills if necessary.
- [ ] self-bootstraps toolchains including reliable versions of command-line programs such as `sed` and `parallel`.
  - [ ] creates self-bootstrapping executables!!!
- [ ] exposes new primitives which increase the power of the language to further reduce erroneous patterns (such as `set -e` not exiting within a `function`).
- [ ] introduces a module system and package manager (?).
- [ ] is powerful enough to bootstrap the compiler
  - [ ] i.e. can it make writing a parser not awful?
- [ ] empowers people with existing experience in some shell environment

## Non-Goals
These shouldn't be considered right now:
- runtime performance (at first).
- breaking compatibility with older bash versions.

# TODO
Right now, the "funnel" language's functionality will be exposed through a single executable `fun`.
- [ ] define command-line tools to control (such as `sed` or `parallel`), and create a method to download them on all supported platforms.
- [ ] define all supported platforms.
- [ ] define a grammar (as close to the [bash grammar](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/V3_chap02.html) as possible).
- [ ] implement the transpiler.
  - [ ] figure out whether/how this language can be smart enough to bootstrap itself (i.e. the compiler is
  written in it)
    - **^!!!^**
  - [ ] begin to consider a module and package system for (portable) bash scripts
    - [ ] want something that will work on existing bash/zsh code (e.g. if you put them in a special
      directory they can be specially required or loaded)?
      - the ["Prelude"/"Runtime"](#prelude--runtime) for this (the shell script code that it loads)
        should have a function that is available to bash and zsh scripts that it loads which allows
        them to load something from the module system with similar ease!
- [ ] consider using any relevant parts of [shellcheck](https://github.com/koalaman/shellcheck)!!

# Language Modes
## GNU / BSD options
Whether to accept command lines using GNU-style (probably long) options, or BSD options (with
different names and some missing functionality).
- *TODO: is this really a thing?*
## bash / zsh output
Whether to generate code for bash or for zsh. **The output of this compiler should be 100%
compatible with code written for the output shell.**

# Code Generation
## Prelude / Runtime
The output of a compile should have some "prelude" or "runtime" which is some script to be evaluated
containing e.g. convenience methods.

# License
[GPL v3 (or any later version)](./LICENSE)
