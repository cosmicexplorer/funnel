bashyscript
===========

- *like "bash y script" en español. also sounds like "[bathyschaphe](https://en.wikipedia.org/wiki/Bathyscaphe)", which is a funny word.*

A little language that compiles to POSIX shell script! With great inspiration from [CoffeeScript](https://coffeescript.org/):

> CoffeeScript is a little language that compiles into JavaScript. Underneath that awkward
> Java-esque patina, JavaScript has always had a gorgeous heart. CoffeeScript is an attempt to
> expose the good parts of JavaScript in a simple way.
>
> The golden rule of CoffeeScript is: "It’s just JavaScript." The code compiles one-to-one into the
> equivalent JS, and there is no interpretation at runtime. You can use any existing JavaScript
> library seamlessly from CoffeeScript (and vice-versa). The compiled output is readable,
> pretty-printed, and tends to run as fast or faster than the equivalent handwritten JavaScript.

In addition to those stated goals, this project also plans to address the myriad incompatibilities
across the diaspora of bash (and zsh, etc) versions installed on every system, by compiling to
lowest-common-denominator POSIX shell script. *This (unstated) goal may also have been underlying
CoffeeScript as well, given the relative slowness with which language features were being added to
the base JavaScript language at CoffeeScript's inception.* It is the intent to make a language which
will work with *every* existing bash/zsh codebase or at least *as many as possible*.

The other component to this goal is providing a myriad of shell tools, either that should be
installed by default but aren't (e.g. [GNU parallel](https://www.gnu.org/software/parallel/),
[ripgrep](https://github.com/burntsushi/ripgrep)), or ones that are, but that always have different
flags across versions and operating systems (`grep`, `sed`, etc). **A goal of this project is to
empower people who have developed experience in some shell environment and accomodate their existing
knowledge to the greatest extent possible**. In the case of e.g. `grep` and `sed`, this language will
allow for either Linux or BSD option sets to be written -- and still compile to
lowest-common-denominator shell script! **Finally, having all of this work to support compatibility
should mean that "bashyscripts" can be run as a self-contained self-bootstrapping script out of the
box on any unix-y OS!**

# Getting Started

*Doesn't work yet -- see [TODO](#TODO)!*

# TODO

- figure out whether/how this language can be smart enough to bootstrap itself (i.e. the compiler is
  written in it)
  - **^!!!^**
- module system?
  - do we want something that will work on existing bash/zsh code (e.g. if you put them in a special
    directory they can be specially required or loaded)?
    - **YES!**
    - the ["Prelude"/"Runtime"](#prelude--runtime) for this (the shell script code that it loads)
      should have a function that is available to bash and zsh scripts that it loads which allows
      them to load something from the module system with similar ease!
- how to set up a self-bootstrapping shell environment with e.g. `parallel` and `ripgrep`, and how
  to package it.
- consider using any relevant parts of [shellcheck](https://github.com/koalaman/shellcheck)!!

# Language Modes
## GNU / BSD options
Whether to output command lines using GNU-style (probably long) options, or BSD options (with
different names and some missing functionality).
## bash / zsh output
Whether to generate code for bash or for zsh. **The output of this compiler should be 100%
compatible with code written for the output shell.**

# Code Generation
## Prelude / Runtime
The output of a compile should have some "prelude" or "runtime" which is some script to be evaluated
containing e.g. convenience methods.

# License
[GPL v3 (or any later version)](./LICENSE)
