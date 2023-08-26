# -*- mode: zsh -*-

# a *type* is a specification which may *match* a *value*. "matching" refers to type resolution.
# the [(...)] operator produces such a specification from a subset of the struct literal syntax:
# -<- *assigns* a type from right to left. =<= does the same for values.
$A -<- [(.x[$Integer])]

# define a typeclass:
$Equatable[\.X\.Y] -<- [(
  # .equal <- [(\.x[.X]\.y[.Y]) => [(.cmp[$Boolean])]],
  # .equal[(\.x[.X]\.y[.Y] => .cmp[$Boolean])]
  # "/" happens to be equivalent to "=>" here, but "/" is used to signal that we are defining
  # a type, not describing any value-level function logic:
  # .equal[(\.x[.X]\.y[.Y] / .cmp[$Boolean])]
  .equal[(\.x[.X]\.y[.Y] / .[$Boolean])]
)]

# this is because "\.x" is actually a function accepting .x and returning .x:
[(\.x)] -!- [(\.x => .x)]

# define an instance of the typeclass (not bound to any named value, but now available in
# implicit search scope):
# $Equatable[.X[$Integer]/.Y[$Integer]] -> (
$Equatable[$Integer/$Integer] -> (
  # we have to use the "=>" here, because "/" evaluates in parallel and we need ".x" and ".y" to be
  # bound when evaluating the expression for ".cmp". this is why it is useful to use "/" in type
  # signatures for functions where possible.
  # .equal(\.x[$Integer]\.y[$Integer] => .cmp($integer-equals(.x/.y)))
  .equal(\.x[$Integer]\.y[$Integer] => $integer-equals(.x/.y))
)

# value assertion:
3 =!= 3
# type assertion:
$Integer -!- [(3)]

# define a method which accepts an implicit instance of the typeclass:
$equals[\.X\.Y] =<=
  # (\.inst~[$Equatable[.X/.Y]], \.a[.X], \.b[.Y]}) =>
  # (\.inst <~ [$Equatable[.X, .Y]]) =>
  (\.inst~[$Equatable[.X/.Y]]) =>
  (\.a[.X]\.b[.Y]) =>
    .inst.equal(.a/.b)
    # .inst.equal(.a, .b)[(.cmp[$Boolean])]

# semicolon/slash/colon used to sequence operations:
# (the $Numeric[.T] instance is added in scope but without any name)
# $f[\.T -> \~$Numeric[.T]] <= \.x[.T]\.y[.T] => [.T];(
# "[x]y" or "(x)y" is necessary to use any variables bound in "x" in "y"; otherwise variables are
# bound in parallel (?)
# $f[[\.T]\~$Numeric[.T]] <= \.x[.T]\.y[.T] => [.T];(
$f[\.T] =<= \.inst~[$Numeric[.T]] => \.x[.T]\.y[.T] => [.T];(
  ; .z <= $plus(.x/.y) /
    .a <= $times(.x/.y)
  ; .b <= $pow(.z/.a)
  # : .ret($xor(.z/.b))
  : $xor(.z/.b)
)

$g(\.x[$Integer]) =<= [$Boolean](
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
  \+ => +false
)

# NB: right now, +cases with an initial capital are not allowed, as it does not seem to correspond
# to the values/types analogy we have for ".[aA]" and "$[aA]".
$Boolean -<- [(
  \+true
  \+false
)]

# implicit rule graph search (for conversions .T => $Integer):
$f[\.T](\.x[.T]) =<= \.convert~[(\.x[.T] / .y[$Integer])] => [$Integer](.convert(.x)$plus(3))
# using shorthand for directly converting arguments:
$f[\.T](\.x[.T] ~> .y[$Integer]) =<= [$Integer](.y$plus(3))

# modules and namespaces:
:submodule {
  :inner {
    $x =<= 3
  }
  # submodules can be reopened after first declared:
  :inner {
    $y =<= 4
  }
}
:submodule:inner {
  $z =<= 5
}
# access nested modules via ":"
$submodule:inner:x =!= 3
$submodule:inner:y =!= 4
# the contents of this submodule will be read from the neighboring file "submodule.fun"
:submodule{+file}
# read from an explicit file path
:submodule{+file("some-file.fun")}
# the contents of this submodule will be read from the neighboring directory "submodule", with
# central file name "mod.fun" (like mod.rs)
:submodule{+dir}

# import "$package:function" as "$function"
$$package:$function
# import "$package:function" as "$wow"
$$package:function$wow

# $functions can be composed via mere juxtaposition:
[($f$g$h)] -!- [($h($g($f(\.-))))]

# declare a function $f with an argument .x of type $Nat which has the default value 0:
$f(\.x[$Nat](0)) =<= .x$plus(3)
$f =<= \.x[$Nat](0)$plus(3)

# possible haskell-like function case syntax, but may be hard to implement:
$List[\.T](\.n[$Nat]) -<- [(...)]
$List[\.T](.n(0)) -<- [()]
$List[\.T](.n(1)) -<- [(.x[.T])]
$List[\.T](\.n) -<- [( .x[.T] / .xs[$List[.T](.n$decrement)] )]

# this definition would use only existing mechanics:
$List[\.T](\.n[$Nat]) -<- .n+(
  \+0 => [()]
  \+1 => [(.x[.T])]
  \+ => [( .x[.T] / .xs[$List[.T](.n$decrement)] )]
)

# this is a built-in type for a length-1 array of .T instances:
.T x 1 -!- [( .0[.T] )]
# NB: note that this is the same type you get when returning a 2-tuple of .T instances already!
.T x 2 -!- [( .0[.T] / .1[.T] )]
# this is a type declaration for the built-in "x" operator, which created the above arrays via infix
# invocation. "[(...)]" is similar to idris's "Type", in that it returns a promise to create a type
# specification that can then be used in later type-level operations:
x[\.T](\.n) -<- [(...)]


# Define a javascript-style keyed object, but where "+a" is a unique symbol key and separate from the
# string "a" key:
$hashMap =<= \+(
  +a => 3
  +("a") => 4
  +b => 5
  +("c") => 6
)
