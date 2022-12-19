<!-- idris
module README

import System.Random.Pure
import System.Random.Pure.StdGen
-->

# Pure pseudo-random sequence generator

## Interfaces

Library defines two interfaces: `RandomGen` and `Random`.

`RandomGen g` describes data type `g` which can be used as a seed and
algorithm of producing pseudo-random data and the next seed (`next` function).
Also, it allows to split a seed to two independent seeds using `split` function.

`Random a` describes how data of type `a` can be generated being given any `g` implementing `RandomGen g`.
Random values are generated uniformly in a specific range of values.
Function `choose` uses some type-specific range (e.g., for `Bits64` it is range between `0` and maximal value,
for `Double` it is range between `0` and `1`, for `Integer` a symmetrical range between `-x` and `x` for some big value `x`).
Function `chooseR` generated values in the given range.
Both those functions also return new seed `g`.

## Splitmix generator

This library contains one implementation of `RandomGen` named `StdGen` using the splitmix algorithm.

One can create a `StdGen` seed using system entropy with `initStdGen` or statically with `mkStdGen` by given 64-bit number.

## Installation

If you use [`pack`](https://github.com/stefan-hoeck/idris2-pack/) package manager,
it is enough to add `random-pure` to `depends` section
of your `.ipkg`-file, or to call `pack install random-pure` to install it directly.
