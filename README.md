<!-- idris
module README

import Control.Monad.Error.Interface
import Control.Monad.Random

import Data.Fin
import Data.List

import System.Random.Pure
import System.Random.Pure.StdGen
-->

# Pure pseudo-random sequence generator

## Pure interfaces

Library defines two interfaces for pure random values generation: `RandomGen` and `Random`.

`RandomGen g` describes a data type `g` which can be used as a seed and
an algorithm of producing pseudo-random data and the next seed (`next` function).
Also, it allows to split a seed to two independent seeds using the `split` function.

`Random a` describes how data of type `a` can be generated being given any `g` implementing `RandomGen g`.
Random values are generated uniformly in a specific range of values.
Function `choose` uses some type-specific range (e.g., for `Bits64` it is a range between `0` and maximal value,
for `Double` it is a range between `0` and `1`, for `Integer` a symmetrical range between `-x` and `x` for some big value `x`).
Function `chooseR` generates values in the given range.
Both those functions also return new seed of type `g`.

## SplitMix generator

This library contains one implementation of `RandomGen` named `StdGen` using the SplitMix algorithm.

One can create a `StdGen` seed using system entropy with `initStdGen` or statically with `mkStdGen` by a given 64-bit number.
The first way obviously requires `IO` in some way (using the standard `HasIO` interface),
the latter one is pure and does not require any additional environment.

## Monadic interface & transformer

### Monadic interface

Also, the library defines a `MonadRandom m` interface that can be used in a complex monadic context
to express an ability for the monad `m` to have some random generation facility.

Basically, this interface mirrors all functions from the `RandomGen` interface,
but does this in a monadic way, hiding the new seed value.

Generation functions are called `getRandom` and `getRandomR`.
Example of usage:

```idris
chooseRandomly : MonadError String m =>
                 MonadRandom m =>
                 List a -> m a
chooseRandomly []        = throwError "insufficient variants"
chooseRandomly xs@(_::_) = index' xs <$> getRandom
```

> **Note**
>
> Notice that in the example above it's probably better to use `List1 a` or `Vect (S n) a` instead of `List a`
> and to remove `MonadError` from the signature;
> this example is used mainly for illustration.

There is no direct analogue of the `split` function of `RandonGen` interface,
but there is an ability to run an independent computation, called `independent`.
At this point, the seed would be split, and one half would be used for the independent computation,
the other would be used for the continuation.

### Monad transformer

`MonadRandom` interface can be implemented in many ways.
All you basically need is preserving the current state of some `g`, for which `RandomGen g` is present.
Once you have `MonadState g m` (or just `StateT g m`) and `RandomGen g` in the context, you can implement `MonadRandom m`.

But this approach makes not really pleasant to work with because you have an additional requirement of `RandomGen`,
plus such `MonadState`s or direct `StateT`s can clutter with other states in your monad transformers stack.
That's why, this library also provides the simplest transformer implementing the `MonadRandom` interface.
It is called `RandomT`.

`RandomT` is basically a `StateT` over some unknown `g`.
It requires specifying particular `g` that has `RandomGen g` implementation only in running functions.
Main running functions mimic ones from `StateT` and they are called `runRandomT`, `execRandomT` and `evalRandomT`.
They differ in whether generated random value and/or the final seed of type `g` is/are returned.

Also, there are specialised runners.
One is called `evalRandomRef` and uses the seed value from the given `Ref` and updates it accordingly after the run.
Another is called `evalRandomIO` and it uses `StdGen` described above initialising it with the system entropy,
thus not requiring any additional implementations, except for `HasIO` for the outer monad.

A simplified version of `RandomT` exists, it is called `Rand`.
Basically, it is a `RandomT` applied to the `Identity` monad,
providing simpler running functions `runRandom`, `execRandom` and `evalRandom`, analogous to the `State` type.
Why is it called `Rand`, not `Random`?
Otherwise it would clutter with the interface called `Random`.

## Installation

If you use [`pack`](https://github.com/stefan-hoeck/idris2-pack/) package manager,
it is enough to add `random-pure` to `depends` section
of your `.ipkg`-file, or to call `pack install random-pure` to install it directly.
