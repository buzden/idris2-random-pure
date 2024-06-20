-- This module contains translation from the Haskell code taken from
-- https://hackage.haskell.org/package/splitmix-0.1.0.4/docs/src/System.Random.SplitMix.html
module System.Random.Pure.StdGen

import Data.Bits
import Data.Vect
import public Data.So

import Deriving.Show

import System
import public System.Random.Pure

import Text.PrettyPrint.Bernardy.Core
import Text.PrettyPrint.Bernardy.Interface

%default total
%language ElabReflection

------------------------------------
--- Utilities over `Bits64` type ---
------------------------------------

goldenGamma : Bits64
goldenGamma = 0x9e3779b97f4a7c15

shiftXor : Fin 64 -> Bits64 -> Bits64
shiftXor n w = w `xor` (w `shiftR` n)

shiftXorMultiply : Fin 64 -> Bits64 -> Bits64 -> Bits64
shiftXorMultiply n k w = shiftXor n w * k

mix64, mix64v13 : Bits64 -> Bits64
mix64    = shiftXor 33 . shiftXorMultiply 33 0xc4ceb9fe1a85ec53 . shiftXorMultiply 33 0xff51afd7ed558ccd
mix64v13 = shiftXor 31 . shiftXorMultiply 27 0x94d049bb133111eb . shiftXorMultiply 30 0xbf58476d1ce4e5b9

mixGamma : Bits64 -> Bits64
mixGamma z0 = do
  let z1 = mix64v13 z0 .|. 1             -- force to be odd
      n  = popCount $ z1 `xor` (z1 `shiftR` 1)
  -- see: http://www.pcg-random.org/posts/bugs-in-splitmix.html
  -- let's trust the text of the paper, not the code.
  if n >= 24 then z1 else z1 `xor` 0xaaaaaaaaaaaaaaaa

-------------------------------------------------
--- The `StdGen` type and its implementations ---
-------------------------------------------------

export
data StdGen = MkStdGen Bits64 Bits64

export
Eq StdGen where
  MkStdGen seed gamma == MkStdGen seed' gamma' = seed == seed' && gamma == gamma'

export
RandomGen StdGen where

  next $ MkStdGen seed gamma = do
    let seed' = seed + gamma
    (MkStdGen seed' gamma, mix64 seed')

  split $ MkStdGen seed gamma = do
    let seed'  = seed + gamma
        seed'' = seed' + gamma
    (MkStdGen seed'' gamma, MkStdGen (mix64 seed') (mixGamma seed''))

  -- idea taken from https://github.com/qfpl/hedgehog-fn/blob/2621548943ffa46c98f430cca6beeb9025ea3127/src/Hedgehog/Function/Internal.hs#L73
  variant v $ MkStdGen seed gamma = MkStdGen (seed + cast v) gamma

export
Show StdGen where
  -- since the constructor is not public, creation of literally any value
  -- can be done through a smart-constructor, so, showing it through it.
  showPrec d $ MkStdGen s g = showCon d "rawStdGen" $ showArg s ++ showArg g

export
Pretty StdGen where
  prettyPrec d $ MkStdGen s g = prettyCon d "rawStdGen" [prettyArg s, prettyArg g]

--- Creation of `StdGen` values ---

export %inline
rawStdGen : (seed, gamma : Bits64) -> (0 gammaIsOdd : So $ testBit gamma 0) => StdGen
rawStdGen s g = MkStdGen s g

||| The most preferrable, but not invertible way for creation specific seed values
export
mkStdGen : Bits64 -> StdGen
mkStdGen s = MkStdGen (mix64 s) (mixGamma (s + goldenGamma))

export
someStdGen : StdGen
someStdGen = rawStdGen 23462 254334222987

||| The way of creation of a seed value based on some system entropy
export
initStdGen : HasIO io => io StdGen
initStdGen = pure $ mkStdGen $ cast !time `xor` cast !getPID

export %defaulthint
EntropySeed : HasIO m => CanInitSeed StdGen m
EntropySeed = E where [E] CanInitSeed StdGen m where initSeed = initStdGen
