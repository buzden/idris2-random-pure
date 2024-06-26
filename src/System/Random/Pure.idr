module System.Random.Pure

import Control.Monad.State.Interface

import Data.Bits
import Data.Fin

%default total

--------------------------------
--- Interface for seed types ---
--------------------------------

||| Interface of algorithms of pseudo-random generation of `Bits64` values using a seed type `g`.
|||
||| Those `Bits64` values must be generated uniformly.
||| Splitting must given independent seeds.
public export
interface RandomGen g where
  next    : g -> (g, Bits64)
  split   : g -> (g, g)
  variant : Nat -> g -> g

||| Interface for getting the starting seed
public export
interface RandomGen g => CanInitSeed g m | m where
  initSeed : m g

export
ConstSeed : Applicative m => RandomGen g => g -> CanInitSeed g m
ConstSeed seed = S where [S] CanInitSeed g m where initSeed = pure seed

--------------------------------------------------------
--- Types for which values can be randomly generated ---
--------------------------------------------------------

||| Interface for generation of values of particular types using any appropriate `RandomGen` algorithm.
|||
||| Contains a function for generation of uniform value sitting in the given range,
||| and a function for generation of any value of the type.
||| For inifinite types, when no range is given, implementation determines actual used range by itself.
public export
interface Random a where
  randomR : RandomGen g => (a, a) -> g -> (g, a)
  random  : RandomGen g => g -> (g, a)

public export %inline
randomFor : RandomGen g => (0 a : _) -> Random a => g -> (g, a)
randomFor _ = random

export
randomR' : Random a => RandomGen g => MonadState g m => (a, a) -> m a
randomR' bounds = let (g, x) = randomR bounds !get in put g $> x

export
random' : Random a => RandomGen g => MonadState g m => m a
random' = let (g, x) = random !get in put g $> x

public export %inline
randomFor' : RandomGen g => MonadState g m => (0 a : _) -> Random a => m a
randomFor' _ = random'

export
randomThru : (0 thru : _) -> Random thru => (from : thru -> a) -> (to : a -> thru) -> Random a
randomThru thru from to = RandomThru where
  [RandomThru] Random a where
    randomR = map from .: randomR {a=thru} . mapHom to
    random  = map from . random {a=thru}

--- Patricular implementations ---

maxMask : Bits64 -> Bits64
maxMask max = case countLeadingZeros max of
                Nothing  => zeroBits
                Just off => oneBits `shiftR` off
  where
    countLeadingZeros : Bits64 -> Maybe $ Fin 64
    countLeadingZeros x = go 63 where
      go : Fin 64 -> Maybe $ Fin 64
      go i = if testBit x i then Just $ complement i else case i of
               FZ    => Nothing
               FS i' => go $ assert_smaller i $ weaken i'

export
Random Bits64 where
  random = next
  randomR (lo, hi) = do
    let (lo, hi) = (lo `min` hi, lo `max` hi)
    map (lo +) . nextMax (hi - lo) where

      nextMax : (max : Bits64) -> g -> (g, Bits64)
      nextMax max = assert_total go where

        mask : Bits64
        mask = maxMask max

        covering
        go : g -> (g, Bits64)
        go g = do
          let (g', x) = next g
              x' = x .&. mask
          if x' > max then go g' else (g', x')

export %hint
RandomInt64 : Random Int64
RandomInt64 = randomThru Bits64 (fromInteger . (\x => x - diff) . cast) (fromInteger . (+ diff) . cast) where
  diff : Integer
  diff = 1 `shiftL` 63

export %hint
RandomBits32 : Random Bits32
RandomBits32 = randomThru Bits64 cast cast

export %hint
RandomInt32 : Random Int32
RandomInt32 = randomThru Int64 cast cast

export %hint
RandomBits16 : Random Bits16
RandomBits16 = randomThru Bits64 cast cast

export %hint
RandomInt16 : Random Int16
RandomInt16 = randomThru Int64 cast cast

export %hint
RandomBits8 : Random Bits8
RandomBits8 = randomThru Bits64 cast cast

export %hint
RandomInt8 : Random Int8
RandomInt8 = randomThru Int64 cast cast

export %hint
RandomInt : Random Int
RandomInt = randomThru Int64 cast cast

two64 : Integer
two64 = 1 `shiftL` 64

export
Random Integer where
  random = randomR (-two64, two64) -- This is more or less arbitrary anyway
  randomR (lo, hi) = do
    let (lo, hi) = (lo `min` hi, lo `max` hi)
    map (lo +) . nextMax (hi - lo) where

      nextMax : Integer -> g -> (g, Integer)
      nextMax max = do
        let goMask : Nat -> Integer -> (Nat, Bits64)
            goMask n x = if x < two64
              then (n, maxMask $ cast x)
              else goMask (S n) (assert_smaller x $ x `shiftR` 64)

        let (restDigits, leadMask) = goMask 0 max
        let generate : g -> (g, Integer)
            generate g0 = do
              let (g', x) = next g0
                  x' = x .&. leadMask
              go (cast x') restDigits g'
              where
                go : Integer -> Nat -> g -> (g, Integer)
                go acc Z     g = (g, acc)
                go acc (S n) g =
                    let (g', x) = next g
                    in go (acc * two64 + cast x) n g'

        let covering loop : g -> (g, Integer)
            loop g = do
              let (g', x) = generate g
              if x > max then loop g' else (g', x)

        assert_total loop

export %hint
RandomNat : Random Nat
RandomNat = randomThru Integer (cast . abs) cast

export
Random Double where
  random = map (\w64 => cast (w64 `shiftR` 11) * doubleULP) . next where
    doubleULP : Double
    doubleULP =  1.0 / cast {from=Bits64} (1 `shiftL` 53)
  randomR (lo, hi) =
    if lo == hi then map (const lo) . next
    else if lo == 1/0 || lo == -1/0 || hi == 1/0 || hi == -1/0 then map (const $ lo + hi) . next
    else map (\x => x * lo + (1 - x) * hi) . random

export
Random Unit where
  randomR ((), ()) gen = map (const ()) $ next gen
  random gen = map (const ()) $ next gen

export
{n : Nat} -> Random (Fin $ S n) where
  random  = map (\x => natToFinLt x @{believe_me Oh}) . randomR (0, n)
  randomR = map (\x => natToFinLt x @{believe_me Oh}) .: randomR . mapHom finToNat

export %hint
RandomBool : Random Bool
RandomBool = randomThru Bits64 (\x => testBit x 0) (\b => if b then 1 else 0)

export
Random Char where
  random  = map cast . randomR {a=Bits64} (0, 0xfffff+0xffff+1)
  randomR = map cast .: randomR {a=Bits64} . mapHom cast
