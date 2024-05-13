module CheckDistribution

import Data.Bits
import Data.List

import DistrCheckCommon

import System.Random.Pure.StdGen

%default total

L, R : Bits32
L = 2147493648
R = 2147493643

ll, rr : Bits32
ll = L `min` R
rr = L `max` R

cnt : Nat
cnt = S $ cast $ rr - ll

main : IO ()
main = printVerdict !initStdGen (randomR (L, R)) $
         iterateN cnt (+1) ll <&> \n =>
           coverWith (ratio 1 cnt) (== n)
