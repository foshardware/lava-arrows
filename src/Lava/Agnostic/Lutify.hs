{-# LANGUAGE Arrows, TupleSections #-}

module Lava.Agnostic.Lutify where

import Control.Arrow.Operations
import Data.Bits
import Lava.Diagnostics
import Lava.Types
import Lava.Wire
import Lava.Agnostic.Gates
import Lava.Agnostic.Arrow
import Lava.Agnostic.ExecCircuit
import Prelude hiding (and, or)


lutify :: (Arrow a, ArrowApply a, Wires i, Wires o) => AgnosticA a i o -> AgnosticA a i o
lutify block = proc xs -> do
  let len = length $ untie xs
  genericLUTs len (booleanTables block len) -<< xs

genericLUTs :: (Arrow a, ArrowApply a, Wires i, Wires o) => Int -> [[Bool]] -> AgnosticA a i o
genericLUTs n bools = proc xs -> tie . reverse . snd ^<< lut n bools -<< (untie xs, [])

lut :: (Arrow a, ArrowApply a) => Int -> [[Bool]] -> AgnosticA a ([Wire], [Wire]) ([Wire], [Wire])
lut 0 ([b] : _) = (, []) . pure ^<< f b <<^ const ()
  where f True = vcc; f False = gnd
lut _ ([ ] : _) = arr id
lut n op = proc (k : addr, rs) -> do
  k' <- inv -< k
  r0 <- and <<< (k',) . head . snd ^<< lut (n-1) x -<< (addr, [])
  r1 <- and <<< (k ,) . head . snd ^<< lut (n-1) y -<< (addr, [])
  r <- or -< (r0, r1)
  lut n $ tail <$> op -<< (k : addr, r : rs)
  where (x, y) = splitAt (2^(n-1)) $ take 1 <$> op

booleanTables :: (Arrow a, ArrowApply a, Wires i, Wires o) => AgnosticA a i o -> Int -> [[Bool]]
booleanTables block n = let wireRepresentation s = wire . testBit s <$> reverse [0..(n-1)] in
  map wireValue . untie . undefined block . tie . wireRepresentation <$> [(0 :: Int)..(2^n-1)]

