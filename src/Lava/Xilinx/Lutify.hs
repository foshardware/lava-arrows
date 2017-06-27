{-# LANGUAGE Arrows, TupleSections #-}

module Lava.Xilinx.Lutify where

import Control.Arrow.Operations
import Data.Bits
import Lava.Diagnostics
import Lava.Types
import Lava.Wire
import Lava.Xilinx.Gates
import Lava.Xilinx.Arrow
import Lava.Xilinx.ExecCircuit
import Prelude hiding (and, or)


lutify :: (Wires a, Wires b) => Xilinx a b -> Xilinx a b
lutify block = proc xs -> do
  let len = length $ untie xs
  genericLUTs len (booleanTables block len) -<< xs

genericLUTs :: (Wires a, Wires b) => Int -> [[Bool]] -> Xilinx a b
genericLUTs n bools = proc xs -> tie . reverse . snd ^<< lut n bools -<< (untie xs, [])

lut :: Int -> [[Bool]] -> Xilinx ([Wire], [Wire]) ([Wire], [Wire])

lut 0 ([b] : _) = (, []) . pure ^<< f b <<^ const ()
  where f True = vcc; f False = gnd

lut _ ([]:_) = arr id

lut 1 op = proc (xs, rs) -> do
  r <- lut1gate "lutify" f -<< tie xs
  lut 1 $ tail <$> op -<< (xs, r : rs)
  where f b0 = if b0 then (head <$> op) !! 1 else (head <$> op) !! 0

lut 2 op = proc (xs, rs) -> do
  r <- lut2gate "lutify" f -<< tie xs
  lut 2 $ tail <$> op -<< (xs, r : rs)
  where
    f b0 b1 = head
      [ (head <$> op) !! x
      | (x, applies) <-
          [ (0, not b0 && not b1)
          , (1, not b0 && b1)
          , (2, b0 && not b1)
          , (3, b0 && b1)
          ]
      , applies
      ]

lut 3 op = proc (xs, rs) -> do
  r <- lut3gate "lutify" f -<< xs
  lut 3 $ tail <$> op -<< (xs, r : rs)
  where
    f b0 b1 b2 = head
      [ (head <$> op) !! x
      | (x, applies) <-
          [ (0, not b0 && not b1 && not b2)
          , (1, not b0 && not b1 && b2)
          , (2, not b0 && b1 && not b2)
          , (3, not b0 && b1 && b2)
          , (4, b0 && not b1 && not b2)
          , (5, b0 && not b1 && b2)
          , (6, b0 && b1 && not b2)
          , (7, b0 && b1 && b2)
          ]
      , applies
      ]

lut 4 op = proc (xs, rs) -> do
  r <- lut4gate "lutify" f -<< xs
  lut 4 $ tail <$> op -<< (xs, r : rs)
  where
    f b0 b1 b2 b3 = head
      [ (head <$> op) !! x
      | (x, applies) <-
          [ ( 0, not b0 && not b1 && not b2 && not b3)
          , ( 1, not b0 && not b1 && not b2 && b3)
          , ( 2, not b0 && not b1 && b2 && not b3)
          , ( 3, not b0 && not b1 && b2 && b3)
          , ( 4, not b0 && b1 && not b2 && not b3)
          , ( 5, not b0 && b1 && not b2 && b3)
          , ( 6, not b0 && b1 && b2 && not b3)
          , ( 7, not b0 && b1 && b2 && b3)
          , ( 8, b0 && not b1 && not b2 && not b3)
          , ( 9, b0 && not b1 && not b2 && b3)
          , (10, b0 && not b1 && b2 && not b3)
          , (11, b0 && not b1 && b2 && b3)
          , (12, b0 && b1 && not b2 && not b3)
          , (13, b0 && b1 && not b2 && b3)
          , (14, b0 && b1 && b2 && not b3)
          , (15, b0 && b1 && b2 && b3)
          ]
      , applies
      ]

lut n op = proc (k : addr, rs) -> do
  k' <- inv -< k
  r0 <- and <<< (k',) . head . snd ^<< lut (n-1) x -<< (addr, [])
  r1 <- and <<< (k ,) . head . snd ^<< lut (n-1) y -<< (addr, [])
  r <- or -< (r0, r1)
  lut n $ tail <$> op -<< (k : addr, r : rs)
  where (x, y) = splitAt (2^(n-1)) $ take 1 <$> op

booleanTables :: (Wires a, Wires b) => Xilinx a b -> Int -> [[Bool]]
booleanTables block n = let wireRepresentation s = wire . testBit s <$> reverse [0..(n-1)] in
  map wireValue . untie . test Nothing block . tie . wireRepresentation <$> [(0 :: Int)..(2^n-1)]

