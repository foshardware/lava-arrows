{-# LANGUAGE Arrows, TypeOperators #-}

module SevenSegment where

import Lava.Xilinx hiding (and, or)


type (->>) = Xilinx

main = do
  let x = evalNetlist "sevenSegment" topLevelSevenSegment
  putAgnosticVHDL x
  putAgnosticBLIF x

topLevelSevenSegment :: () ->> ()
topLevelSevenSegment =
  outputBitVec "out" (BitVec 6 Downto 0) <<< sevenSegment <<< inputBitVec "in" (BitVec 3 Downto 0)

sevenSegment :: [Wire] ->> [Wire]
sevenSegment = proc [x3,x2,x1,x0] -> let xs = (x3,x2,x1,x0) in do
  a <- gateA -< xs
  b <- gateB -< xs
  c <- gateC -< xs
  d <- gateD -< xs
  e <- gateE -< xs
  f <- gateF -< xs
  g <- gateG -< xs
  returnA -< [a,b,c,d,e,f,g]
  where
  gateA = lut4gate "A" $ \i3 i2 i1 i0 -> or
    [i3 && not i2 && not i1, not i3 && i2 && i0, not i2 && not i0, i3 && not i0, not i3 && i1, i2 && i1]
  gateB = lut4gate "B" $ \i3 i2 i1 i0 -> or
    [not i3 && not i1 && not i0, i3 && not i1 && i0, not i3 && i1 && i0, not i2 && not i0, not i3 && not i2]
  gateC = lut4gate "C" $ \i3 i2 i1 i0 -> or
    [not i3 && not i1, i3 && not i2, not i3 && i2, not i1 && i0, not i3 && i0]
  gateD = lut4gate "D" $ \i3 i2 i1 i0 -> or
    [not i3 && not i2 && not i0, not i2 && i1 && i0, i2 && i1 && not i0, i2 && not i1 && i0, i3 && not i1]
  gateE = lut4gate "E" $ \i3 i2 i1 i0 -> or
    [i1 && not i0, i3 && i1, i3 && i2, not i2 && not i0]
  gateF = lut4gate "F" $ \i3 i2 i1 i0 -> or
    [not i3 && i2 && not i1, not i1 && not i0, i3 && not i2, i2 && not i0, i3 && i1]
  gateG = lut4gate "G" $ \i3 i2 i1 i0 -> or
    [not i3 && i2 && not i1, i3 && not i2, i1 && not i0, not i2 && i1, i3 && i0]
