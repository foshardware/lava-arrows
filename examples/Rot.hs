{-# LANGUAGE Arrows, TypeOperators #-}

module Rot where

import Lava.Agnostic hiding (and, or)
import Lava.Agnostic.Latch
import Lava.BitVec


type (->>) = Agnostic

main = putAgnosticBLIF $ evalNetlist "rot" topLevelRot ()

topLevelRot :: () ->> ()
topLevelRot = outputBitVec "out" (BitVec 3 Downto 0) <<< rot <<< clock

rot :: Clock ->> BitVec4
rot = proc clk -> do
  result@(d0 :. d1 :. d2 :. d3 :. ()) <- newBitVec -< ()
  clk' <- clockDivider (2^20) -< clk
  flipFlopD 0 -< (d3, d0, clk')
  flipFlopD 1 -< (d0, d1, clk') 
  flipFlopD 1 -< (d1, d2, clk') 
  flipFlopD 0 -< (d2, d3, clk') 
  returnA -< result
