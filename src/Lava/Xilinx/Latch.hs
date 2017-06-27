{-# LANGUAGE Arrows, TypeOperators, TupleSections #-}

module Lava.Xilinx.Latch where

import Control.Arrow.Operations
import Lava.Arrow
import Lava.Diagnostics
import Lava.Wire
import Lava.Xilinx.Arrow
import Lava.Xilinx.Gates


latchSR :: Xilinx (Wire, Wire) (Wire, Wire)
latchSR = proc (s, r) -> do
  n <- netCount ^<< fetch -< ()
  let (q, q') = (toEnum $ n + 2, toEnum $ n + 3)  
  (p, p') <- nand *** nand -< ((s, q'), (r, q))
  if fromWire p == fromWire q && fromWire p' == fromWire q'
    then returnA -< (q, q')
    else error "SR latch invalid netCount" -< ()  

clockDivider :: Xilinx Clock Clock
clockDivider = Clock . fst ^<< flipFlopT <<^ (1,)

clockDelay :: Xilinx (Wire, Clock) Wire
clockDelay = fst ^<< flipFlopD

flipFlopT :: Xilinx (Wire, Clock) (Wire, Wire)
flipFlopT = proc (t, clk) -> flipFlopJK -< (t, clk, t)

flipFlopD :: Xilinx (Wire, Clock) (Wire, Wire)
flipFlopD = proc (d, clk) -> flipFlopJK <<< (d, clk,) ^<< inv -<< d

flipFlopJK :: Xilinx (Wire, Clock, Wire) (Wire, Wire)
flipFlopJK = proc (j, Clock clk, k) -> do
  n <- netCount ^<< fetch -< ()
  let (q, q') = (toEnum $ n + 2, toEnum $ n + 3)
  (i, i') <- nand *** nand -< ((j, clk, q'), (q, clk, k))
  (p, p') <- nand *** nand -< ((i, q'), (i', q))
  if fromWire p == fromWire q && fromWire p' == fromWire q'
    then returnA -< (q, q')
    else error "JK flip-flop invalid netCount" -< ()

