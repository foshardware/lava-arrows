{-# LANGUAGE Arrows, TypeOperators, TupleSections #-}

module Lava.Xilinx.Register where

import Lava.Arrow
import Lava.BitVec hiding (foldl)
import Lava.Wire
import Lava.Xilinx.Arrow
import Lava.Xilinx.Gates
import Lava.Xilinx.Latch


sipo :: Wires a => Int -> Xilinx (Wire, Clock) a
sipo n = proc (x, clk) -> do
  let circuit = proc (b : rest) -> ( : b : rest) . fst ^<< flipFlopD -<< (b, clk)
  tie . init ^<< foldl (<<<) (arr id) (replicate n circuit) -<< [x]

muxed4BitSIPO :: Xilinx (Wire, Clock) (BitVec4, BitVec4)
muxed4BitSIPO = proc (v, Clock clk) -> do
  Clock t <- clockDivider <<< clockDivider -< Clock clk
  t' <- inv -< t
  (s0, s1) <- and2 *** and2 -< ((t, clk), (t', clk))
  sipo 4 *** sipo 4 -< ((v, Clock s0), (v, Clock s1))

piso :: Wires a => Xilinx (a, Wire, Clock) Wire
piso = proc (as, shift, clk) -> let (a : rest) = untie as in do
  write <- inv -< shift
  let circuit = proc (x, y) -> fst ^<< flipFlopD <<< (, clk) ^<< f -<< ((x, write), (shift, y))
  foldl (<<<) (arr id) [circuit <<^ (x,) | x <- rest] <<< fst ^<< flipFlopD -<< (a, clk)
  where f = nand2 <<< nand2 *** nand2

muxed4BitPISO :: Wires a => Xilinx (a, a, Clock) Wire
muxed4BitPISO = proc (a, b, clk) -> do
  Clock t <- clockDivider <<< clockDivider -< clk
  t' <- inv -< t
  mux <<< (t,) ^<< piso *** piso -<< ((a, t, clk), (b, t', clk))

