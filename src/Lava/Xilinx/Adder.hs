{-# LANGUAGE Arrows, TypeOperators #-}

module Lava.Xilinx.Adder where

import Lava.Arrow
import Lava.Wire
import Lava.Xilinx.Arrow
import Lava.Xilinx.Gates
import Lava.Xilinx.Subtractor


adderSubtractor :: Wires a => Xilinx (Wire, (a, a)) a
adderSubtractor = proc (minus, (a, b)) -> do
  (add, sub) <- adder &&& subtractor -< (a, b)
  when -< (minus, sub, add)

adder :: Wires a => Xilinx (a, a) a
adder = proc (a, b) -> do
  fst ^<< adderWithCarry -< (0, (a, b))

adderWithCarry :: Wires a => Xilinx (Wire, (a, a)) (a, Wire)
adderWithCarry = proc (c, (a, b)) -> do
  first (tie . reverse) ^<< column oneBitAdder -< (c, reverse $ untie a `zip` untie b)

oneBitAdder :: Xilinx (Wire, (Wire, Wire)) (Wire, Wire)
oneBitAdder = proc (cin, (a, b)) -> do
  part <- xor -< (a, b)
  xor *** mux -< ((part, cin), (part, (a, cin)))
