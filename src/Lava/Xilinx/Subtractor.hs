{-# LANGUAGE Arrows, TypeOperators, TupleSections #-}

module Lava.Xilinx.Subtractor where

import Lava.Arrow
import Lava.Wire
import Lava.Xilinx.Arrow
import Lava.Xilinx.Gates


comparator :: Wires a => Xilinx (a, a) Wire
comparator = snd ^<< subtractorWithCarry <<^ (1,)

subtractor :: Wires a => Xilinx (a, a) a
subtractor = fst ^<< subtractorWithCarry <<^ (1,)

subtractorWithCarry :: Wires a => Xilinx (Wire, (a, a)) (a, Wire)
subtractorWithCarry = proc (c, (a,b)) ->
  first (tie . reverse) ^<< column oneBitSubtractor -< (c, reverse $ untie a `zip` untie b)

oneBitSubtractor :: Xilinx (Wire, (Wire, Wire)) (Wire, Wire)
oneBitSubtractor = proc (cin, (a, b)) -> do
  part <- xnor -< (a, b)
  xor *** mux -< ((part, cin), (part, (a, cin)))
