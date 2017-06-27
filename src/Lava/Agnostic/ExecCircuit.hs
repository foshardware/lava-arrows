{-# LANGUAGE Arrows #-}

module Lava.Agnostic.ExecCircuit where

import Lava.Agnostic.Arrow
import Lava.Agnostic.Netlist
import Lava.Types


test :: Arrow l => AgnosticA l a b -> l a b
test = execCircuit

execCircuit :: Arrow l => AgnosticA l a b -> l a b
execCircuit block = proc input -> do fst ^<< runStateA block -< (input, emptyNetlist)
  
