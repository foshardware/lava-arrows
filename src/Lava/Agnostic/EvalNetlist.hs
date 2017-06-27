{-# LANGUAGE Arrows #-}

module Lava.Agnostic.EvalNetlist where

import Lava.Agnostic.Arrow
import Lava.Agnostic.Gates
import Lava.Agnostic.Netlist
import Lava.Types


evalNetlist :: Arrow a => String -> AgnosticA a () () -> a () Netlist
evalNetlist name circuit = preLayoutNetlist name $ wireUpPowerAndGround >>> circuit

preLayoutNetlist :: Arrow a => String -> AgnosticA a () () -> a () Netlist
preLayoutNetlist name (StateA circuit) = proc () -> do
  snd ^<< circuit -< ((), emptyNetlist { circuitName = name })

wireUpPowerAndGround :: Arrow a => AgnosticA a () ()
wireUpPowerAndGround = let nul = arr $ const () in gnd ->- nul ->- vcc ->- nul

