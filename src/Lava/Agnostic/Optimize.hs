{-# LANGUAGE Arrows #-}

module Lava.Agnostic.Optimize where

import Lava.Agnostic.Arrow
import Lava.Agnostic.ExecCircuit
import Lava.Types
import Lava.Wire


minimalForm :: (Arrow l, Wires a, Wires b) => AgnosticA l a b -> AgnosticA l a b
minimalForm _ = undefined
