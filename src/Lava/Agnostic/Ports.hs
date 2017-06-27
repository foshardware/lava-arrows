{-# LANGUAGE Arrows #-}

module Lava.Agnostic.Ports where

import Lava.Agnostic.Arrow
import Lava.Agnostic.Netlist
import Lava.Wire


clock :: Arrow a => AgnosticA a () Clock
clock = proc () -> do
  state <- fetch -< ()
  () <- store -< state
    { ports = Port "clk" InputPort ClockType [netCount state] : ports state
    , netCount = netCount state + 1
    }
  returnA -< Clock $ Wire (netCount state) False

inputPort :: Arrow a => String -> AgnosticA a () Wire
inputPort name = proc () -> do
  state <- fetch -< ()
  () <- store -< state
    { ports = Port name InputPort BitType [netCount state] : ports state
    , netCount = netCount state + 1
    }
  returnA -< Wire (netCount state) False

outputPort :: Arrow a => String -> AgnosticA a Wire ()
outputPort name = proc o -> do
  state <- fetch -< ()
  store -< state { ports = Port name OutputPort BitType [fromWire o] : ports state }

inputBitVec :: (Arrow l, Wires a) => String -> NetType -> AgnosticA l () a
inputBitVec name typ@(BitVec a dir b) = proc () -> do
  state <- fetch -< ()
  let oNets = [netCount state + i | (_, i) <- zip (portRange a dir b) [0..]]
  () <- store -< state
    { ports = Port name InputPort typ oNets : ports state
    , netCount = netCount state + 1 + abs (a-b)
    }
  returnA -< tie $ toEnum <$> oNets

outputBitVec :: (Arrow l, Wires a) => String -> NetType -> AgnosticA l a ()
outputBitVec name typ = proc o -> do
  state <- fetch -< ()
  store -< state { ports = Port name OutputPort typ (fromWire <$> untie o) : ports state }

