{-# LANGUAGE Arrows #-}

module Lava.Agnostic.Latch where

import Data.Bits
import Data.Set (insert)
import Lava.Agnostic.Arrow
import Lava.Agnostic.Gates
import Lava.Agnostic.Netlist
import Lava.Agnostic.Primitive
import Lava.Wire


latch :: Arrow a => Int -> AgnosticA a (Wire, Wire) ()
latch n = proc (i, o) -> do
  state <- fetch -< ()
  let new = Instance (Latch n (fromEnum i) (fromEnum o)) "latch" (instCount state)
  store -< state
    { instances = new : instances state
    , instCount = instCount state + 1
    }

flipFlopD :: Arrow a => Int -> AgnosticA a (Wire, Wire, Clock) ()
flipFlopD n = proc (d, q, Clock clk) -> do
  state <- fetch -< ()
  let new = Instance (FlipFlopD n (fromEnum d) (fromEnum q) (fromEnum clk)) "d-ff" (instCount state)
  store -< state
    { instances = new : instances state
    , instCount = instCount state + 1
    }

flipFlopT :: Arrow a => AgnosticA a (Wire, Wire, Clock) ()
flipFlopT = proc (t, q, clk) -> flipFlopJK -< ((t, t), q, clk)

flipFlopJK :: Arrow a => AgnosticA a ((Wire, Wire), Wire, Clock) ()
flipFlopJK = proc ((j, k), q, Clock clk) -> do
  state <- fetch -< ()
  let new = Instance
        (FlipFlopJK (fromEnum j) (fromEnum k) (fromEnum q) (fromEnum clk)) "jk-ff" (instCount state)
  store -< state
    { instances = new : instances state
    , instCount = instCount state + 1
    , usedPrims = UFlipFlopJK `insert` usedPrims state
    }

clockDivider :: Arrow a => Int -> AgnosticA a Clock Clock
clockDivider n | n .&. (n - 1) /= 0 = error "clockDivider !power of 2 not implemented"
clockDivider 1 = arr id
clockDivider n = proc clk -> do
  q <- newWire -< ()
  flipFlopT -< (toEnum 1, q, clk)
  clockDivider (div n 2) -< Clock q

clockDelay :: Arrow a => Int -> AgnosticA a (Wire, Clock) Wire
clockDelay 0 = arr fst 
clockDelay n = proc (d, clk) -> do
  q <- newWire -< ()
  flipFlopD 0 -< (d, q, clk)
  clockDelay (n - 1) -< (q, clk)

