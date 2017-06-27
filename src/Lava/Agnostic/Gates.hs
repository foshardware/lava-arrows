{-# LANGUAGE Arrows, TupleSections #-}

module Lava.Agnostic.Gates where

import Lava.Arrow
import Lava.Agnostic.Arrow
import Lava.Agnostic.Netlist
import Lava.Agnostic.Primitive
import Lava.Wire

import Data.Set hiding (null)


switch :: (ArrowChoice l, ArrowLoop l, Wires a) => [Wire] -> [a] -> AgnosticA l a a
switch bs vs = switch_ <<< arr (\def -> zip bs vs ++ [(undefined, def)])

switch_ :: (ArrowChoice l, ArrowLoop l, Wires a) => AgnosticA l [(Wire, a)] a
switch_ = proc ((bool, out) : rest) ->
  if null rest
  then returnA -< out
  else do
    val <- switch_ -< rest
    when -< (bool, out, val)

when :: (ArrowChoice l, ArrowLoop l, Wires a) => AgnosticA l (Wire, a, a) a
when = proc (c, a, b) -> do
  tie ^<< sequenceV cond -< zip3 (repeat c) (untie a) (untie b)

cond :: Arrow a => AgnosticA a (Wire, Wire, Wire) Wire
cond = mux <<^ (\(s,c,d) -> (s,(d,c)))

newWire :: Arrow a => AgnosticA a () Wire
newWire = proc () -> do
  state <- fetch -< ()
  () <- store -< state { netCount = netCount state + 1 }
  returnA -< Wire (netCount state) False

newBitVec :: (Arrow a, Wires w) => AgnosticA a () w
newBitVec = proc () -> do
  state <- fetch -< ()
  let result = tie [Wire w False | w <- [(netCount state)..(netCount state + 128)]]
  () <- store -< state { netCount = netCount state + length (untie result) }
  returnA -< result 

gnd :: Arrow a => AgnosticA a () Wire
gnd = proc () -> do
  state <- fetch -< ()
  let new = Instance (Gnd (netCount state)) "gnd" (instCount state)
  () <- store -< state
    { instances = new : instances state
    , netCount  = netCount  state + 1
    , instCount = instCount state + 1
    , usedPrims = UGnd `insert` usedPrims state
    }
  returnA -< Wire (netCount state) False

vcc :: Arrow a => AgnosticA a () Wire
vcc = proc () -> do
  state <- fetch -< ()
  let new = Instance (Vcc (netCount state)) "vcc" (instCount state)
  () <- store -< state
    { instances = new : instances state
    , netCount  = netCount  state + 1
    , instCount = instCount state + 1
    , usedPrims = UVcc `insert` usedPrims state
    }
  returnA -< Wire (netCount state) True

mux :: Arrow a => AgnosticA a (Wire, (Wire, Wire)) Wire
mux = proc (s, (d, c)) -> do
  state <- fetch -< ()
  let new = Instance
        (MuxCY (fromEnum s) (fromEnum d) (fromEnum c) (netCount state)) "muxcy" (instCount state)
  () <- store -< state
    { instances = new : instances state
    , netCount  = netCount  state + 1
    , instCount = instCount state + 1
    , usedPrims = UMuxCY `insert` usedPrims state
    }
  returnA -< Wire (netCount state) $ if wireValue s then wireValue c else wireValue d

xor :: Arrow a => AgnosticA a (Wire, Wire) Wire
xor = proc (a, b) -> do
  state <- fetch -< ()
  let new = Instance (XorCY (fromEnum a) (fromEnum b) (netCount state)) "xorcy" (instCount state)
  () <- store -< state
    { instances = new : instances state
    , netCount  = netCount  state + 1
    , instCount = instCount state + 1
    , usedPrims = UXorCY `insert` usedPrims state
    }
  returnA -< Wire (netCount state) $ wireValue a /= wireValue b

and2 :: Arrow a => AgnosticA a (Wire, Wire) Wire
and2 = proc (a,b) -> do
  state <- fetch -< ()
  let new = Instance (And2 (fromEnum a) (fromEnum b) (netCount state)) "and2" (instCount state)
  () <- store -< state
    { instances = new : instances state
    , netCount  = netCount  state + 1
    , instCount = instCount state + 1
    , usedPrims = UAnd2 `insert` usedPrims state
    }
  returnA -< Wire (netCount state) $ wireValue a && wireValue b

or2 :: Arrow a => AgnosticA a (Wire, Wire) Wire
or2 = proc (a,b) -> do
  state <- fetch -< ()
  let new = Instance (Or2 (fromEnum a) (fromEnum b) (netCount state)) "or2" (instCount state)
  () <- store -< state
    { instances = new : instances state
    , netCount  = netCount  state + 1
    , instCount = instCount state + 1
    , usedPrims = UOr2 `insert` usedPrims state
    }
  returnA -< Wire (netCount state) $ wireValue a || wireValue b

inv :: Arrow a => AgnosticA a Wire Wire
inv = proc a -> do
  state <- fetch -< ()
  let new = Instance (Inv (fromEnum a) (netCount state)) "inv" (instCount state)
  () <- store -< state
    { instances = new : instances state
    , netCount  = netCount  state + 1
    , instCount = instCount state + 1
    , usedPrims = UInv `insert` usedPrims state
    }
  returnA -< Wire (netCount state) . not $ wireValue a

nand2 :: Arrow a => AgnosticA a (Wire, Wire) Wire
nand2 = inv <<< and2

nor2 :: Arrow a => AgnosticA a (Wire, Wire) Wire
nor2 = inv <<< or2

and :: (Arrow a, ArrowApply a, Wires v) => AgnosticA a v Wire
and = proc is -> let input = untie is in and' $ length input -<< input
and' :: (Arrow a, ArrowApply a) => Int -> AgnosticA a [Wire] Wire
and' 0 = error "generic and: wrong input"
and' 2 = and2 <<^ tie
and' n | m == 0 = and2 <<< and' d *** and' d <<^ splitAt d
  where (d, m) = divMod n 2
and' n = proc (i:is) -> and2 <<< (i,) ^<< and' (n - 1) -<< is

nand :: (Arrow a, ArrowApply a, Wires v) => AgnosticA a v Wire
nand = proc is -> let input = untie is in nand' $ length input -<< input
nand' :: (Arrow a, ArrowApply a) => Int -> AgnosticA a [Wire] Wire 
nand' n = inv <<< and' n

or :: (Arrow a, ArrowApply a, Wires v) => AgnosticA a v Wire
or = proc is -> let input = untie is in or' $ length input -<< input
or' :: (Arrow a, ArrowApply a) => Int -> AgnosticA a [Wire] Wire
or' 0 = error "generic or: wrong input"
or' 2 = or2 <<^ tie
or' n | m == 0 = or2 <<< or' d *** or' d <<^ splitAt d
  where (d, m) = divMod n 2
or' n = proc (i:is) -> or2 <<< (i,) ^<< or' (n - 1) -<< is

nor :: (Arrow a, ArrowApply a, Wires v) => AgnosticA a v Wire
nor = proc is -> let input = untie is in nor' $ length input -<< input
nor' :: (Arrow a, ArrowApply a) => Int -> AgnosticA a [Wire] Wire
nor' n = inv <<< or' n

