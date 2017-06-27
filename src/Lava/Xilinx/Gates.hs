{-# LANGUAGE Arrows, TypeOperators, TupleSections #-}

module Lava.Xilinx.Gates where

import Control.Applicative
import qualified Lava as L
import qualified Lava.Virtex2 as L
import Prelude hiding (odd, even, and, or)

import Lava.Arrow
import Lava.Types
import Lava.Wire
import Lava.Xilinx.Arrow


switch :: Wires a => [Wire] -> [a] -> Xilinx a a
switch bs vs = switch_ <<^ f
  where f def = zip bs vs ++ [(undefined, def)]

switch_ :: Wires a => Xilinx [(Wire, a)] a
switch_ = proc ((bool, out) : rest) ->
  if null rest
  then returnA -< out
  else do
    val <- switch_ -< rest
    when -< (bool, out, val)

when :: Wires a => Xilinx (Wire, a, a) a
when = proc (c, a, b) -> do
  tie ^<< sequenceV cond -< zip3 (repeat c) (untie a) (untie b)

cond :: Xilinx (Wire, Wire, Wire) Wire
cond = lut3gate "cond" $ \b0 b1 b2 -> if b0 then b1 else b2

mux :: Xilinx (Wire, (Wire, Wire)) Wire
mux = proc (a, (b, c)) -> lut3gate "mux" (\b0 b1 b2 -> if b0 then b2 else b1) -< (a, b, c)

inv :: Xilinx Wire Wire
inv = lut1gate "inv" not

and2 :: Xilinx (Wire, Wire) Wire
and2 = lut2gate "and2" (&&)

and3 :: Wires a => Xilinx a Wire
and3 = lut3gate "and3" $ \b0 b1 b2 -> b0 && b1 && b2

and4 :: Wires a => Xilinx a Wire
and4 = lut4gate "and4" $ \b0 b1 b2 b3 -> b0 && b1 && b2 && b3

and :: Wires a => Xilinx a Wire
and = proc is -> let input = untie is in and' $ length input -<< input
and' :: Int -> Xilinx [Wire] Wire
and' 0 = error "generic and: wrong input"
and' 2 = and2 <<^ tie
and' 3 = and3
and' 4 = and4
and' n | m == 0 = and2 <<< and' d *** and' d <<^ splitAt d
  where (d, m) = divMod n 2
and' n = proc (i:is) -> and2 <<< (i,) ^<< and' (n - 1) -<< is

nand2 :: Xilinx (Wire, Wire) Wire
nand2 = lut2gate "nand2" $ \b0 b1 -> not $ b0 && b1

nand3 :: Wires a => Xilinx a Wire
nand3 = lut3gate "nand3" $ \b0 b1 b2 -> not $ b0 && b1 && b2

nand4 :: Wires a => Xilinx a Wire
nand4 = lut4gate "nand4" $ \b0 b1 b2 b3 -> not $ b0 && b1 && b2 && b3

nand5 :: Wires a => Xilinx a Wire
nand5 = lut5gate "nand5" $ \b0 b1 b2 b3 b4 -> not $ b0 && b1 && b2 && b3 && b4

nand :: Wires a => Xilinx a Wire
nand = proc is -> let input = untie is in nand' $ length input -<< input
nand' :: Int -> Xilinx [Wire] Wire
nand' 2 = nand2 <<^ tie
nand' 3 = nand3
nand' 4 = nand4
nand' 5 = nand5
nand' n = inv <<< and' n

xor :: Xilinx (Wire, Wire) Wire
xor = lut2gate "xor2" (/=) 

xnor :: Xilinx (Wire, Wire) Wire
xnor = lut2gate "xnor2" (==) 

or2 :: Xilinx (Wire, Wire) Wire
or2 = lut2gate "or2" (||)

or3 :: Wires a => Xilinx a Wire
or3 = lut3gate "or3" $ \b0 b1 b2 -> b0 || b1 || b2

or4 :: Wires a => Xilinx a Wire
or4 = lut4gate "or4" $ \b0 b1 b2 b3 -> b0 || b1 || b2 || b3

nor2 :: Xilinx (Wire, Wire) Wire
nor2 = lut2gate "nor2" (||)

nor3 :: Wires a => Xilinx a Wire
nor3 = lut3gate "nor3" $ \b0 b1 b2 -> not $ b0 || b1 || b2

nor4 :: Wires a => Xilinx a Wire
nor4 = lut4gate "nor4" $ \b0 b1 b2 b3 -> not $ b0 || b1 || b2 || b3

nor5 :: Wires a => Xilinx a Wire
nor5 = lut5gate "nor5" $ \b0 b1 b2 b3 b4 -> not $ b0 || b1 || b2 || b3 || b4

or :: Wires a => Xilinx a Wire
or = proc is -> let input = untie is in or' $ length input -<< input
or' :: Int -> Xilinx [Wire] Wire
or' 0 = error "generic or: wrong input"
or' 2 = or2 <<^ tie
or' 3 = or3
or' 4 = or4
or' n | m == 0 = or2 <<< or' d *** or' d <<^ splitAt d
  where (d, m) = divMod n 2
or' n = proc (i:is) -> or2 <<< (i,) ^<< or' (n - 1) -<< is

nor :: Wires a => Xilinx a Wire
nor = proc is -> let input = untie is in nor' $ length input -<< input
nor' :: Int -> Xilinx [Wire] Wire
nor' 2 = nor2 <<^ tie
nor' 3 = nor3
nor' 4 = nor4
nor' 5 = nor5
nor' n = inv <<< or' n

vcc :: Xilinx () Wire
vcc = KleisliA $ \() -> flip Wire True <$> L.vcc

gnd :: Xilinx () Wire
gnd = KleisliA $ \() -> flip Wire False <$> L.gnd

odd :: Wires a => Xilinx a Wire
odd = arr $ last . untie

even :: Wires a => Xilinx a Wire
even = inv <<< odd

lut1gate :: String -> (Bool -> Bool) -> Xilinx Wire Wire
lut1gate comment operation = proc (Wire i b) -> do
  o <- KleisliA (L.lut1gate operation comment) -< i
  returnA -< Wire o (operation b)

lut2gate :: String -> (Bool -> Bool -> Bool) -> Xilinx (Wire, Wire) Wire
lut2gate comment operation = proc (Wire i0 b0, Wire i1 b1) -> do
  o <- KleisliA (L.lut2gate operation comment) -< (i0, i1)
  returnA -< Wire o (operation b0 b1)

lut3gate :: Wires a => String -> (Bool -> Bool -> Bool -> Bool) -> Xilinx a Wire
lut3gate comment operation = proc xs -> do
  let (Wire i0 b0 : Wire i1 b1 : Wire i2 b2 : _) = untie xs
  o <- KleisliA (L.lut3gate operation comment) -< (i0, i1, i2)
  returnA -< Wire o (operation b0 b1 b2)

lut4gate :: Wires a => String -> (Bool -> Bool -> Bool -> Bool -> Bool) -> Xilinx a Wire
lut4gate comment operation = proc xs -> do
  let (Wire i0 b0 : Wire i1 b1 : Wire i2 b2 : Wire i3 b3 : _) = untie xs
  o <- KleisliA (L.lut4gate operation comment) -< (i0, i1, i2, i3)
  returnA -< Wire o (operation b0 b1 b2 b3)

lut5gate :: Wires a => String -> (Bool -> Bool -> Bool -> Bool -> Bool -> Bool) -> Xilinx a Wire
lut5gate comment operation = proc xs -> do
  let (Wire i0 b0 : Wire i1 b1 : Wire i2 b2 : Wire i3 b3 : Wire i4 b4 : _) = untie xs
  o <- KleisliA (L.lut5gate operation comment) -< (i0, i1, i2, i3, i4)
  returnA -< Wire o (operation b0 b1 b2 b3 b4)

lut6gate :: Wires a => String -> (Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool) -> Xilinx a Wire
lut6gate comment operation = proc xs -> do
  let (Wire i0 b0 : Wire i1 b1 : Wire i2 b2 : Wire i3 b3 : Wire i4 b4 : Wire i5 b5 : _) = untie xs
  o <- KleisliA (L.lut6gate operation comment) -< (i0, i1, i2, i3, i4, i5)
  returnA -< Wire o (operation b0 b1 b2 b3 b4 b5)
