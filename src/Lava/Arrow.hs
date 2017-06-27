{-# LANGUAGE Arrows, TypeOperators #-}

module Lava.Arrow where

import Control.Arrow
import Control.Applicative


sequenceV :: (ArrowChoice l, ArrowLoop l) => l i o -> l [i] [o]
sequenceV block = proc (b : bs) ->
  if null bs
  then pure ^<< block -< b
  else do
    (c, cs) <- block *** sequenceV block -< (b, bs)
    returnA -< c : cs

column :: (ArrowChoice l, ArrowLoop l) => l (a, b) (c, a) -> l (a, [b]) ([c], a)
column block = proc (a, b : bs) ->
  if null bs
  then first pure ^<< block -< (a, b)
  else do
    rec   ((c, d0), (cs, d)) <- block *** column block -< ((a, b), (d0, bs))
    returnA -< (c : cs, d)

