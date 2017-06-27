{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TupleSections #-}

module Lava.Xilinx.Arrow
  ( module Lava.Class
  , module Lava.Xilinx.Arrow
  , module Control.Arrow
  ) where

import Lava.Class
import Lava.Types

import Control.Applicative
import Control.Arrow
import Control.Arrow.Operations
import Control.Category
import Control.Monad.Fix
import Control.Monad.Trans.State
import qualified Lava as Xilinx
import Prelude hiding (id, (.))


type Xilinx = KleisliA (->) (State Xilinx.Netlist)

instance Category Xilinx where
  id = KleisliA pure
  -- default composition goes from left to right
  KleisliA g . KleisliA f = KleisliA $ f Xilinx.>-> g

instance Arrow Xilinx where
  arr f = KleisliA $ pure . f
  first  (KleisliA f) = KleisliA $ \ ~(a,b) -> (,b) <$> f a
  second (KleisliA f) = KleisliA $ \ ~(a,b) -> (a,) <$> f b
  -- parallel composition goes from bottom to top
  KleisliA f *** KleisliA g = KleisliA $ f `Xilinx.par2` g

instance ArrowLava Xilinx where
  KleisliA f >|> KleisliA g = KleisliA $ f Xilinx.>|> g
  KleisliA f ->- KleisliA g = KleisliA $ f Xilinx.->- g

instance ArrowChoice Xilinx where
  left  f = f +++ arr id
  right f = arr id +++ f
  f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
  KleisliA f ||| KleisliA g = KleisliA $ either f g

instance ArrowLoop Xilinx where
  loop (KleisliA f) = KleisliA $ fmap fst . mfix . (\x y -> f (x, snd y))

instance ArrowApply Xilinx where
  app = KleisliA $ \(KleisliA f, x) -> f x

instance ArrowState Xilinx.Netlist Xilinx where
  fetch = KleisliA $ const get
  store = KleisliA put

instance Functor (Xilinx a) where
  fmap = (^<<)

instance Applicative (Xilinx a) where
  pure    = arr . const
  -- side effects of `f` are had first
  f <*> g = arr (\ ~(x,y) -> x y) . second g . first f . arr (\x -> (x,x))
