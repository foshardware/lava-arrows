{-# LANGUAGE Arrows, FlexibleInstances, MultiParamTypeClasses #-}

module Lava.Agnostic.Arrow
  ( module Lava.Class
  , module Lava.Agnostic.Arrow
  , module Control.Arrow
  , module Control.Arrow.Operations
  , module Control.Applicative
  ) where

import Lava.Class
import Lava.Agnostic.Netlist as Agnostic
import Lava.Types

import Control.Applicative
import Control.Arrow
import Control.Arrow.Operations
import Control.Arrow.Transformer
import Control.Arrow.Transformer.State
import Control.Category
import Data.Monoid
import Prelude hiding (id, (.))


type AgnosticA = StateA Agnostic.Netlist
type Agnostic = AgnosticA (->)

instance Arrow a => Category (AgnosticA a) where
  id = StateA id
  StateA g . StateA f = StateA $ g . f 

swapsnd :: ((a, b), c) -> ((a, c), b)
swapsnd ~(~(x, y), z) = ((x, z), y)

instance Arrow a => Arrow (AgnosticA a) where
  arr f = StateA . arr $ \(x, s) -> (f x, s)
  first (StateA f) = StateA $ arr swapsnd >>> first f >>> arr swapsnd

instance Arrow a => ArrowLava (AgnosticA a) where
  f >|> g = f >>> g 
  f ->- g = f >>> g

instance Arrow a => ArrowTransformer AgnosticA a where
  lift f = StateA $ first f

instance Arrow a => ArrowState Agnostic.Netlist (AgnosticA a) where
  fetch = StateA . arr $ \ ~(_, s) -> (s, s)
  store = StateA . arr $ \ ~(s, _) -> ((), s)

instance Arrow a => ArrowAddState Agnostic.Netlist (AgnosticA a) a where
  liftState = lift
  elimState = runStateA

instance ArrowChoice a => ArrowChoice (AgnosticA a) where
  left (StateA f) = StateA $ arr distr >>> left f >>> arr undistr
    where
    distr (Left  y, s) = Left (y, s)
    distr (Right z, s) = Right (z, s)
    undistr (Left  (y, s)) = (Left y, s)
    undistr (Right (z, s)) = (Right z, s)

instance ArrowApply a => ArrowApply (AgnosticA a) where
  app = StateA $ arr (\((StateA f, x), s) -> (f, (x, s))) >>> app

instance ArrowLoop a => ArrowLoop (AgnosticA a) where
  loop (StateA f) = StateA . loop $ arr swapsnd >>> f >>> arr swapsnd

instance ArrowPlus a => ArrowPlus (AgnosticA a) where
  StateA f <+> StateA g = StateA $ f <+> g

instance ArrowZero a => ArrowZero (AgnosticA a) where
  zeroArrow = StateA zeroArrow

instance Arrow a => Functor (AgnosticA a b) where
  fmap = (^<<)

instance Arrow a => Applicative (AgnosticA a b) where
  pure    = arr . const
  -- side effects of `f` are had first
  f <*> g = arr (\ ~(x,y) -> x y) . second g . first f . arr (\x -> (x,x))

instance ArrowPlus a => Alternative (AgnosticA a b) where
  empty = zeroArrow
  (<|>) = (<+>)

instance ArrowPlus a => Monoid (AgnosticA a b c) where
  mempty  = zeroArrow
  mappend = (<+>)
