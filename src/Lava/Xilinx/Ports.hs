{-# LANGUAGE Arrows, TypeOperators #-}

module Lava.Xilinx.Ports
  ( module Lava
  , module Lava.Xilinx.Ports
  ) where

import Lava.Types
import Lava.Wire
import Lava.Xilinx.Arrow

import Control.Applicative
import qualified Lava
import Lava (NetType(..), Dir(..))


clock :: String -> Xilinx () Clock
clock name = Clock ^<< inputPort name

inputPort :: String -> Xilinx () Wire
inputPort name = KleisliA $ \() -> toEnum <$> Lava.inputPort name Lava.BitType

inputBitVec :: Wires a => String -> Lava.NetType -> Xilinx () a
inputBitVec name bvt = KleisliA $ \() -> tie . map toEnum <$> Lava.inputBitVec name bvt

outputPort :: String -> Xilinx Wire ()
outputPort name = proc (Wire i _) -> do
  KleisliA (Lava.outputPort name Lava.BitType) -< i

outputBitVec :: Wires a => String -> Lava.NetType -> Xilinx a ()
outputBitVec name bvt = proc w -> do
  KleisliA (Lava.outputBitVec name bvt) -< fromWire <$> untie w
