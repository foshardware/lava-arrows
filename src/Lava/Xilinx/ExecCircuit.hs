{-# LANGUAGE FlexibleInstances #-}

module Lava.Xilinx.ExecCircuit where

import Lava.Types
import Lava.Xilinx.Arrow

import Control.Monad.Trans.State
import qualified Lava as Xilinx

-- | Can be used with quickCheck like so:
-- quickCheck $ \g -> correct g $ test Virtex6 block g
-- where `correct` takes input and output value and returns a `Testable`, for instance `Bool`
test :: Maybe Xilinx.XilinxArchitecture -> Xilinx i o -> i -> o
test x = execCircuit $ maybe Xilinx.Virtex2 id x

execCircuit :: Xilinx.XilinxArchitecture -> Xilinx i o -> i -> o
execCircuit arch (KleisliA block) i = block i `evalState` Xilinx.computeNetlist "test" arch (return ())

