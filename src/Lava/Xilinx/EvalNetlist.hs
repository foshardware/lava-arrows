
module Lava.Xilinx.EvalNetlist
  ( module Lava
  , module Lava.Diagnostics
  , evalNetlist
  ) where

import Lava.Types
import Lava.Xilinx.Arrow

import Lava (putXilinxVHDL, XilinxArchitecture(..))
import Lava.Diagnostics (Netlist(..))
import qualified Lava


evalNetlist :: String -> XilinxArchitecture -> Xilinx () () -> Netlist
evalNetlist name arch (KleisliA block) = Lava.computeNetlist name arch $ block ()

