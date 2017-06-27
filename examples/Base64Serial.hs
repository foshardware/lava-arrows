{-# LANGUAGE Arrows, TypeOperators, TupleSections #-}

module Base64Serial where

import Base64 (base64Encode, base64Decode)

import Lava.BitVec
import Lava.Xilinx
import Lava.Xilinx.FlipFlop
import Lava.Xilinx.Register
import Lava.Xilinx.Virtex6
import Prelude hiding (and)

main :: IO ()
main = do
  putXilinxVHDL $ evalNetlist "base64EncodeSerial" Virtex6 topLevelEncode
  putXilinxVHDL $ evalNetlist "base64DecodeSerial" Virtex6 topLevelDecode

topLevelEncode :: Xilinx () ()
topLevelEncode = proc () -> do
  clk@(Clock s) <- clock "clk" -< ()
  Clock t <- clockDivider <<< clockDivider <<< clockDivider -< clk
  t' <- inv -< t
  (s0, s1) <- and *** and -< ((t, s), (t', s))
  let withClocks v = ((v, Clock s0), (v, Clock s1))
  (byteA0, byteB0) <- sipo 8 *** sipo 8 <<< withClocks ^<< inputPort "bit0" -<< ()
  (byteA1, byteB1) <- sipo 8 *** sipo 8 <<< withClocks ^<< inputPort "bit1" -<< ()
  (byteA2, byteB2) <- sipo 8 *** sipo 8 <<< withClocks ^<< inputPort "bit2" -<< ()
  (outA0, outA1, outA2, outA3) <- base64Encode -< (byteA0, byteA1, byteA2)
  (outB0, outB1, outB2, outB3) <- base64Encode -< (byteB0, byteB1, byteB2)
  outputPort "out0" <<< mux <<< (t,) ^<< piso *** piso -<< ((outA0, t, clk), (outB0, t', clk))
  outputPort "out1" <<< mux <<< (t,) ^<< piso *** piso -<< ((outA1, t, clk), (outB1, t', clk))
  outputPort "out2" <<< mux <<< (t,) ^<< piso *** piso -<< ((outA2, t, clk), (outB2, t', clk))
  outputPort "out3" <<< mux <<< (t,) ^<< piso *** piso -<< ((outA3, t, clk), (outB3, t', clk))

topLevelDecode :: Xilinx () ()
topLevelDecode = proc () -> do
  clk@(Clock s) <- clock "clk" -< ()
  Clock t <- clockDivider <<< clockDivider <<< clockDivider -< clk
  t' <- inv -< t
  (s0, s1) <- and *** and -< ((t, s), (t', s))
  let withClocks v = ((v, Clock s0), (v, Clock s1))
  (byteA0, byteB0) <- sipo 8 *** sipo 8 <<< withClocks ^<< inputPort "bit0" -<< ()
  (byteA1, byteB1) <- sipo 8 *** sipo 8 <<< withClocks ^<< inputPort "bit1" -<< ()
  (byteA2, byteB2) <- sipo 8 *** sipo 8 <<< withClocks ^<< inputPort "bit2" -<< ()
  (byteA3, byteB3) <- sipo 8 *** sipo 8 <<< withClocks ^<< inputPort "bit3" -<< ()
  (outA0, outA1, outA2) <- base64Decode -< (byteA0, byteA1, byteA2, byteA3)
  (outB0, outB1, outB2) <- base64Decode -< (byteB0, byteB1, byteB2, byteB3)
  outputPort "out0" <<< mux <<< (t,) ^<< piso *** piso -<< ((outA0, t, clk), (outB0, t', clk))
  outputPort "out1" <<< mux <<< (t,) ^<< piso *** piso -<< ((outA1, t, clk), (outB1, t', clk))
  outputPort "out2" <<< mux <<< (t,) ^<< piso *** piso -<< ((outA2, t, clk), (outB2, t', clk))

