
module Lava.Agnostic.Primitive where


data Primitive
  = MuxCY Int Int Int Int
  | XorCY Int Int Int
  | Gnd Int | Vcc Int
  | And2 Int Int Int | Or2 Int Int Int
  | Inv Int Int
  | Latch Int Int Int
  | FlipFlopD Int Int Int Int
  | FlipFlopJK Int Int Int Int
  deriving (Eq, Show)

data UsedPrimitive
  = UMuxCY | UXorCY
  | UGnd | UVcc
  | UAnd2 | UOr2
  | UInv
  | UFlipFlopJK
  deriving (Eq, Ord, Show)

