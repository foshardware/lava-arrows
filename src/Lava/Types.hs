
module Lava.Types where


newtype KleisliA a m i o = KleisliA { runKleisliA :: a i (m o) }

newtype StateA s a b c = StateA { runStateA :: a (b, s) (c, s) }


type TruthTable = [([BoolExp], Bool)]

data BoolExp =
  I Int
  | Or BoolExp BoolExp
  | And BoolExp BoolExp
  | Not BoolExp
  deriving Show

instance Num BoolExp where
  (*) = And
  (+) = Or
  fromInteger = I . fromIntegral

