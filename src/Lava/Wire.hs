{-# LANGUAGE FlexibleInstances #-}

module Lava.Wire where

import Test.QuickCheck


class Wires a where
  tie :: [Wire] -> a
  untie :: a -> [Wire]

data Wire = Wire
  { fromWire  :: !Int -- there is no need for thunks, we need this value everytime
  , wireValue :: Bool -- wire state in testing
  }

wire :: Bool -> Wire
wire True  = Wire 1 True
wire False = Wire 0 False

newtype Clock = Clock Wire

instance Wires Wire where
  untie = pure
  tie = head

instance Wires [Wire] where
  untie = id
  tie = id

instance Wires (Wire, Wire) where
  untie (a, b) = [a, b]
  tie [a, b] = (a, b)
  tie _ = error "Wires (Wire, Wire): not enough elements"

instance Wires (Wire, Wire, Wire) where
  untie (a, b, c) = [a, b, c]
  tie (a:b:c:_) = (a, b, c)
  tie _ = error "Wires (Wire, Wire, Wire): not enough elements"

instance Wires (Wire, (Wire, Wire)) where
  untie (a, (b, c)) = [a, b, c]
  tie (a:b:c:_) = (a, (b, c))
  tie _ = error "Wires (Wire, Wire, Wire): not enough elements"

instance Wires (Wire, Wire, Wire, Wire) where
  untie (a, b, c, d) = [a, b, c, d]
  tie (a:b:c:d:_) = (a, b, c, d)
  tie _ = error "Wires (Wire, Wire, Wire): not enough elements"

instance Wires (Wire, (Wire, (Wire, Wire))) where
  untie (a, (b, (c, d))) = [a, b, c, d]
  tie (a:b:c:d:_) = (a, (b, (c, d)))
  tie _ = error "Wires (Wire, Wire, Wire): not enough elements"

instance Show Wire where
  show (Wire i b) = show i ++ ": " ++ if b then "1" else "0"

instance Eq Wire where
  Wire _ v == Wire _ v' = v == v'

instance Enum Wire where
  toEnum i | i > 65535 = error "wire literal > 65535"
  toEnum i = Wire (fromIntegral i) (odd i)
  fromEnum (Wire i _) = i

instance Num Wire where fromInteger = fromIntegral . fromEnum

instance Arbitrary Wire where
  arbitrary = Wire (-1) <$> arbitrary

