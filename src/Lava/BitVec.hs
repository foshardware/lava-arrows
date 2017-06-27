{-# LANGUAGE TypeOperators, FlexibleInstances #-}

module Lava.BitVec
  ( module Data.Vec
  , (+:+), (.:)
  , numFromBitVec, bitVecFromEnum
  , BitVec2, BitVec3, BitVec4, BitVec5, BitVec6, BitVec7, BitVec8
  , BitVec9, BitVec10, BitVec11, BitVec12, BitVec13, BitVec14, BitVec15, BitVec16
  ) where

import Lava.Wire

import Control.Applicative
import Data.Bits
import Data.Vec
import Data.Word
import Prelude hiding (take, drop, tail)
import Prelude as L
import Test.QuickCheck


type BitVec2 = Wire :. (Wire :. ())
type BitVec3 = Wire :. BitVec2
type BitVec4 = Wire :. BitVec3
type BitVec5 = Wire :. BitVec4
type BitVec6 = Wire :. BitVec5
type BitVec7 = Wire :. BitVec6
type BitVec8 = Wire :. BitVec7
type BitVec9 = Wire :. BitVec8
type BitVec10 = Wire :. BitVec9
type BitVec11 = Wire :. BitVec10
type BitVec12 = Wire :. BitVec11
type BitVec13 = Wire :. BitVec12
type BitVec14 = Wire :. BitVec13
type BitVec15 = Wire :. BitVec14
type BitVec16 = Wire :. BitVec15

(+:+) :: Append v1 v2 v3 => v1 -> v2 -> v3
(+:+) = append

(.:) :: b -> a -> a :. b
(.:) = flip (:.)

infixr 9 .:, +:+

instance Wires BitVec2 where
  untie w = flip getElem w <$> [0..1]
  tie = fromList

instance Wires BitVec3 where
  untie w = flip getElem w <$> [0..2]
  tie = fromList

instance Wires BitVec4 where
  untie w = flip getElem w <$> [0..3]
  tie = fromList

instance Wires BitVec5 where
  untie w = flip getElem w <$> [0..4]
  tie = fromList

instance Wires BitVec6 where
  untie w = flip getElem w <$> [0..5]
  tie = fromList

instance Wires BitVec7 where
  untie w = flip getElem w <$> [0..6]
  tie = fromList

instance Wires BitVec8 where
  untie w = flip getElem w <$> [0..7]
  tie = fromList

instance Wires BitVec9 where
  untie w = flip getElem w <$> [0..8]
  tie = fromList

instance Wires BitVec10 where
  untie w = flip getElem w <$> [0..9]
  tie = fromList

instance Wires BitVec11 where
  untie w = flip getElem w <$> [0..10]
  tie = fromList

instance Wires BitVec12 where
  untie w = flip getElem w <$> [0..11]
  tie = fromList

instance Wires BitVec13 where
  untie w = flip getElem w <$> [0..12]
  tie = fromList

instance Wires BitVec14 where
  untie w = flip getElem w <$> [0..13]
  tie = fromList

instance Wires BitVec15 where
  untie w = flip getElem w <$> [0..14]
  tie = fromList

instance Wires BitVec16 where
  untie w = flip getElem w <$> [0..15]
  tie = fromList

toBitVec :: Int -> Int -> [Wire]
toBitVec n i | i > (2^n-1) = error "BitVec literal too large"
toBitVec n i = wire . testBit i <$> L.reverse [0..(n-1)]

fromBitVec :: Int -> [Wire] -> Int
fromBitVec n xs = L.sum $ f <$> xs `zip` L.reverse [0..(n-1)]
  where f (b, x) = if wireValue b then 2^x else 0

numFromBitVec :: (Enum v, Num n) => v -> n
numFromBitVec = fromIntegral . fromEnum

bitVecFromEnum :: (Enum v, Enum n) => n -> v
bitVecFromEnum = toEnum . fromEnum

instance Enum BitVec2 where
  toEnum = tie . toBitVec 2
  fromEnum = fromBitVec 2 . untie

instance Enum BitVec3 where
  toEnum = tie . toBitVec 3
  fromEnum = fromBitVec 3 . untie

instance Enum BitVec4 where
  toEnum = tie . toBitVec 4
  fromEnum = fromBitVec 4 . untie

instance Enum BitVec5 where
  toEnum = tie . toBitVec 5
  fromEnum = fromBitVec 5 . untie

instance Enum BitVec6 where
  toEnum = tie . toBitVec 6
  fromEnum = fromBitVec 6 . untie

instance Enum BitVec7 where
  toEnum = tie . toBitVec 7
  fromEnum = fromBitVec 7 . untie

instance Enum BitVec8 where
  toEnum = tie . toBitVec 8
  fromEnum = fromBitVec 8 . untie

instance Enum BitVec9 where
  toEnum = tie . toBitVec 9
  fromEnum = fromBitVec 9 . untie

instance Enum BitVec10 where
  toEnum = tie . toBitVec 10
  fromEnum = fromBitVec 10 . untie

instance Enum BitVec11 where
  toEnum = tie . toBitVec 11
  fromEnum = fromBitVec 11 . untie

instance Enum BitVec12 where
  toEnum = tie . toBitVec 12
  fromEnum = fromBitVec 12 . untie

instance Enum BitVec13 where
  toEnum = tie . toBitVec 13
  fromEnum = fromBitVec 13 . untie

instance Enum BitVec14 where
  toEnum = tie . toBitVec 14
  fromEnum = fromBitVec 14 . untie

instance Enum BitVec15 where
  toEnum = tie . toBitVec 15
  fromEnum = fromBitVec 15 . untie

instance Enum BitVec16 where
  toEnum = tie . toBitVec 16
  fromEnum = fromBitVec 16 . untie

instance Num BitVec2 where fromInteger = toEnum . fromIntegral
instance Num BitVec3 where fromInteger = toEnum . fromIntegral
instance Num BitVec4 where fromInteger = toEnum . fromIntegral
instance Num BitVec5 where fromInteger = toEnum . fromIntegral
instance Num BitVec6 where fromInteger = toEnum . fromIntegral
instance Num BitVec7 where fromInteger = toEnum . fromIntegral
instance Num BitVec8 where fromInteger = toEnum . fromIntegral
instance Num BitVec9 where fromInteger = toEnum . fromIntegral
instance Num BitVec10 where fromInteger = toEnum . fromIntegral
instance Num BitVec11 where fromInteger = toEnum . fromIntegral
instance Num BitVec12 where fromInteger = toEnum . fromIntegral
instance Num BitVec13 where fromInteger = toEnum . fromIntegral
instance Num BitVec14 where fromInteger = toEnum . fromIntegral
instance Num BitVec15 where fromInteger = toEnum . fromIntegral
instance Num BitVec16 where fromInteger = toEnum . fromIntegral

instance Arbitrary BitVec2 where
  arbitrary = toEnum . fromIntegral . (`mod` 4) <$> (arbitrary :: Gen Word8)

instance Arbitrary BitVec3 where
  arbitrary = toEnum . fromIntegral . (`mod` 8) <$> (arbitrary :: Gen Word8)

instance Arbitrary BitVec4 where
  arbitrary = toEnum . fromIntegral . (`mod` 16) <$> (arbitrary :: Gen Word8)

instance Arbitrary BitVec5 where
  arbitrary = toEnum . fromIntegral . (`mod` 32) <$> (arbitrary :: Gen Word8)

instance Arbitrary BitVec6 where
  arbitrary = toEnum . fromIntegral . (`mod` 64) <$> (arbitrary :: Gen Word8)

instance Arbitrary BitVec7 where
  arbitrary = toEnum . fromIntegral . (`mod` 128) <$> (arbitrary :: Gen Word8)

instance Arbitrary BitVec8 where
  arbitrary = toEnum . fromIntegral <$> (arbitrary :: Gen Word8)

instance Arbitrary BitVec9 where
  arbitrary = toEnum . fromIntegral . (`mod` 512) <$> (arbitrary :: Gen Word16)

instance Arbitrary BitVec10 where
  arbitrary = toEnum . fromIntegral . (`mod` 1024) <$> (arbitrary :: Gen Word16)

instance Arbitrary BitVec11 where
  arbitrary = toEnum . fromIntegral . (`mod` 2048) <$> (arbitrary :: Gen Word16)

instance Arbitrary BitVec12 where
  arbitrary = toEnum . fromIntegral . (`mod` 4096) <$> (arbitrary :: Gen Word16)

instance Arbitrary BitVec13 where
  arbitrary = toEnum . fromIntegral . (`mod` 8192) <$> (arbitrary :: Gen Word16)

instance Arbitrary BitVec14 where
  arbitrary = toEnum . fromIntegral . (`mod` 16384) <$> (arbitrary :: Gen Word16)

instance Arbitrary BitVec15 where
  arbitrary = toEnum . fromIntegral . (`mod` 32768) <$> (arbitrary :: Gen Word16)

instance Arbitrary BitVec16 where
  arbitrary = toEnum . fromIntegral <$> (arbitrary :: Gen Word16)
