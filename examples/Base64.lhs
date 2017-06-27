> {-# LANGUAGE Arrows, TypeOperators #-}

For arrow notation to work we need language pragma of `Arrows`.
We also make use of type operators to declare new components
where a component of type (a ->> b) corresponds to the following illustration:
      ___
 --->|   |--->
  a  |___| b

> module Base64 where

Modules can be compiled to executables, for instance:

    $ runhaskell Base64.lhs

> import qualified Data.ByteString as B
> import qualified Data.ByteString.Base64 as B64
> import Data.Word
> import Lava.BitVec
> import Lava.Xilinx
> import Lava.Xilinx.Virtex6

> import Prelude hiding (take, drop, tail)
> import Test.QuickCheck

> type (->>) = Xilinx

The main function is the place to generate, test and export netlists to files. In this case
two different .vhd files: base64Encode.vhd and base64Decode.vhd from two
top level components: topLevelDecode and topLevelEncode respectively.

> main :: IO ()
> main = do
>   putXilinxVHDL $ evalNetlist "base64Encode" Virtex6 topLevelEncode
>   putXilinxVHDL $ evalNetlist "base64Decode" Virtex6 topLevelDecode

>   quickCheckWith stdArgs { maxSuccess = 1000 } $ \(a,b,c) ->
>     let (o, p, q, r) = test Nothing base64Encode (k' a, k' b, k' c)
>     in B.pack (k <$> [o, p, q, r]) == B64.encode (B.pack [a,b,c])

>   quickCheckWith stdArgs { maxSuccess = 1000 } $ \(a',b',c') ->
>     let [a, b, c, d] = fmap k' . B.unpack . B64.encode $ B.pack [a',b',c']
>         (o, p, q) = test Nothing base64Decode (a, b, c, d)
>     in (k <$> [o, p, q]) == [a', b', c']

>   where
>     k  = numFromBitVec  :: BitVec8 -> Word8
>     k' = bitVecFromEnum :: Word8 -> BitVec8

We can only generate netlists from _closed_ logical blocks, those are blocks of type:

> topLevelEncode :: () ->> ()

In a closed logical block you should declare only necessary components like
input and output ports. It is not possible to test a closed block, because it has no input.

> topLevelEncode = proc () -> do

As the block is closed, we just process the unary operator () which denotes to no input wires.

The following initializes three input bytes and applies the component `base64Encode` to them,
which returns four bytes that are put out. The block is now closed.

>   byte0 <- inputBitVec "byte0" (BitVec 7 Downto 0) -< ()
>   byte1 <- inputBitVec "byte1" (BitVec 7 Downto 0) -< ()
>   byte2 <- inputBitVec "byte2" (BitVec 7 Downto 0) -< ()
>   (out0, out1, out2, out3) <- base64Encode -< (byte0, byte1, byte2)
>   outputBitVec "out0" (BitVec 7 Downto 0) -< out0
>   outputBitVec "out1" (BitVec 7 Downto 0) -< out1
>   outputBitVec "out2" (BitVec 7 Downto 0) -< out2
>   outputBitVec "out3" (BitVec 7 Downto 0) -< out3

`base64Encode` divides 24 input bits in four equally sized parts and applies `base64Value`
on each part.
Lava arrows denote to from left to right in synthesis;
The `***` operator allows vertical composition of components.

> base64Encode :: (BitVec8, BitVec8, BitVec8) ->> (BitVec8, BitVec8, BitVec8, BitVec8)
> base64Encode = proc (byte0, byte1, byte2) -> do
>   (c0, c1) <- base64Value *** base64Value -< (take n6 byte0, drop n6 byte0 +:+ take n4 byte1)
>   (c2, c3) <- base64Value *** base64Value -< (drop n4 byte1 +:+ take n2 byte2, drop n2 byte2)
>   returnA -< (c0, c1, c2, c3)

> base64Value :: BitVec6 ->> BitVec8
> base64Value = lutify $ proc c -> do
>   slash     <- comparator -< (c, 63)
>   plus      <- comparator -< (c, 62)
>   digit     <- comparator -< (c, 52)
>   lowerCase <- comparator -< (c, 26)
>   minus :. summand <- switch
>     [slash, plus, digit, lowerCase] [1 :. 16, 1 :. 19, 1 :. 4, 0 :. 71] -<< 0 :. 65
>   (0 :.) ^<< adderSubtractor -< (minus, (0 :. c, summand))

> topLevelDecode :: () ->> ()
> topLevelDecode = proc () -> do
>   byte0 <- inputBitVec "byte0" (BitVec 7 Downto 0) -< ()
>   byte1 <- inputBitVec "byte1" (BitVec 7 Downto 0) -< ()
>   byte2 <- inputBitVec "byte2" (BitVec 7 Downto 0) -< ()
>   byte3 <- inputBitVec "byte3" (BitVec 7 Downto 0) -< ()
>   (out0, out1, out2) <- base64Decode -< (byte0, byte1, byte2, byte3)
>   outputBitVec "out0" (BitVec 7 Downto 0) -< out0
>   outputBitVec "out1" (BitVec 7 Downto 0) -< out1
>   outputBitVec "out2" (BitVec 7 Downto 0) -< out2

> base64Decode :: (BitVec8, BitVec8, BitVec8, BitVec8) ->> (BitVec8, BitVec8, BitVec8)
> base64Decode = proc (byte0, byte1, byte2, byte3) -> do
>   (val0, val1) <- base64Char *** base64Char -< (byte0, byte1)
>   (val2, val3) <- base64Char *** base64Char -< (byte2, byte3)
>   returnA -< (val0 +:+ take n2 val1, drop n2 val1 +:+ take n4 val2, drop n4 val2 +:+ val3)

> base64Char :: BitVec8 ->> BitVec6
> base64Char = lutify $ proc b -> do
>   lowerCase <- comparator -< (tail b, 97)
>   upperCase <- comparator -< (tail b, 65)
>   digit     <- comparator -< (tail b, 48)
>   slash     <- comparator -< (tail b, 47)
>   minus :. summand <- switch
>     [lowerCase, upperCase, digit, slash] [1 :. 71, 1 :. 65, 0 :. 4, 0 :. 16] -<< 0 :. 19
>   tail ^<< adderSubtractor -< (minus, (tail b, summand))

