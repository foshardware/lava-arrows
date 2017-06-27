
module Lava.Agnostic.BLIF where

import Lava.Agnostic.Netlist
import Lava.Agnostic.Primitive

import Control.Applicative
import Data.List as L
import Data.Set
import System.IO


putAgnosticBLIF :: Netlist -> IO ()
putAgnosticBLIF netlist = do
  putStr $ "Generating "++ filename ++" ..."
  file <- openFile filename WriteMode
  hPutStrLn file $ circuitToBLIF netlist
  hClose file
  putStrLn " [done]"
  where
  filename = circuitName netlist ++".blif"

circuitToBLIF :: Netlist -> String
circuitToBLIF netlist = unlines $
  (circuitToBLIF <$> subCircuits netlist)
  ++
  [ blifMainModel netlist ]
  ++
  L.filter (not . L.null) (blifPrimitive <$> toList (usedPrims netlist))

blifMainModel :: Netlist -> String
blifMainModel netlist = unlines $
  [ ".model "++ circuitName netlist
  , ".inputs w0 w1"++ (blifInputs $ ports netlist)
  , ".outputs "++ (blifOutputs $ ports netlist)
  , ".clock "++ (blifClock $ ports netlist)
  ]
  ++
  L.filter (not . L.null) (blifInstance <$> instances netlist)
  ++ 
  [ ".end" ]

blifInputs :: [PortDeclaration] -> String
blifInputs intf = intercalate " " [input p | p <- intf, isInput p]
  where
  isInput (Port _ _ ClockType _) = False
  isInput (Port _ InputPort _ _) = True
  isInput _ = False
  input (Port _ _ BitType [x]) = "w"++ show x
  input (Port _ _ (BitVec _ _ _) net) = intercalate " " (("w"++) . show <$> net)
  input _ = ""

blifOutputs :: [PortDeclaration] -> String
blifOutputs intf = intercalate " " [output p | p <- intf, isOutput p]
  where
  isOutput (Port _ OutputPort _ _) = True
  isOutput _ = False
  output (Port _ _ BitType [x]) = "w"++ show x
  output (Port _ _ (BitVec _ _ _) net) = intercalate " " (("w"++) . show <$> net)
  output _ = ""

blifClock :: [PortDeclaration] -> String
blifClock intf = intercalate " " [clock p | p <- intf, isClock p]
  where
  isClock (Port _ _ ClockType _) = True
  isClock _ = False
  clock (Port _ _ ClockType [x]) = "w"++ show x
  clock _ = ""

blifInstance :: Instance -> String
blifInstance (Instance (Vcc o) _ _) = ""
blifInstance (Instance (Gnd o) _ _) = ""
blifInstance (Instance (MuxCY s d c o) _ _) =
  ".muxcy w"++ show c ++" w"++ show d ++" w"++ show s ++" w"++ show o
blifInstance (Instance (XorCY c l o) _ _) =
  ".xorcy w"++ show c ++" w"++ show l ++" w"++ show o
blifInstance (Instance (And2 i0 i1 o) _ _) =
  ".and2 w"++ show i0 ++" w"++ show i1 ++" w"++ show o
blifInstance (Instance (Or2 i0 i1 o) _ _) =
  ".or2 w"++ show i0 ++" w"++ show i1 ++" w"++ show o
blifInstance (Instance (Inv i o) _ _) =
  ".inv w"++ show i ++" w"++ show o
blifInstance (Instance (Latch n i o) _ _) =
  ".latch w"++ show i ++" w"++ show o ++ " "++ show n
blifInstance (Instance (FlipFlopD n d q c) _ _) =
  ".latch w"++ show d ++" w"++ show q ++ " re w"++ show c ++" "++ show n
blifInstance (Instance (FlipFlopJK j k q c) _ _) =
  ".jk w"++ show j ++" w"++ show k ++ " w"++ show c ++" w"++ show q
blifInstance e = error $ "cannot print BLIF from " ++ show e ++ " in blifInstance :: Instance -> String"

blifPrimitive :: UsedPrimitive -> String
blifPrimitive UVcc = ""
blifPrimitive UGnd = ""
blifPrimitive UMuxCY = unlines
  [ ".model xorcy"
  , ".inputs c d s"
  , ".outputs o"
  , ".names c d s o"
  , "01- 1"
  , "1-1 1"
  , ".end"
  ]
blifPrimitive UXorCY = unlines
  [ ".model xorcy"
  , ".inputs c l"
  , ".outputs o"
  , ".names c l o"
  , "01 1"
  , "10 1"
  , ".end"
  ]
blifPrimitive UAnd2 = unlines
  [ ".model and2"
  , ".inputs a b"
  , ".outputs o"
  , ".names a b o"
  , "11 1"
  , ".end"
  ]
blifPrimitive UOr2 = unlines
  [ ".model or2"
  , ".inputs a b"
  , ".outputs o"
  , ".names a b o"
  , "01 1"
  , "10 1"
  , "11 1"
  , ".end"
  ]
blifPrimitive UInv = unlines
  [ ".model inv"
  , ".inputs i"
  , ".outputs o"
  , ".names i o"
  , "0 1"
  , ".end"
  ]
blifPrimitive UFlipFlopJK = unlines
  [ ".model jk"
  , ".inputs j kbar clk"
  , ".outputs out"
  , ".latch temp q re clk 1"
  , ".names j k q temp"
  , "-01 1"
  , "1-0 1"
  , ".names q out"
  , "0 1"
  , ".names kbar k"
  , "0 1"
  , ".end"
  ]
blifPrimitive e = error $
  "cannot print BLIF from " ++ show e ++ "  in blifPrimitive :: UsedPrimitive -> String"

