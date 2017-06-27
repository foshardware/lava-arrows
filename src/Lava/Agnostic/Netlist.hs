
module Lava.Agnostic.Netlist where

import Lava.Agnostic.Primitive

import Data.Set as Set


data Dir = To | Downto deriving (Eq, Show)

portRange :: Int -> Dir -> Int -> [Int]
portRange a To b = [a..b]
portRange a Downto b = [b..a]

data NetType
  = ClockType
  | BitType
  | BitVec Int Dir Int
  | ArrayType Int Dir Int NetType
  | NamedType String
  deriving (Eq, Show)

data PortDirection
  = InputPort
  | OutputPort
  | LocalInput
  | LocalOutput
  deriving (Eq, Show)

data PortDeclaration = Port String PortDirection NetType [Int] deriving Show

data Instance = Instance
  { primitive      :: Primitive
  , instanceName   :: String
  , instanceNumber :: Int
  } deriving (Eq, Show)

data Netlist = Netlist
  { circuitName :: String
  , ports       :: [PortDeclaration]
  , instances   :: [Instance]
  , netCount    :: Int
  , instCount   :: Int
  , usedPrims   :: Set UsedPrimitive
  , subCircuits :: [Netlist]
  }

emptyNetlist :: Netlist
emptyNetlist = Netlist "" [] [] 0 0 Set.empty []

