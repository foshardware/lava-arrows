
module Lava.Agnostic.VHDL where

import Lava.Agnostic.Netlist
import Lava.Agnostic.Primitive

import Control.Applicative
import Data.List
import Data.Set
import System.IO


putAgnosticVHDL :: Netlist -> IO ()
putAgnosticVHDL netlist = do
  putStr $ "Generating "++ filename ++" ..."
  file <- openFile filename WriteMode
  hPutStrLn file $ replicate 79 '-'
  hPutStrLn file $ "-- Automatically generated by LavaArrows"
  hPutStrLn file $ replicate 79 '-'
  hPutStrLn file ""
  hPutStrLn file . concat $ primitiveEntity <$> toList (usedPrims netlist)
  hPutStrLn file $ replicate 79 '-'
  hPutStrLn file . primitivePackage $ usedPrims netlist
  hPutStrLn file $ replicate 79 '-'
  hPutStrLn file $ circuitToVHDL netlist
  hPutStrLn file . unlines $ primitiveArchitecture <$> toList (usedPrims netlist)
  hPutStrLn file $ replicate 79 '-'
  hClose file
  putStrLn " [done]"
  where
  filename = circuitName netlist ++".vhd"

circuitToVHDL :: Netlist -> String
circuitToVHDL netlist = unlines $
  (circuitToVHDL <$> subCircuits netlist)
  ++
  [ vhdlPackage name $ ports netlist
  , vhdlEntity name $ ports netlist
  , vhdlArchitecture netlist
  , replicate 79 '-', ""
  ]
  where
  name = circuitName netlist
  filename = name++".vhd"

vhdlPackage :: String -> [PortDeclaration] -> String
vhdlPackage name interface = unlines
  [ "", "library ieee ;"
  , "use ieee.std_logic_1164.all ;"
  , "package " ++ name ++ "_package is"
  , "  component " ++ name ++ " is"
  , "    port(" ++ vhdlPorts (reverse interface)
  , "        ) ;"
  , "  end component " ++ name ++ " ;"
  , "end package " ++ name ++ "_package ;"
  ]
    
vhdlEntity :: String -> [PortDeclaration] -> String 
vhdlEntity name interface = unlines
  [ "library ieee ;"
  , "use ieee.std_logic_1164.all ;"
  , "use work." ++ name ++ "_package.all ;"
  , "entity " ++ name ++ " is"
  , "  port(" ++ vhdlPorts (reverse interface)
  , "      ) ;"
  , "end entity " ++ name ++ " ;"
  ]

vhdlPorts :: [PortDeclaration] -> String
vhdlPorts intf = intercalate " ;\n       " [vhdlPort p | p <- intf, isNotKept p]

vhdlPort :: PortDeclaration -> String
vhdlPort (Port name  InputPort typ net) = "signal " ++ name ++ " : in " ++ showType typ
vhdlPort (Port name OutputPort typ net) = "signal " ++ name ++ " : out " ++ showType typ

isNotKept :: PortDeclaration -> Bool
isNotKept (Port _  LocalInput _ _) = False
isNotKept (Port _ LocalOutput _ _) = False
isNotKept other = True

showType :: NetType -> String
showType ClockType = "std_logic"
showType BitType = "std_logic"
showType (BitVec low dir high) =
  "std_logic_vector (" ++ show low ++ " " ++ showDir dir ++ " " ++ show high ++ ")"
showType (NamedType typeName) = typeName

showDir :: Dir -> String
showDir To = "to"
showDir Downto = "downto"

vhdlArchitecture :: Netlist -> String
vhdlArchitecture netlist = unlines $
  [ "library ieee ;"
  , "use ieee.std_logic_1164.all ;"
  ] ++ ["use work." ++ circuitName nl ++ "_package.all ;" | nl <- subCircuits netlist]
  ++
  [ "use work.primitiveLava_package.all ;"
  , "architecture lava of " ++ circuitName netlist ++ " is "
  , netDefs
  ] ++ declareKeptPorts (ports netlist)
  ++
  ["begin"]
  ++ (wirePort <$> ports netlist)
  ++ (vhdlInstance <$> instances netlist)
  ++ ["end architecture lava ;"]
  where
  netDefs :: String
  netDefs = if netCount netlist == 1
            then "  -- no local Lava nets"
            else "  signal net : std_logic_vector (0 to " ++ show (netCount netlist - 1) ++ ") ;"

declareKeptPorts :: [PortDeclaration] -> [String]
declareKeptPorts [] = [] 
declareKeptPorts ((Port name LocalOutput typ nets):rest) =
  ["  signal " ++ name ++ " :  " ++ showType typ ++ " ;"]
  ++ declareKeptPorts rest
  ++ ["  attribute keep of " ++ name ++ " : signal is \"true\" ;"]
declareKeptPorts ((Port name LocalInput typ nets):rest) =
  [ "  signal " ++ name ++ " :  " ++ showType typ ++ " ;"
  , "  attribute keep of " ++ name ++ " : signal is \"true\" ;"
  ] ++ declareKeptPorts rest
declareKeptPorts (_:rest) = declareKeptPorts rest

wirePort :: PortDeclaration -> String
wirePort (Port name InputPort BitType [net]) = "  net(" ++ show net ++ ") <= " ++ name ++ ";"
wirePort (Port name InputPort (BitVec a dir b) nets) = unlines
  [ "  net(" ++ show net ++ ") <= " ++ name ++ "(" ++ show i ++ ")" ++ ";"
  | (i, net) <- zip (portRange a dir b) nets
  ]
wirePort (Port name OutputPort BitType [net]) = "  " ++ name ++ " <= net(" ++show  net ++ ") ;"
wirePort (Port name OutputPort (BitVec a dir b) nets) = unlines
  [ "  " ++ name ++ "(" ++ show i ++ ") <= net(" ++show  net ++ ") ;"
  | (i, net) <- zip (portRange a dir b) nets
  ]
wirePort e = error $ "cannot print VHDL from " ++ show e ++ " in wirePort :: PortDeclaration -> String"

vhdlInstance :: Instance -> String
vhdlInstance (Instance (Gnd g) name number) =
  "  " ++ name ++ "_" ++ show number ++ " : " ++ name ++ " port map (g => net(" ++ show g ++ ")); "
vhdlInstance (Instance (Vcc p) name number) =
  "  " ++ name ++ "_" ++ show number ++ " : " ++ name ++ " port map (p => net(" ++ show p ++ ")); "
vhdlInstance (Instance (MuxCY s d c o) name number) =
  "  " ++ name ++ "_" ++ show number ++ " : " ++ name
  ++ " port map (ci => net(" ++ show c ++ ")"
  ++ ", di => net(" ++ show d ++ ")"
  ++ ", s => net(" ++ show s ++ ")"
  ++ ", o => net(" ++ show o ++ "));"
vhdlInstance (Instance (XorCY c l o) name number) =
  "  " ++ name ++ "_" ++ show number ++ " : " ++ name
  ++ " port map (ci => net(" ++ show c ++ ")"
  ++ ", li => net(" ++ show l ++ ")"
  ++ ", o => net(" ++ show o ++ "));"
vhdlInstance (Instance (And2 i0 i1 o) name number) =
  "  " ++ name ++ "_" ++ show number ++ " : " ++ name
  ++ " port map (i0 => net(" ++ show i0 ++ ")"
  ++ ", i1 => net(" ++ show i1 ++ ")"
  ++ ", o => net(" ++ show o ++ "));"
vhdlInstance (Instance (Or2 i0 i1 o) name number) =
  "  " ++ name ++ "_" ++ show number ++ " : " ++ name
  ++ " port map (i0 => net(" ++ show i0 ++ ")"
  ++ ", i1 => net(" ++ show i1 ++ ")"
  ++ ", o => net(" ++ show o ++ "));"
vhdlInstance (Instance (Inv i o) name number) =
  "  " ++ name ++ "_" ++ show number ++ " : " ++ name
  ++ " port map (i0 => net(" ++ show i ++ ")"
  ++ ", o => net(" ++ show o ++ "));"
vhdlInstance e = error $ "cannot print VHDL from " ++ show e ++ " in vhdlInstance :: Instance -> String"

primitivePackage :: Set UsedPrimitive -> String
primitivePackage usedComponents = unlines $
  [ "", "library ieee ;"
  , "use ieee.std_logic_1164.all ;"
  , "package primitiveLava_package is"
  ] ++ (primitiveComponent <$> toList usedComponents)
  ++ ["end package primitiveLava_package ;"]

primitiveEntity :: UsedPrimitive -> String
primitiveEntity UVcc = unlines
  [ "library ieee ;"
  , "use ieee.std_logic_1164.all ;"
  , "entity vcc is"
  , "  port("
  , "    p : out std_ulogic := '1'"
  , "    );"
  , "end vcc;"
  ]
primitiveEntity UGnd = unlines
  [ "library ieee ;"
  , "use ieee.std_logic_1164.all ;"
  , "entity gnd is"
  , "  port("
  , "    g : out std_ulogic := '0'"
  , "  );"
  , "end gnd;", ""
  ]
primitiveEntity UMuxCY = unlines
  [ "library ieee ;"
  , "use ieee.std_logic_1164.all ;"
  , "entity muxcy is"
  , "  port("
  , "    o : out std_ulogic;"
  , "    ci : in std_ulogic;"
  , "    di : in std_ulogic;"
  , "    si : in std_ulogic"
  , "    );"
  , "end muxcy;", ""
  ]
primitiveEntity UXorCY = unlines
  [ "library ieee ;"
  , "use ieee.std_logic_1164.all ;"
  , "entity xorcy is"
  , "  port("
  , "    o : out std_ulogic;"
  , "    ci : in std_ulogic;"
  , "    li : in std_ulogic"
  , "    );"
  , "end xorcy;", ""
  ]
primitiveEntity UAnd2 = unlines
  [ "library ieee ;"
  , "use ieee.std_logic_1164.all ;"
  , "entity and2 is"
  , "  port("
  , "    o : out std_ulogic;"
  , "    i0 : in std_ulogic;"
  , "    i1 : in std_ulogic"
  , "    );"
  , "end and2;", ""
  ]
primitiveEntity UOr2 = unlines
  [ "library ieee ;"
  , "use ieee.std_logic_1164.all ;"
  , "entity or2 is"
  , "  port("
  , "    o : out std_ulogic;"
  , "    i0 : in std_ulogic;"
  , "    i1 : in std_ulogic"
  , "    );"
  , "end or2;", ""
  ]
primitiveEntity UInv = unlines
  [ "library ieee ;"
  , "use ieee.std_logic_1164.all ;"
  , "entity inv is"
  , "  port("
  , "    o : out std_ulogic;"
  , "    i : in std_ulogic;"
  , "    );"
  , "end inv;", ""
  ]
primitiveEntity e = error $
  "cannot print VHDL from " ++ show e ++ "  in primitiveEntity :: UsedPrimitive -> String"

primitiveComponent :: UsedPrimitive -> String
primitiveComponent UGnd = unlines
  [ "  component gnd is"
  , "    port(g : out std_ulogic"
  , "         ) ;"
  , "  end component gnd;"
  ]
primitiveComponent UVcc = unlines
  [ "  component vcc is"
  , "    port(p : out std_ulogic"
  , "        ) ;"
  , "  end component vcc;"
  ]
primitiveComponent UXorCY = unlines
  [ "  component xorcy is"
  , "    port(o : out std_ulogic;"
  , "         ci : in std_ulogic;"
  , "         li : in std_ulogic"
  , "        ) ;"
  , "  end component xorcy;"
  ]
primitiveComponent UAnd2 = unlines
  [ "  component and2 is"
  , "    port(o : out std_ulogic;"
  , "         i0 : in std_ulogic;"
  , "         i1 : in std_ulogic"
  , "        ) ;"
  , "  end component and2;"
  ]
primitiveComponent UOr2 = unlines
  [ "  component or2 is"
  , "    port(o : out std_ulogic;"
  , "         i0 : in std_ulogic;"
  , "         i1 : in std_ulogic"
  , "        ) ;"
  , "  end component or2;"
  ]
primitiveComponent UInv = unlines
  [ "  component inv is"
  , "    port(o : out std_ulogic;"
  , "         i : in std_ulogic;"
  , "        ) ;"
  , "  end component inv;"
  ]
primitiveComponent e = error $
  "cannot print VHDL from " ++ show e ++ "  in primitiveComponent :: UsedPrimitive -> String"

primitiveArchitecture :: UsedPrimitive -> String
primitiveArchitecture UVcc = unlines
  [ "architecture vcc_v of vcc is"
  , "begin"
  , "  p <= '1';"
  , "end vcc_v;"
  ]
primitiveArchitecture UGnd = unlines
  [ "architecture gnd_v of gnd is"
  , "begin"
  , "  g <= '0';"
  , "end gnd_v;"
  ]
primitiveArchitecture UMuxCY = unlines
  [ "architecture muxcy_v of muxcy is"
  , "begin"
  , "  VITALBehavior : process (ci, di, s)"
  , "  begin"
  , "    if (s = '0') then"
  , "      o <= di;"
  , "    else"
  , "      o <= ci;"
  , "    end if;"
  , "  end process;"
  , "end muxcy_v;"
  ]
primitiveArchitecture UXorCY = unlines
  [ "architecture xorcy_v of xorcy is"
  , "begin"
  , "  o <= (ci xor li);"
  , "end xorcy_v;"
  ]
primitiveArchitecture UAnd2 = unlines
  [ "architecture and2_v of and2 is"
  , "begin"
  , "  o <= (i0 and i1);"
  , "end and2_v;"
  ]
primitiveArchitecture UOr2 = unlines
  [ "architecture or2_v of or2 is"
  , "begin"
  , "  o <= (i0 or i1);"
  , "end or2_v;"
  ]
primitiveArchitecture UInv = unlines
  [ "architecture inv_v of inv is"
  , "begin"
  , "  o <= (not i);"
  , "end xorcy_v;"
  ]
primitiveArchitecture e = error $
  "cannot print VHDL from " ++ show e ++ "  in primitiveArchitecture :: UsedPrimitive -> String"
