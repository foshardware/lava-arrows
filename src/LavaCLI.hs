
module Main where

import Control.Applicative
import Data.Char
import System.IO
import System.Directory
import System.Environment


cabalContents :: String -> String
cabalContents name = unlines
  [ "name:                "++ map toLower name
  , "version:             0.0.0.0"
  , "synopsis:            "
  , "description:         "
  , ""
  , "license:             OtherLicense"
  , "license-file:        LICENSE"
  , "category:            Hardware"
  , "build-type:          Simple"
  , "cabal-version:       >=1.18"
  , ""
  , "Executable           lava"
  , "  hs-source-dirs:    ."
  , "  main-is:           "++ name ++".hs"
  , "  build-depends:     base >= 4.7, lava-arrows"
  , "  ghc-options:       -Wall -O2 -fno-warn-orphans -fno-warn-unused-do-bind"
  , "  default-language:  Haskell2010"
  ]
  
data LavaType = Xilinx | Generic deriving Show

hsContents :: String -> LavaType -> (Int, Int) -> String
hsContents name typ (inputCount, outputCount) = unlines
  [ "{-# LANGUAGE Arrows, TypeOperators #-}"
  , ""
  , "module Main where"
  , ""
  , "import Lava."++ show typ
  , "", ""
  , "type (->>) = "++ show typ
  , ""
  , "main :: IO ()"
  , "main = put"++ show typ ++"VHDL $ evalNetlist \""++ name ++"\" topLevel"++ name
  , ""
  , "topLevel"++ name ++" :: () ->> ()"
  , "topLevel"++ name ++" = proc () -> do"
  , "  input  <- "++ inputPorts inputCount ++ " -< ()"
  , "  output <- "++ decapitalize name ++" -< input"
  , "  "++ outputPorts outputCount ++ " -< output"
  , ""
  ]
  where
  decapitalize (l:rest) = toLower l : rest
  decapitalize l = l
  inputPorts c | c  > 1 = "inputBitVec \"in\" (BitVec "++ show (c - 1) ++" Downto 0)"
  inputPorts c | c == 1 = "inputPort \"in\""
  inputPorts _ = error "<= 0 input ports"
  outputPorts c | c  > 1 = "outputBitVec \"out\" (BitVec "++ show (c - 1) ++" Downto 0)"
  outputPorts c | c == 1 = "outputPort \"out\""
  outputPorts _ = error "<= 0 output ports"

helpMessage :: String
helpMessage = ""

main :: IO ()
main = getArgs >>= \args -> case args of
  ("init" : rest) -> lavaInit rest
  _ -> putStrLn helpMessage

lavaInit :: [String] -> IO ()
lavaInit _ = do
  name <- promptDefault "Circuit" "Name [Circuit]: "
  let cabalFileName = map toLower name ++".cabal"; hsFileName = name ++".hs"
  exists <- (||) <$> doesFileExist cabalFileName <*> doesFileExist hsFileName
  if exists then putStrLn "Project files exist already." else do
    typ <- promptDefault "Generic" "Type [Generic]: "
    inputCount  <- promptDefault "2" "Input Ports [2]: "
    outputCount <- promptDefault "2" "Output Ports [2]: "
    cabalFile <- openFile cabalFileName WriteMode
    hPutStr cabalFile $ cabalContents name
    hClose cabalFile
    hsFile <- openFile hsFileName WriteMode
    hPutStr hsFile $ hsContents name (lavaType typ) (read inputCount, read outputCount)
    hClose hsFile
  where
  lavaType "Xilinx" = Xilinx
  lavaType _ = Generic

promptDefault :: String -> String -> IO String
promptDefault def s = prompt s >>= \res -> return $ if null res then def else res

prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

