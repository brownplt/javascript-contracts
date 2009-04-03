module BrownPLT.JavaScript.Contracts
  ( Contract (..)
  , InterfaceItem (..)
  , compile
  , compileFormatted
  , compileRelease
  , parseInterface
  , getContractLibraryPath
  ) where

import System.FilePath
import Paths_JsContracts -- created by Cabal

import BrownPLT.JavaScript.Contracts.Types
import BrownPLT.JavaScript.Contracts.Compiler
import BrownPLT.JavaScript.Contracts.Parser

getContractLibraryPath :: IO FilePath
getContractLibraryPath = do
  dataDir <- getDataDir
  return $ dataDir </> "contracts.js"
