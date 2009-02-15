module Language.JavaScript.Contracts
  ( Contract (..)
  , InterfaceItem (..)
  , compile
  , getContractLibraryPath
  ) where

import System.FilePath
import Paths_JsContracts -- created by Cabal

import JsContracts.Types
import JsContracts.Compiler

getContractLibraryPath :: IO FilePath
getContractLibraryPath = do
  dataDir <- getDataDir
  return $ dataDir </> "contracts.js"
