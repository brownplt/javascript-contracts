-- JavaScript Contract Compiler
module Main where

import System.Console.GetOpt
import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import Control.Monad

import JsContracts.Compiler
import JsContracts.Parser
import Paths_JsContracts -- created by Cabal

import WebBits.Common ( pp )
import WebBits.JavaScript.Parser ( parseJavaScriptFromFile )
import Text.PrettyPrint.HughesPJ ( render )

data Flag
  = Help
  | Release
  | Debug
  | Interface String
  deriving (Eq,Ord,Show)
      

options ::  [ OptDescr Flag ]
options =
  [ Option ['h'] ["help"] (NoArg Help)
      "display this help message"
  , Option ['r'] ["release"]  (NoArg Release)
      "encapsulate, ignoring all contracts"
  , Option ['d'] ["debug"] (NoArg Debug)
      "enable contracts and encapsulate (default)"
  , Option ['i'] ["interface"] (ReqArg Interface "PATH")
      "path to the interface; uses module.jsi by default"
  ]

usage = usageInfo
  "Usage: jscc [options] module.js\nOptions:\n" options

main = do
  args <- getArgs
  dataDir <- getDataDir
  let (opts, nonOpts, errors) = getOpt RequireOrder options args
  unless (null errors) $ do
    mapM_ putStrLn  errors
    fail "jscc terminated"
  checkHelp opts
  (isDebugMode, opts) <- getDebugMode opts
  (ifacePath, opts) <- getInterfacePath opts nonOpts
  when (not $ null opts) $ do
    putStrLn $ "spurious arguments: " ++ (show opts)
    fail "jscc terminated"
  case nonOpts of
    [implPath] -> do
      checkFile implPath
      rawImpl <- readFile implPath
      let boilerplatePath = dataDir </> "contracts.js"
      rawBoilerplate <- readFile boilerplatePath
      interface <- parseInterface ifacePath
      let result = if isDebugMode
                     then compileFormatted rawImpl implPath rawBoilerplate 
                            interface
                     else compileRelease rawImpl implPath rawBoilerplate
                            interface
      putStrLn result
      return () 
    otherwise -> do
      putStrLn "expected a single filename.js"
      fail "jscc terminated"

checkFile path = do
  exists <- doesFileExist path
  unless exists $ do
    putStrLn $ "could not find the file: " ++ path
    exitFailure

getDebugMode (Release:rest) = return (False,rest)
getDebugMode (Debug:rest) = return (True,rest)
getDebugMode rest = return (True,rest)

checkHelp (Help:_) = do
  putStrLn usage
  exitSuccess
checkHelp _ = return ()

getInterfacePath :: [Flag] -> [String] -> IO (FilePath,[Flag])
getInterfacePath (Interface path:rest) _ = do
  checkFile path
  return (path,rest)
getInterfacePath rest (implPath:_) = do
  let path = addExtension (dropExtension implPath) "jsi"
  checkFile path
  return (path,rest)
getInterfacePath _ [] = do
  putStrLn "Invalid arguments (use -h for help)"
  exitFailure
