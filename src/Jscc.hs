-- JavaScript Contract Compiler
module Main where

import System.Console.GetOpt
import System.Environment
import System.Directory
import System.FilePath
import Control.Monad

import JsContracts.Compiler ( compile' )
import JsContracts.Parser

import WebBits.Common ( pp )
import WebBits.JavaScript.Parser ( parseJavaScriptFromFile )
import Text.PrettyPrint.HughesPJ ( render )

data Flag
  = Encapsulate
  | EnableContracts
  deriving (Show)
      

options ::  [ OptDescr Flag ]
options =
  [ Option ['e'] ["with-encapsulation"]  (NoArg Encapsulate)
      "encapsulate, ignoring all contracts"
  , Option ['c'] ["with-contracts"] (NoArg EnableContracts)
      "enable contracts and encapsulate (default)"
  ]

usage = usageInfo
  "jscc options implementation.js\n" options

main = do
  args <- getArgs
  let (opts, nonOpts, errors) = getOpt RequireOrder options args
  unless (null errors) $ do
    mapM_ putStrLn  errors
    fail "jscc terminated"
  (compilerMode, opts) <- getCompilerMode opts
  when (not $ null opts) $ do
    putStrLn $ "spurious arguments: " ++ (show opts)
    fail "jscc terminated"
  case nonOpts of
    [implPath] -> do
      let ifacePath = addExtension (dropExtension implPath) "jsi"
      exists <- doesFileExist implPath
      unless exists $ do
        fail $ "could not find " ++ implPath
      exists <- doesFileExist ifacePath
      unless exists $ do
        fail $ "could not find " ++ ifacePath
      impl  <- parseJavaScriptFromFile implPath
      iface <- parseInterface ifacePath
      result <- compile' impl iface
      putStrLn $ render $ pp result
      return () 
    otherwise -> do
      putStrLn "expected a single filename.js"
      fail "jscc terminated"

getCompilerMode (Encapsulate:rest) = return (Encapsulate,rest)
getCompilerMode (EnableContracts:rest) = return (EnableContracts,rest)
getCompilerMode rest = return (EnableContracts,rest)
