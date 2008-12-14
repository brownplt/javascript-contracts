module Parser where

import Test.HUnit.Base
import Test.HUnit.Text

import JsContracts.Parser
import JsContracts.Compiler

import WebBits.JavaScript.Parser


testParse = TestLabel "test parsing a file" $ TestCase $ do
  parseInterface "basic.jsi"
  return ()
 
testCompile = TestLabel "compiling basic.jsi" $ TestCase $ do
    impl    <- parseJavaScriptFromFile "basic.js"
    iface   <- parseInterface "basic.jsi"
    compile' impl iface
    return ()

allTests = TestList
  [ testParse
  , testCompile
  ]

main = return allTests
