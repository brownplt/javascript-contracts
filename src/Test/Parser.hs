module Test.Parser where

import Test.HUnit.Base
import Test.HUnit.Text
import JsContracts.Parser
import JsContracts.Compiler

testParse = TestLabel "test parsing a file" $ TestCase $ do
  parseInterface "Test/basic.jsi"
  return ()
 
testCompile = TestLabel "compiling basic.jsi" $ TestCase $ do
  compile "Test/basic.js" "Test/basic.js"
  return ()
  
allTests = TestList
  [ testParse,
    testCompile
  ]

main = return allTests
