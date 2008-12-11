module Test.Parser where

import Test.HUnit.Base
import Test.HUnit.Text
import JsContracts.Parser

testParsing = TestLabel "test parsing a file" $ TestCase $ do
  parseInterface "Test/basic.jsi"
  return ()
 
  
allTests = TestList
  [ testParsing
  ]

main = return allTests
