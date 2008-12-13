-- |Tests execution of code generated by the contract compiler by running it
-- on Rhino.
--
-- The JsContracts library must be built and installed.  Rhino's js.jar must
-- be in your CLASSPATH.
module Execution where

import Test.HUnit.Base
import Test.HUnit.Text
import System.Cmd
import System.Exit
import JsContracts.Compiler
import JsContracts.Template

expandTests :: String -> String
expandTests testSuite = renderTemplate
  $ expandCall "testExn" expandTest
  $ expandCall "test" expandTest (stmtTemplate testSuite) where
    expandTest [try,expected] = [thunkExpr try, expected]
    expandTest _ = error "expandTests: invalid number of arguments to test"

testExecution :: String -- module.js
              -> String -- interactions.js
              -> Assertion
testExecution moduleJs interactionsJs = do
  impl <- compile moduleJs (moduleJs ++ "i")
  interactions <- readFile interactionsJs
  let js = "window = { };" ++ impl ++ "with(window) { \n" ++
           expandTests interactions ++ "}\n"
  code <- rawSystem "java" ["org.mozilla.javascript.tools.shell.Main","-e",js]
  case code of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      putStrLn js
      assertFailure "failed"


testCalls = TestLabel "test function calls" $ TestCase $ do
  testExecution "moduleFunctions.js" "testCalls.js"
 
  return ()
  
allTests = TestList
  [ testCalls
  ]

main = return allTests
