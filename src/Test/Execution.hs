-- |Tests execution of code generated by the contract compiler by running it
-- on Rhino.
--
-- jscc must already be built.  Adjust the rhinoJarPath as appropriate.
-- 
module Test.Execution where

import Test.HUnit.Base
import Test.HUnit.Text
import System.Cmd
import System.Exit
import JsContracts.Compiler

rhinoJarPath = "/Users/arjun/.local/rhino/js.jar"

testExecution :: String -- module.js
              -> String -- interactions.js
              -> Assertion
testExecution moduleJs interactionsJs = do
  impl <- compile moduleJs (moduleJs ++ "i")
  interactions <- readFile interactionsJs
  let js = "window = { };" ++ impl ++ "with(window) { \n" ++
           interactions ++ "}\n"
  code <- rawSystem "java" ["-classpath",rhinoJarPath, 
                            "org.mozilla.javascript.tools.shell.Main",
                            "-e",js]
  case code of
    ExitSuccess -> return ()
    ExitFailure _ -> do
      putStrLn js
      assertFailure "failed"


testCalls = TestLabel "test function calls" $ TestCase $ do
  testExecution "Test/moduleFunctions.js" "Test/testCalls.js"
 
  return ()
  
allTests = TestList
  [ testCalls
  ]

main = return allTests