module JsContracts.Compiler 
  ( compile
  ) where

import Paths_JsContracts -- created by Cabal
import System.FilePath ((</>))
import WebBits.JavaScript.Parser (ParsedExpression, ParsedStatement,
  parseJavaScriptFromFile)
import WebBits.JavaScript.Syntax
import WebBits.JavaScript.PrettyPrint ()
import WebBits.Common (pp)
import Text.PrettyPrint.HughesPJ (render)
import JsContracts.Types
import JsContracts.Parser
import JsContracts.Template


wrapImplementation :: [ParsedStatement]
                   -> [ParsedStatement]
wrapImplementation impl =
  let f = ParenExpr noPos $ FuncExpr noPos [] (BlockStmt noPos impl)
      apply = DotRef noPos f (Id noPos "apply")
      def = CallExpr noPos apply [VarRef noPos (Id noPos "impl"), 
                                  ArrayLit noPos []]
      decl = VarDecl noPos (Id noPos "impl") (Just $ ObjectLit noPos [])
    in [VarDeclStmt noPos [decl], ExprStmt noPos def]

makeExportStatement :: InterfaceItem -> ParsedStatement
makeExportStatement (InterfaceExport (Export id contract)) = 
  ExprStmt noPos $ AssignExpr noPos OpAssign 
    (DotRef noPos (VarRef noPos (Id noPos "window")) (Id noPos id))
    (compileContract id contract $ 
       DotRef noPos (VarRef noPos (Id noPos "impl")) (Id noPos id))
makeExportStatement _ = error "makeExportStatement: expected InterfaceItem"


encapsulate :: [ParsedStatement]  -- ^implementation
            -> [InterfaceItem]   -- ^the interface
            -> [ParsedStatement] -- ^contract library
            -> ParsedStatement -- ^encapsulated implementation
encapsulate impl interface boilerplateStmts =
  let wrappedImpl = wrapImplementation impl
      interfaceStmts = map interfaceStatement $ 
        filter isInterfaceStatement interface
      interfaceExports = filter isInterfaceExport interface
      exportStmts = map makeExportStatement interfaceExports
      outerWrapper = ParenExpr noPos $ FuncExpr noPos [] $ BlockStmt noPos $ 
        wrappedImpl ++ boilerplateStmts ++ interfaceStmts ++ exportStmts
    in ExprStmt noPos $ CallExpr noPos outerWrapper []

compile :: String -- ^implementation file
        -> String -- ^interface file
        -> IO String -- ^encapsulated library (as a string)
compile implementationPath interfacePath = do
  dataDir <- getDataDir
  impl <- parseJavaScriptFromFile implementationPath
  boilerplate <- parseJavaScriptFromFile (dataDir</>"contracts.js")
  interface <- parseInterface interfacePath
  let encapsulated = encapsulate impl interface boilerplate
  return $ render $ pp encapsulated


compileContract :: String -- ^export name
                -> Contract -> ParsedExpression -> ParsedExpression
compileContract exportId contract guardExpr =
  CallExpr noPos (DotRef noPos (VarRef noPos (Id noPos "contracts")) 
                         (Id noPos "guard"))
   [cc contract, guardExpr, StringLit noPos "server", StringLit noPos "client"]


flatTemplate :: JavaScriptTemplate
flatTemplate = exprTemplate "contracts.flat(pred)"

functionTemplate :: JavaScriptTemplate
functionTemplate = exprTemplate "contracts.func(contracts)"

objectTemplate :: JavaScriptTemplate
objectTemplate = exprTemplate "contracts.obj({ fieldNames: 42 })"

-- |Core contract compiler
cc :: Contract -> ParsedExpression
cc (FlatContract _ predExpr) =  
  templateExpression
    $ substVar "pred" predExpr flatTemplate
cc (FunctionContract _ domainContracts rangeContract) = 
  let argContracts = map cc domainContracts
      contracts = argContracts ++ [cc rangeContract]
    in templateExpression
         $ substVarList "contracts" contracts functionTemplate
cc (ObjectContract _ fields) = 
  let getField id = DotRef noPos (VarRef noPos $ Id noPos "val") (Id noPos id)
      mkProp id = PropId noPos (Id noPos id) 
      fieldContract (id,contract) = (id, cc contract)
    in templateExpression 
         $ substFieldList "fieldNames" (map fieldContract fields) objectTemplate
