module JsContracts.Compiler 
  ( compile
  , compile'
  ) where

import Control.Monad

import qualified Data.Map as M

import Paths_JsContracts -- created by Cabal
import System.FilePath ((</>))
import WebBits.JavaScript.Parser (ParsedExpression, ParsedStatement,
  parseJavaScriptFromFile)
import WebBits.JavaScript.Environment
import WebBits.JavaScript.Syntax
import WebBits.JavaScript.PrettyPrint ()
import WebBits.Common (pp)
import JsContracts.Types
import JsContracts.Parser
import JsContracts.Template



--wrapImplementation :: [ParsedStatement] -> [ParsedStatement]
wrapImplementation impl names = 
  [VarDeclStmt noPos [implDecl], ExprStmt noPos callThunkedImpl] where
    implDecl = VarDecl noPos (Id noPos "impl") (Just $ ObjectLit noPos [])
    implExport = 
        [ExprStmt noPos $ AssignExpr noPos OpAssign
            (DotRef noPos (VarRef noPos (Id noPos "impl")) (Id noPos n))
            (VarRef noPos (Id noPos n))
            | n <- names ]
    callThunkedImpl = CallExpr noPos $ ParenExpr noPos $ FuncExpr noPos 
      [Id noPos "impl"] (BlockStmt noPos (impl ++ implExport))
        [VarRef noPos (Id noPos "impl")]
  
escapeGlobals :: [ParsedStatement] -> [ParsedStatement]
escapeGlobals impl = 
  [VarDeclStmt noPos [VarDecl noPos (Id noPos s) Nothing] | s <- M.keys globals]
    where (_, globals, _) = staticEnvironment impl


makeExportStatement :: InterfaceItem -> ParsedStatement
makeExportStatement (InterfaceExport id contract) = 
  ExprStmt noPos $ AssignExpr noPos OpAssign 
    (DotRef noPos (VarRef noPos (Id noPos "window")) (Id noPos id))
    (compileContract id contract $ 
       DotRef noPos (VarRef noPos (Id noPos "impl")) (Id noPos id))
makeExportStatement _ = error "makeExportStatement: expected InterfaceItem"

aliasTemplate = stmtTemplate
  "var alias = { }; \
  \(function() { \
  \   var tmp = contract; \
  \   alias.client = tmp.client; \
  \   alias.server = tmp.server; \
  \})();"

compileAlias :: InterfaceItem -> [ParsedStatement]
compileAlias (InterfaceAlias id contract) = templateStatements 
  $ renameVar "alias"  id
  $ substVar "contract" (cc contract)
  aliasTemplate
compileAlias _ = error "compileAlias: expected an InterfaceAlias"

compile :: [ParsedStatement]  -- ^implementation
        -> [InterfaceItem]   -- ^the interface
        -> [ParsedStatement] -- ^contract library
        -> ParsedStatement -- ^encapsulated implementation
compile impl interface boilerplateStmts =
  let exportStmts = map makeExportStatement interfaceExports
      exportNames = [n | InterfaceExport n _ <- interfaceExports]
      aliases = filter isInterfaceAlias interface
      aliasStmts = concatMap compileAlias aliases
      wrappedImpl = wrapImplementation (escapeGlobals impl ++ impl) exportNames
      interfaceStmts = map interfaceStatement $ 
        filter isInterfaceStatement interface
      interfaceExports = filter isInterfaceExport interface

      outerWrapper = ParenExpr noPos $ FuncExpr noPos [] $ BlockStmt noPos $ 
        wrappedImpl ++ boilerplateStmts ++ interfaceStmts ++ aliasStmts ++
        exportStmts
    in ExprStmt noPos $ CallExpr noPos outerWrapper []

compile' :: [ParsedStatement] -> [InterfaceItem] -> IO ParsedStatement
compile' impl iface  = liftM (compile impl iface) boilerplate

boilerplate :: IO [ParsedStatement]
boilerplate = getDataDir >>= parseJavaScriptFromFile . (</> "contracts.js")
    
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
cc (NamedContract _ name) = VarRef noPos (Id noPos name)
