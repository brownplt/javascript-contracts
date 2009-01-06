module JsContracts.Compiler
  ( compile
  , compile'
  , compileFormatted
  , compileRelease
  ) where

import Control.Monad

import qualified Data.Map as M

import Text.PrettyPrint.HughesPJ ( render, vcat )
import Paths_JsContracts -- created by Cabal
import System.FilePath ((</>))
import WebBits.JavaScript.Parser (ParsedExpression, ParsedStatement,
  parseJavaScriptFromFile, parseScriptFromString )
import WebBits.JavaScript.Environment
import WebBits.JavaScript.Syntax
import WebBits.JavaScript.PrettyPrint ()
import WebBits.Common (pp)
import JsContracts.Types
import JsContracts.Parser
import JsContracts.Template


-- Given the name foo, we get
--
-- try {
--   // allow the implementation to export to this directly
--   if (this.foo !== undefined) {
--     this.foo = foo;
--   }
-- }
-- catch (_) {
--   // in case the local variable foo is undefined
-- }
exposeImplementation :: [String]
                     -> [ParsedStatement]
exposeImplementation names = map export names where
  var n = VarRef noPos (Id noPos n)
  undef = VarRef noPos (Id noPos "undefined")
  export n = TryStmt noPos 
    (IfSingleStmt noPos (InfixExpr noPos OpStrictNEq (var n) undef)
       (ExprStmt noPos $ AssignExpr noPos OpAssign
          (DotRef noPos (ThisRef noPos) (Id noPos n)) 
          (var n)))
    [CatchClause noPos (Id noPos "_") (EmptyStmt noPos)]
    Nothing

wrapImplementation :: [ParsedStatement] -> [String] -> [ParsedStatement]
wrapImplementation impl names = 
  [VarDeclStmt noPos [implDecl], ExprStmt noPos callThunkedImpl] where
    implDecl = VarDecl noPos (Id noPos "impl") (Just $ ObjectLit noPos [])
    implExport = exposeImplementation names
    callThunkedImpl = CallExpr noPos 
      (DotRef noPos 
         (ParenExpr noPos $ FuncExpr noPos [Id noPos "impl"] 
           (BlockStmt noPos (impl ++ implExport)))
         (Id noPos "apply"))
      [VarRef noPos (Id noPos "impl")]
  
escapeGlobals :: [ParsedStatement] -> [String] -> [ParsedStatement]
escapeGlobals impl exportNames = 
  [VarDeclStmt noPos [VarDecl noPos (Id noPos s) Nothing] | s <- exportedGlobals]
    where (_, globalMap, _) = staticEnvironment impl
          allGlobals = M.keys globalMap
          exportedGlobals = filter (`elem` exportNames) allGlobals 


makeExportStatements :: InterfaceItem -> [ParsedStatement]
makeExportStatements (InterfaceExport id contract) = 
  [ ExprStmt noPos $ AssignExpr noPos OpAssign 
      (DotRef noPos (VarRef noPos (Id noPos "window")) (Id noPos id))
      (compileContract id contract $ 
         DotRef noPos (VarRef noPos (Id noPos "impl")) (Id noPos id))
  ]
-- allows external code to use "instanceof id"
makeExportStatements (InterfaceInstance id _) =
  [ ExprStmt noPos $ AssignExpr noPos OpAssign 
      (DotRef noPos (VarRef noPos (Id noPos "window")) (Id noPos id))
      (DotRef noPos (VarRef noPos (Id noPos "impl")) (Id noPos id))
  ]
makeExportStatements _ = [ ]

exportRelease :: InterfaceItem -> ParsedStatement
exportRelease (InterfaceExport id contract) = 
  ExprStmt noPos $ AssignExpr noPos OpAssign 
    (DotRef noPos (VarRef noPos (Id noPos "window")) (Id noPos id))
    (DotRef noPos (VarRef noPos (Id noPos "impl")) (Id noPos id))
exportRelease _ = error "exportRelease: expected InterfaceItem"


-- Given source that reads:
--
-- foo = contract;
-- ...
--
-- Transform it to:
--
-- var foo = { }; ...
-- (function() {
--    var tmp = contract;
--    foo.client = tmp.client;
--    foo.server = tmp.server;
-- })();
--
-- The names are first initialized to empty object to permit mutually-recursive
-- contract definitions.
compileAliases :: [InterfaceItem] -> [ParsedStatement]
compileAliases aliases = concatMap init aliases ++ concatMap def aliases where
  init (InterfaceAlias id _) = templateStatements
    $ renameVar "alias" id (stmtTemplate "var alias = { };")
  init (InterfaceInstance id _) = templateStatements -- same as above
    $ renameVar "alias" id (stmtTemplate "var alias = { };")
  init _ =  []
  def (InterfaceAlias id contract) = templateStatements
    $ renameVar "alias" id
    $ substVar "contract" (cc id contract)
    (stmtTemplate "(function() { var tmp = contract;\n \
                  \              alias.client = tmp.client;\n \
                  \              alias.flat = tmp.flat; \
                  \              alias.server = tmp.server; })(); \n")
  def (InterfaceInstance id contract) = templateStatements
    $ renameVar "alias" id
    $ substVar "contract" (cc id contract)
    $ substVar "constrId" (StringLit noPos id)
    $ substVar "constr" (DotRef noPos (VarRef noPos (Id noPos "impl"))
                                (Id noPos id))
    (stmtTemplate "(function() { \
                  \   var tmp = contracts.instance(constr,constrId,contract); \
                  \   alias.client = tmp.client; \
                  \   alias.flat = tmp.flat;     \
                  \   alias.server = tmp.server; })(); ")
  def _ = []

compile :: [ParsedStatement]  -- ^implementation
        -> [InterfaceItem]   -- ^the interface
        -> [ParsedStatement] -- ^contract library
        -> ParsedStatement -- ^encapsulated implementation
compile impl interface boilerplateStmts =
  let exportStmts = concatMap makeExportStatements interface
      exportNames = [n | InterfaceExport n _ <- interfaceExports]
      instanceNames = [ n | InterfaceInstance n _
                              <- filter isInterfaceInstance interface ]
      aliases = filter isInterfaceAlias interface
      aliasStmts = compileAliases interface
      wrappedImpl = wrapImplementation (escapeGlobals impl exportNames ++ impl)
                                        (exportNames ++ instanceNames)
      interfaceStmts = map interfaceStatement $ 
        filter isInterfaceStatement interface
      interfaceExports = filter isInterfaceExport interface

      outerWrapper = ParenExpr noPos $ FuncExpr noPos [] $ BlockStmt noPos $ 
        wrappedImpl ++ boilerplateStmts ++ interfaceStmts ++ aliasStmts ++
        exportStmts
    in ExprStmt noPos $ CallExpr noPos outerWrapper []


libraryHeader =
  "(function () {\n \
  \   var impl = { };\n \
  \   (function() {\n"

compileRelease :: String -- ^implementation
               -> String -- ^implementation source
               -> String -- ^contract library
               -> [InterfaceItem] -- ^the interface
               -> String -- ^encapsulated implementation
compileRelease rawImpl implSource boilerplate interface =
  libraryHeader ++ (concat $ map (render.pp) $ escapeGlobals impl exportNames) 
    ++ rawImpl ++ exposeStatements ++ "\n}).apply(impl,[]);\n" 
    ++ exportStatements ++ "\n})();" where
     impl = case parseScriptFromString implSource rawImpl of
              Left err -> error (show err)
              Right (Script _ stmts) -> stmts
     exports = filter isInterfaceExport interface
     exportStatements = render $ vcat $
       map (pp.exportRelease) exports
     exportNames = [n | InterfaceExport n _ <- exports ]
     instanceNames = 
       [n | InterfaceInstance n _ <- filter isInterfaceInstance interface]
     exposeStatements = render $ vcat $ 
       map pp (exposeImplementation (exportNames ++ instanceNames))

compileFormatted :: String -- ^implementation
                 -> String -- ^implementation source
                 -> String -- ^contract library
                 -> [InterfaceItem] -- ^the interface
                 -> String -- ^encapsulated implementation
compileFormatted rawImpl implSource boilerplate interface =
  libraryHeader ++ (concat $ map (render.pp) $ escapeGlobals impl exportNames) 
    ++ rawImpl
    ++ exposeStatements ++ "\n}).apply(impl,[]);\n" ++ boilerplate 
    ++ interfaceStatements
    ++ aliasStatements ++ exportStatements
    ++ "\n})();" where
     impl = case parseScriptFromString implSource rawImpl of
              Left err -> error (show err)
              Right (Script _ stmts) -> stmts
     exports = filter isInterfaceExport interface
     exportStatements = render.vcat $ 
       concatMap (map pp.makeExportStatements) interface
     exportNames = [n | InterfaceExport n _ <- exports ]
     aliases = filter isInterfaceAlias interface
     aliasStatements = concatMap (render.pp) $ compileAliases interface
     instanceNames = 
       [n | InterfaceInstance n _ <- filter isInterfaceInstance interface]
     exposeStatements = concatMap (render.pp) $ 
       exposeImplementation (exportNames ++ instanceNames)
     interfaceStatements = render.vcat $ map (pp.interfaceStatement) $ 
       filter isInterfaceStatement interface
     
compile' :: [ParsedStatement] -> [InterfaceItem] -> IO ParsedStatement
compile' impl iface = do
  dataDir <- getDataDir
  boilerplateStmts <- parseJavaScriptFromFile (dataDir</>"contracts.js")
  return $ compile impl iface boilerplateStmts

    
compileContract :: String -- ^export name
                -> Contract -> ParsedExpression -> ParsedExpression
compileContract exportId contract guardExpr =
  CallExpr noPos (DotRef noPos (VarRef noPos (Id noPos "contracts")) 
                         (Id noPos "guard"))
   -- 
   [cc "" contract, guardExpr, StringLit noPos exportId, 
    StringLit noPos "client"]


functionTemplate :: JavaScriptTemplate
functionTemplate = exprTemplate 
  "contracts.varArityFunc([fixedArgs],restArg,result)"

fixedArrayTemplate = exprTemplate "contracts.fixedArray(contracts)"
arrayTemplate = exprTemplate "contracts.unsizedArray(contract)"

objectTemplate :: JavaScriptTemplate
objectTemplate = exprTemplate "contracts.obj({ fieldNames: 42 })"

-- TODO: hygiene.  Use an extended annotation (Either SourcePos ...) to
-- determine whether or not to substitute into a template.
-- |Core contract compiler
cc :: String -- ^human-readable name for the contract, not the guarded value
   -> Contract 
   -> ParsedExpression
cc name (FlatContract pos predExpr) =  
  templateExpression
    $ substVar "pred" predExpr 
    $ substVar "contractName" (StringLit noPos (name ++ " at " ++ show pos))
      (exprTemplate "contracts.flat(pred,contractName)")
cc name 
  (FunctionContract pos domainContracts (Just restContract) rangeContract) =
  templateExpression
    $ substVar "restArg" (cc (name ++ " at " ++ show pos) restContract)
    $ substVar "result" (cc (name ++ " at " ++ show pos) rangeContract)
    $ substVarList "fixedArgs" 
        (map (cc (name ++ " at " ++ show pos)) domainContracts) 
      functionTemplate
cc name (FunctionContract _ domainContracts Nothing rangeContract) = 
  let isUndefined = DotRef noPos (VarRef noPos (Id noPos "contracts"))
                           (Id noPos "isUndefined")
    in templateExpression
         $ substVar "restArg" isUndefined
         $ substVar "result" (cc name rangeContract)
         $ substVarList "fixedArgs" (map (cc name) domainContracts) 
           functionTemplate
cc name (ConstructorContract pos constrName args) =
  CallExpr noPos (VarRef noPos (Id noPos constrName)) 
  (map (cc (name ++ " at " ++ show pos)) args)
cc name (FixedArrayContract _ elts) =  templateExpression
  $ substVarList "contracts" (map (cc name) elts) fixedArrayTemplate 
cc name (ArrayContract _ elt) =  templateExpression
  $ substVar "contract" (cc name elt) arrayTemplate 
cc name (ObjectContract pos fields) = 
  let getField id = DotRef noPos (VarRef noPos $ Id noPos "val") (Id noPos id)
      mkProp id = PropId noPos (Id noPos id) 
      fieldContract (id,contract) = 
        (id, cc (name ++ " at " ++ show pos) contract)
    in templateExpression 
         $ substFieldList "fieldNames" (map fieldContract fields) objectTemplate
cc name (NamedContract _ nameRef) = VarRef noPos (Id noPos nameRef)
