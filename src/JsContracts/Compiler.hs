module JsContracts.Compiler
  ( compile
  , compile'
  , compileFormatted
  , compileRelease
  ) where

import Control.Monad

import qualified Data.Map as M

import Text.PrettyPrint.HughesPJ ( render, vcat )
import Text.ParserCombinators.Parsec.Pos (SourcePos)
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

-- Given a namespace, e.g. flapjax, we get
--
-- window.flapjax = { };
-- for (var ix in impl) {
--   window.flapjax[ix] = impl[ix]; }
exportNamespace :: String
                -> [ParsedStatement]
exportNamespace namespace = [decl,loop] where
  ix = VarRef noPos (Id noPos "ix")
  window_namespace = 
           (DotRef noPos (VarRef noPos (Id noPos "window")) 
                         (Id noPos namespace))
  decl = ExprStmt noPos $ AssignExpr noPos OpAssign window_namespace
           (ObjectLit noPos [])
  loop = ForInStmt noPos (ForInVar (Id noPos "ix")) 
           (VarRef noPos (Id noPos "impl")) $ ExprStmt noPos $
             AssignExpr noPos OpAssign (BracketRef noPos window_namespace ix)
               (BracketRef noPos (VarRef noPos (Id noPos "impl")) ix)

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
    where (_, globalMap,_,_) = staticEnvironment impl
          allGlobals = M.keys globalMap
          exportedGlobals = filter (`elem` exportNames) allGlobals 


makeExportStatements :: InterfaceItem -> [ParsedStatement]
makeExportStatements (InterfaceExport id pos contract) = 
  [ ExprStmt noPos $ AssignExpr noPos OpAssign 
      (DotRef noPos (ThisRef noPos) (Id noPos id))
      (compileContract id contract pos $ 
         DotRef noPos (VarRef noPos (Id noPos "impl")) (Id noPos id))
  ]
-- allows external code to use "instanceof id"
makeExportStatements (InterfaceInstance id _ _) =
  [ ExprStmt noPos $ AssignExpr noPos OpAssign 
      (DotRef noPos (ThisRef noPos) (Id noPos id))
      (DotRef noPos (VarRef noPos (Id noPos "impl")) (Id noPos id))
  ]
makeExportStatements _ = [ ]

exportRelease :: InterfaceItem -> ParsedStatement
exportRelease (InterfaceExport id _ contract) = 
  ExprStmt noPos $ AssignExpr noPos OpAssign 
    (DotRef noPos (ThisRef noPos) (Id noPos id))
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
  init (InterfaceInstance id _ _) = templateStatements -- same as above
    $ renameVar "alias" id (stmtTemplate "var alias = { };")
  init _ =  []
  def (InterfaceAlias id contract) = templateStatements
    $ renameVar "alias" id
    $ substVar "contract" (cc contract)
    (stmtTemplate "(function() { var tmp = contract;\n \
                  \              alias.client = tmp.client;\n \
                  \              alias.flat = tmp.flat; \
                  \              alias.server = tmp.server; })(); \n")
  def (InterfaceInstance id loc contract) = templateStatements
    $ renameVar "alias" id
    $ substVar "contract" (cc contract)
    $ substVar "name" (StringLit noPos id)
    $ substVar "constr" (DotRef noPos (VarRef noPos (Id noPos "impl"))
                                (Id noPos id))
    (stmtTemplate "(function() { \
                  \   var tmp = contracts.instance(name)(constr,contract); \
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
      exportNames = [n | InterfaceExport n _ _ <- interfaceExports]
      instanceNames = [ n | InterfaceInstance n _ _
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
               -> Maybe String -- ^the namespace name
               -> String -- ^encapsulated implementation
compileRelease rawImpl implSource boilerplate interface namespace =
  libraryHeader ++ (concat $ map (render.pp) $ escapeGlobals impl exportNames) 
    ++ rawImpl ++ exposeStatements ++ "\n}).apply(impl,[]);\n" 
    ++ exportStatements ++ namespaceStatements ++ "\n})();" where
     impl = case parseScriptFromString implSource rawImpl of
              Left err -> error (show err)
              Right (Script _ stmts) -> stmts
     exports = filter isInterfaceExport interface
     exportStatements = render $ vcat $
       map (pp.exportRelease) exports
     exportNames = [n | InterfaceExport n _ _ <- exports ]
     instanceNames = 
       [n | InterfaceInstance n _ _ <- filter isInterfaceInstance interface]
     exposeStatements = render $ vcat $ 
       map pp (exposeImplementation (exportNames ++ instanceNames))
     namespaceStatements = case namespace of
       Nothing -> ""
       Just s -> render $ vcat $ map pp $ exportNamespace s

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
     exportNames = [n | InterfaceExport n _ _ <- exports ]
     aliases = filter isInterfaceAlias interface
     aliasStatements = concatMap (render.pp) $ compileAliases interface
     instanceNames = 
       [n | InterfaceInstance n _ _ <- filter isInterfaceInstance interface]
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
                -> Contract -- ^ contract
                -> SourcePos -- ^ location of export
                -> ParsedExpression 
                -> ParsedExpression
compileContract exportId contract pos guardExpr =
  CallExpr noPos (DotRef noPos (VarRef noPos (Id noPos "contracts")) 
                         (Id noPos "guard"))
   [cc contract, guardExpr, StringLit noPos exportId, 
    StringLit noPos "client", StringLit noPos (show (contractPos contract))]
     where loc = "on " ++ exportId ++ ", defined at " ++ show pos



-- |contract name compiler
nc :: Contract
   -> String
nc (FlatContract pos predExpr) = 
  "value that satisfies the predicate at " ++ show pos

-- TODO: hygiene.  Use an extended annotation (Either SourcePos ...) to
-- determine whether or not to substitute into a template.
-- |contract compiler

cc :: Contract -- ^parsed contract
   -> ParsedExpression -- ^contract in JavaScript
cc ctc = case ctc of
  FlatContract _ predExpr -> templateExpression
    $ substVar "pred" predExpr 
    $ substVar "name" (StringLit noPos (nc ctc))
    $ exprTemplate "contracts.flat(name)(pred)"
  FunctionContract pos domainContracts (Just restContract) rangeContract ->
    templateExpression
      $ substVar "restArg" (cc restContract)
      $ substVar "result" (cc rangeContract)
      $ substVarList "fixedArgs" 
          (map cc domainContracts) 
      $ substVar "name" (StringLit noPos $ "function at " ++ show pos)
      $ exprTemplate 
          "contracts.varArityFunc(name)([fixedArgs],restArg,result)"
  FunctionContract pos domainContracts Nothing rangeContract ->
    let isUndefined = DotRef noPos (VarRef noPos (Id noPos "contracts"))
                           (Id noPos "isUndefined")
      in templateExpression
           $ substVar "restArg" isUndefined
           $ substVar "result" (cc rangeContract)
           $ substVarList "fixedArgs" (map cc domainContracts) 
           $ substVar "name" (StringLit noPos $ "function at " ++ show pos)
           $ exprTemplate 
                "contracts.varArityFunc(name)([fixedArgs],restArg,result)"
  -- User-defined contract constructor
  ConstructorContract pos constrName args -> CallExpr noPos
    (CallExpr noPos
              (VarRef noPos (Id noPos constrName)) 
              [StringLit noPos constrName])
    (map cc args)
  FixedArrayContract pos elts -> templateExpression
    $ substVarList "contracts" (map cc elts) 
    $ substVar "name" (StringLit noPos "fixed-array")
    $ exprTemplate "contracts.fixedArray(name)(contracts)"
  ArrayContract _ elt -> templateExpression
    $ substVar "contract" (cc elt) 
    $ substVar "name" (StringLit noPos "array")
    $ exprTemplate "contracts.unsizedArray(name)(contract)"
  ObjectContract pos fields ->
    let getField id = DotRef noPos (VarRef noPos $ Id noPos "val") (Id noPos id)
        mkProp id = PropId noPos (Id noPos id) 
        fieldContract (id,contract) = (id, cc contract)
      in templateExpression 
           $ substFieldList "fieldNames" (map fieldContract fields) 
           $ substVar "name" (StringLit noPos "name")
           $ exprTemplate "contracts.obj(name)({ fieldNames: 42 })"
  NamedContract _ nameRef ->
    VarRef noPos (Id noPos nameRef)
