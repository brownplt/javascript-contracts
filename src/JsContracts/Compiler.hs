module JsContracts.Compiler where

import Text.ParserCombinators.Parsec.Pos (initialPos, SourcePos)
import WebBits.JavaScript.Parser (ParsedExpression, ParsedStatement,
  parseJavaScriptFromFile)
import WebBits.JavaScript.Syntax
import WebBits.JavaScript.PrettyPrint ()
import WebBits.Common (pp)
import Text.PrettyPrint.HughesPJ (render)
import JsContracts.Types
import JsContracts.Parser
import JsContracts.Template

noPos :: SourcePos
noPos = initialPos "JsContracts.Compiler"

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
             -> ParsedStatement -- ^encapsulated implementation
encapsulate impl interface =
  let wrappedImpl = wrapImplementation impl
      interfaceStmts = map interfaceStatement $ 
        filter isInterfaceStatement interface
      interfaceExports = filter isInterfaceExport interface
      exportStmts = map makeExportStatement interfaceExports
      outerWrapper = ParenExpr noPos $ FuncExpr noPos [] $ BlockStmt noPos $ 
        wrappedImpl ++ interfaceStmts ++ exportStmts
    in ExprStmt noPos $ CallExpr noPos outerWrapper []

compile :: String -- ^implementation file
        -> String -- ^interface file
        -> IO String -- ^encapsulated library (as a string)
compile implementationPath interfacePath = do
  impl <- parseJavaScriptFromFile implementationPath
  interface <- parseInterface interfacePath
  let encapsulated = encapsulate impl interface
  return $ render $ pp encapsulated 

compileContract :: String -- ^export name
                -> Contract -> ParsedExpression -> ParsedExpression
compileContract exportId contract guardExpr =
  CallExpr noPos (cc contract) [guardExpr]

flatTemplate =  exprTemplate
  "(function(val) { \
  \   if (pred(val)) { \
  \     return val;    \
  \   }                \
  \   else {           \
  \     throw \"flat contract violation\"; \
  \   }                                    \
  \ })"

functionTemplate = exprTemplate
  "(function(proc) { \
  \   if (typeof(proc) == \"function\") { \
  \     return wrappedProc; \
  \   } \
  \   else { \
  \     throw \"function contract violation\"; \
  \   } \
  \ })"

objectTemplate = exprTemplate
  "(function(val) { \
  \   return wrappedObj; \
  \ });"

-- |Core contract compiler
cc :: Contract -> ParsedExpression
cc (FlatContract _ predExpr) =  
  templateExpression (substExpr "pred" predExpr flatTemplate)
cc (FunctionContract _ domainContracts rangeContract) = 
  let isProc = InfixExpr noPos OpEq 
        (PrefixExpr noPos PrefixTypeof (VarRef noPos $ Id noPos "proc")) 
        (StringLit noPos "function")
      argIds = map (\n -> Id noPos $ "arg" ++ show n) $ 
                 map fst (zip [0..] domainContracts)
      checkedCall = CallExpr noPos (VarRef noPos $ Id noPos "proc") $
                      map (\(argId,ctc) -> CallExpr noPos (cc ctc) 
                                          [VarRef noPos argId])
                        (zip argIds domainContracts)
      wrapProc = FuncExpr noPos argIds $ ReturnStmt noPos $ Just $
                   CallExpr noPos (cc rangeContract) [checkedCall]
    in templateExpression (substExpr "wrappedProc" wrapProc functionTemplate)
cc (ObjectContract _ fields) = 
  let getField id = DotRef noPos (VarRef noPos $ Id noPos "val") (Id noPos id)
      mkProp id = PropId noPos (Id noPos id) 
      fieldContract (id,contract) = 
        (mkProp id, CallExpr noPos (cc contract) [getField id])
    in templateExpression $ substExpr "wrappedObj" 
         (ObjectLit noPos (map fieldContract fields)) objectTemplate
