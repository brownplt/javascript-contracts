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
    (DotRef noPos (VarRef noPos (Id noPos "impl")) (Id noPos id))
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

