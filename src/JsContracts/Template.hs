module JsContracts.Template 
  ( JavaScriptTemplate
  , exprTemplate
  , stmtTemplate
  , substVar
  , substVarList
  , substIdList
  , substFieldList
  , expandCall
  , templateExpression
  , noPos
  , thunkExpr
  , renderTemplate
  ) where

import Data.Data
import Data.Generics
import Text.ParserCombinators.Parsec (parse, many1)
import Text.ParserCombinators.Parsec.Pos (initialPos, SourcePos)
import Text.PrettyPrint.HughesPJ (render)
import WebBits.JavaScript.PrettyPrint ()
import WebBits.Common (pp)
import WebBits.JavaScript.Parser
import WebBits.JavaScript.Syntax
import WebBits.JavaScript.Instances()


noPos :: SourcePos
noPos = initialPos "template"

thunkExpr :: ParsedExpression -> ParsedExpression
thunkExpr expr = FuncExpr noPos [] (ReturnStmt noPos (Just expr))

-- We may extend this later so that template definitions explicitly
-- state their free identifiers
data JavaScriptTemplate
  = ExpressionTemplate ParsedExpression
  | StatementTemplate [ParsedStatement]

exprTemplate :: String -> JavaScriptTemplate
exprTemplate str = case parse parseAssignExpr "expression template" str of
  Left err -> error ("Error parsing template: " ++ show err ++ 
                     "; template:\n\n" ++ str)
  Right expr -> ExpressionTemplate expr

stmtTemplate :: String -> JavaScriptTemplate
stmtTemplate str = case parse (many1 parseStatement) "statement template" str of
  Left err -> error ("Error parsing template: " ++ show err ++ 
                     "; template:\n\n" ++ str)
  Right stmts -> StatementTemplate stmts
  
renderTemplate :: JavaScriptTemplate -> String
renderTemplate (ExpressionTemplate expr) = render (pp expr)
renderTemplate (StatementTemplate stmts) = concat $ map (render.pp) stmts

templateExpression :: JavaScriptTemplate -> ParsedExpression
templateExpression (ExpressionTemplate expr) = expr

expandCall :: String -- ^function name to expand
           -> ([ParsedExpression] -> [ParsedExpression]) -- ^argument expander
           -> JavaScriptTemplate
           -> JavaScriptTemplate
expandCall functionId expander (StatementTemplate body) =
  StatementTemplate (everywhere (mkT subst) body) where
    subst (CallExpr p1 fn@(VarRef p2 (Id p3 id')) args) 
      | id' == functionId = CallExpr p1 fn (expander args)
    subst expr = expr

substVar :: String -- ^free identifier
         -> ParsedExpression -- ^expression to substitute
         -> JavaScriptTemplate
         -> JavaScriptTemplate
substVar id expr (ExpressionTemplate body) = 
  ExpressionTemplate (everywhere (mkT subst) body) where
    subst (VarRef _ (Id _ id')) | id' == id = expr
    subst expr = expr

substVarList :: String -- ^identifier in a list
             -> [ParsedExpression] -- ^list of expressions to substitute
             -> JavaScriptTemplate
             -> JavaScriptTemplate
substVarList id exprs (ExpressionTemplate body) =
  ExpressionTemplate (everywhere (mkT subst) body) where
    subst [VarRef _ (Id _ id')] | id' == id = exprs
    subst lst = lst

substIdList :: String -- ^identifier in a list
            -> [String] -- ^list of identifiers to substitute
            -> JavaScriptTemplate
            -> JavaScriptTemplate
substIdList id ids (ExpressionTemplate body) =
  ExpressionTemplate (everywhere (mkT subst) body) where
    subst [Id _ id'] | id' == id = map (Id noPos) ids
    subst lst = lst

substFieldList :: String -- ^ placeholder field name
               -> [(String,ParsedExpression)] -- ^list of fields
               -> JavaScriptTemplate
               -> JavaScriptTemplate
substFieldList fieldId fields (ExpressionTemplate body) = 
  ExpressionTemplate (everywhere (mkT subst) body) where
    fields' = map (\(name,expr) -> (PropId noPos (Id noPos name),expr)) fields
    subst [(PropId _ (Id _ id'), _)] | id' == fieldId = fields'
    subst lst = lst
      

