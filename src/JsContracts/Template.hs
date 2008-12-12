module JsContracts.Template 
  ( JavaScriptTemplate
  , exprTemplate
  , substVar
  , substVarList
  , substIdList
  , substFieldList
  , templateExpression
  ) where

import Data.Data
import Data.Generics
import Text.ParserCombinators.Parsec (parse)
import Text.ParserCombinators.Parsec.Pos (initialPos, SourcePos)
import Text.PrettyPrint.HughesPJ (render)
import WebBits.JavaScript.PrettyPrint ()
import WebBits.Common (pp)
import WebBits.JavaScript.Parser
import WebBits.JavaScript.Syntax
import WebBits.JavaScript.Instances()


noPos :: SourcePos
noPos = initialPos "template"

-- We may extend this later so that template definitions explicitly
-- state their free identifiers
data JavaScriptTemplate = ExpressionTemplate ParsedExpression

exprTemplate :: String -> JavaScriptTemplate
exprTemplate str = case parse parseAssignExpr "template" str of
  Left err -> error ("Error parsing template: " ++ show err)
  Right expr -> ExpressionTemplate expr

renderTemplate :: JavaScriptTemplate -> String
renderTemplate (ExpressionTemplate expr) = render (pp expr)

templateExpression :: JavaScriptTemplate -> ParsedExpression
templateExpression (ExpressionTemplate expr) = expr

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
      

