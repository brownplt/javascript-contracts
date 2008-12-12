module JsContracts.Template 
  ( JavaScriptTemplate
  , exprTemplate
  , substExpr
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

substExpr :: String -- ^free identifier
          -> ParsedExpression -- ^expression to substitute
          -> JavaScriptTemplate
          -> JavaScriptTemplate
substExpr id expr (ExpressionTemplate body) = 
  ExpressionTemplate (everywhere (mkT subst) body) where
    subst (VarRef _ (Id _ id')) | id' == id = expr
    subst expr = expr

