module BrownPLT.JavaScript.Contracts.Template 
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
  , renameVar
  , templateStatements
  ) where

import Data.Data
import Data.Generics
import Text.ParserCombinators.Parsec (parse, many1)
import Text.ParserCombinators.Parsec.Pos (initialPos, SourcePos)
import Text.PrettyPrint.HughesPJ (render)
import BrownPLT.JavaScript.PrettyPrint (renderExpression, renderStatements)
import BrownPLT.JavaScript.Parser
import BrownPLT.JavaScript.Syntax
import BrownPLT.JavaScript.Instances()
import BrownPLT.JavaScript.Crawl() -- hack for instance 

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
renderTemplate (ExpressionTemplate expr) = renderExpression expr
renderTemplate (StatementTemplate stmts) = renderStatements stmts

templateExpression :: JavaScriptTemplate -> ParsedExpression
templateExpression (ExpressionTemplate expr) = expr

templateStatements :: JavaScriptTemplate -> [ParsedStatement]
templateStatements (StatementTemplate stmts) = stmts

expandCall :: String -- ^function name to expand
           -> ([ParsedExpression] -> [ParsedExpression]) -- ^argument expander
           -> JavaScriptTemplate
           -> JavaScriptTemplate
expandCall functionId expander (StatementTemplate body) =
  StatementTemplate (everywhere (mkT subst) body) where
    subst (CallExpr p1 fn@(VarRef p2 (Id p3 id')) args) 
      | id' == functionId = CallExpr p1 fn (expander args)
    subst expr = expr

renameVar :: String -- ^original id
          -> String -- ^new id
          -> JavaScriptTemplate
          -> JavaScriptTemplate
renameVar idOld idNew body =
  let -- explicit signatures needed for generics
      substExpr :: ParsedExpression -> ParsedExpression
      substExpr (VarRef p1 (Id p2 thisId))
        | thisId == idOld = VarRef p1 (Id p2 idNew)
      substExpr v = v
      substDecl :: VarDecl SourcePos -> VarDecl SourcePos
      substDecl (VarDecl p1 (Id p2 thisId) val)
        | thisId == idOld = VarDecl p1 (Id p2 idNew) val
      substDecl v = v
    in case body of
         ExpressionTemplate body ->  ExpressionTemplate $
           everywhere ((mkT substDecl) . (mkT substExpr)) body
         StatementTemplate stmts -> StatementTemplate $
           everywhere (mkT substDecl . mkT substExpr) stmts

substVar :: String -- ^free identifier
         -> ParsedExpression -- ^expression to substitute
         -> JavaScriptTemplate
         -> JavaScriptTemplate
substVar id expr body = 
  let subst (VarRef _ (Id _ id')) | id' == id = expr
      subst expr = expr
    in case body of
         ExpressionTemplate body -> 
           ExpressionTemplate (everywhere (mkT subst) body)
         StatementTemplate stmts ->
           StatementTemplate (everywhere (mkT subst) stmts)

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
      

