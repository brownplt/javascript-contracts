module JsContracts.Parser
  ( interface
  , parseInterface
  ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Pos
import WebBits.JavaScript.Lexer
import WebBits.JavaScript.Parser (parseSimpleExpr', ParsedExpression, 
  parseBlockStmt)
import JsContracts.Types

{-
  interface = interfaceItem *

  interfaceItem = identifier :: contract;
                | identifier = contract;
                | blockStmt
  
  function = nonFunction * -> function
           | nonFunction

  nonFunction = flat
              | contractLabel
              | object
              | ( function )

  flat = jsExpr

  object = { identifier : contract ,* }

-}

jsExpr = parseSimpleExpr'

contract :: CharParser st Contract
contract =  function

function :: CharParser st Contract
function = do
  pos <- getPosition
  args <- nonFunction `sepBy` whiteSpace
  case args of
    [] -> do
      reserved "->"
      result <- function
      return (FunctionContract pos [] result)
    [arg] -> (do reserved "->"
                 result <- function
                 return (FunctionContract pos [arg] result)) <|>
             return arg -- nonfunction
    args' -> do
      reserved "->"
      result <- function
      return (FunctionContract pos args' result)

nonFunction = parens function <|> object <|> flat

field :: CharParser st (String,Contract)
field = do
  id <- identifier
  reservedOp ":"
  ctc <- contract
  return (id,ctc)
  

object :: CharParser st Contract
object = do
  pos <- getPosition
  fields <- braces $ field `sepBy1` (reservedOp ",")
  return (ObjectContract pos fields) 
  
flat :: CharParser st Contract
flat = do
  pos <- getPosition
  expr <- jsExpr <?> "JavaScript expression"
  return (FlatContract pos expr)
       
interface :: CharParser st [InterfaceItem]
interface =
  (do id <- identifier
      item <- (do reservedOp "::"
                  ctc <- contract
                  return (InterfaceExport id ctc)) <|>
              (do reservedOp "="
                  ctc <- contract
                  return (InterfaceAlias id ctc))
      reservedOp ";"
      rest <- interface
      return (item:rest)) <|>
  (do stmt <- parseBlockStmt
      rest <- interface
      return $ (InterfaceStatement stmt):rest) <|>
  (return [])
  

parseInterface :: String -> IO [InterfaceItem]
parseInterface filename = do
  chars <- readFile filename
  case parse interface filename chars of
    Left err      -> fail (show err)
    Right exports -> return exports
