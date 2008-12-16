module JsContracts.Parser
  ( interface
  , parseInterface
  ) where

import qualified Data.List as L
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
           | nonFunction + ... -> function
           | nonFunction

  nonFunction = flat
              | :contractLabel
              | :customConstructor(contract ,*)
              | object
              | ( function )
              | [ contract ,* ] -- fixed length array

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
      return (FunctionContract pos [] Nothing result)
    [arg] -> (do reserved "->"
                 result <- function
                 return (FunctionContract pos [arg] Nothing result)) <|>
             (do reserved "..."
                 reserved "->"
                 result <- function
                 return (FunctionContract pos [] (Just arg) result)) <|>
             return arg -- nonfunction
    args' -> (do reserved "->"
                 result <- function
                 return (FunctionContract pos args' Nothing result)) <|>
             (do reserved "..."
                 reserved "->" <?> "-> after ..."
                 result <- function
                 let (fixedArgs,[restArg]) =  L.splitAt (length args' - 1) args'
                 return (FunctionContract pos fixedArgs (Just restArg) result))

namedContract :: CharParser st Contract
namedContract = do
  idFirst <- letter <|> oneOf "$_"
  pos <- getPosition
  -- same as JavaScript (from WebBits' lexer)
  idRest <- many1 (alphaNum <|> oneOf "$_")
  let name = idFirst:idRest
  let constr = do
        args <- parens $ many1 contract
        return (ConstructorContract pos name args)
  let named = do
        whiteSpace
        return (NamedContract pos name)
  constr <|> named
  

nonFunction = parens function <|> object <|> namedContract <|> array <|> flat 

array :: CharParser st Contract
array = do
  pos <- getPosition
  reservedOp "["
  elts <- contract `sepBy1` comma <?> "elements in an array contract"
  reservedOp "]"
  return (FixedArrayContract pos elts)

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
  reservedOp ":"
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
  let parser = do
        whiteSpace
        r <- interface
        eof
        return r
  case parse parser filename chars of
    Left err      -> fail (show err)
    Right exports -> return exports
