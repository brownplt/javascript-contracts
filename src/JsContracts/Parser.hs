module JsContracts.Parser
  ( interface
  , parseInterface
  ) where

import Control.Monad

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
              | [ contract , ... ] -- arbitrary length array

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
  elt1 <- contract
  comma
  let arbitrary = do
        reservedOp "..."
        reservedOp "]"
        return (ArrayContract pos elt1)
  let fixed = do
        elts <- contract `sepBy` comma <?> "elements in an array contract"
        reservedOp "]"
        return (FixedArrayContract pos (elt1:elts))
  arbitrary <|> fixed

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
       
interfaceExport = do
    reservedOp "::" >> (return $ \id -> liftM (InterfaceExport id) contract)

interfaceAlias  = do
    reservedOp "=" >> (return $ \id -> liftM (InterfaceAlias id) contract)

interfaceElement = interfaceExport <|> interfaceAlias

interface :: CharParser st [InterfaceItem]
interface = many $ (stmt $ interfaceElement `fap` identifier) <|> (liftM InterfaceStatement parseBlockStmt)
    where stmt p  = do { e <- p; reservedOp ";"; return e }
          fap k m = do { e <- m; f <- k; f e }

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
