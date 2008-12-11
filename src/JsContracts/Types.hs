module JsContracts.Types where

import Text.ParserCombinators.Parsec.Pos (SourcePos)
import WebBits.JavaScript.Parser (ParsedExpression, ParsedStatement)

data Contract
  = FlatContract SourcePos ParsedExpression
  | FunctionContract SourcePos [Contract] Contract
  | ObjectContract SourcePos [(String,Contract)]
  | NoContract SourcePos
  deriving (Show)

data Export = Export String Contract deriving (Show)

data InterfaceItem = InterfaceExport Export
                   | InterfaceStatement ParsedStatement
                   deriving (Show)
