module JsContracts.Types where

import qualified Data.List as L
import Text.ParserCombinators.Parsec.Pos (SourcePos)
import WebBits.JavaScript.Parser (ParsedExpression, ParsedStatement)

data Contract
  = FlatContract SourcePos ParsedExpression
  | FunctionContract SourcePos [Contract] Contract
  | ObjectContract SourcePos [(String,Contract)]
  | NoContract SourcePos

instance Show Contract where
  show (FlatContract _ _) = "<flat>"
  show (FunctionContract _ args result) = 
    L.concat (L.intersperse " " $ map show args) ++ " -> " ++ show result
  show (ObjectContract _ _) = "<object-contract>"
  show (NoContract _) = "<no-contract>"

data Export = Export String Contract deriving (Show)

data InterfaceItem 
  = InterfaceExport Export
  | InterfaceStatement { interfaceStatement :: ParsedStatement }
  deriving (Show)

isInterfaceStatement (InterfaceStatement _) = True
isInterfaceStatement _ = False

isInterfaceExport (InterfaceExport _) = True
isInterfaceExport _ = False
