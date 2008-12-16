module JsContracts.Types where

import qualified Data.List as L
import Text.ParserCombinators.Parsec.Pos (SourcePos)
import WebBits.JavaScript.Parser (ParsedExpression, ParsedStatement)

data Contract
  = FlatContract SourcePos ParsedExpression
  | NamedContract SourcePos String
  | FunctionContract SourcePos [Contract] (Maybe Contract) Contract
  | ConstructorContract SourcePos String [Contract]
  | FixedArrayContract SourcePos [Contract]
  | ObjectContract SourcePos [(String,Contract)]
  | NoContract SourcePos

instance Show Contract where
  show (FlatContract _ _) = "<flat>"
  show (NamedContract _ s) = ':':s
  show (FunctionContract _ args _ result) = 
    L.concat (L.intersperse " " $ map show args) ++ " -> " ++ show result
  show (ConstructorContract _ name args) = name ++ "(" ++
    concat (L.intersperse "," (map show args)) ++ ")"
  show (ObjectContract _ _) = "<object-contract>"
  show (NoContract _) = "<no-contract>"

data InterfaceItem 
  = InterfaceExport String Contract
  | InterfaceAlias String Contract
  | InterfaceStatement { interfaceStatement :: ParsedStatement }
  deriving (Show)

isInterfaceStatement (InterfaceStatement _) = True
isInterfaceStatement _ = False

isInterfaceExport (InterfaceExport{}) = True
isInterfaceExport _ = False

isInterfaceAlias (InterfaceAlias{}) = True
isInterfaceAlias _ = False
