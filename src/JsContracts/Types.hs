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
  | ArrayContract SourcePos Contract
  | ObjectContract SourcePos [(String,Contract)]
  deriving (Show)

data InterfaceItem 
  = InterfaceExport String Contract
  | InterfaceAlias String Contract
  | InterfaceStatement { interfaceStatement :: ParsedStatement }
  | InterfaceInstance String Contract -- ^always an object contract
  deriving (Show)

isInterfaceStatement (InterfaceStatement _) = True
isInterfaceStatement _ = False

isInterfaceExport (InterfaceExport{}) = True
isInterfaceExport _ = False

isInterfaceAlias (InterfaceAlias{}) = True
isInterfaceAlias _ = False

isInterfaceInstance (InterfaceInstance{}) = True
isInterfaceInstance _ = False
