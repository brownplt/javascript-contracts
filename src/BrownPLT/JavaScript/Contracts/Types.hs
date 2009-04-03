module BrownPLT.JavaScript.Contracts.Types where

import qualified Data.List as L
import Text.ParserCombinators.Parsec.Pos (SourcePos)
import BrownPLT.JavaScript.Parser (ParsedExpression, ParsedStatement)

data Contract
  = FlatContract SourcePos ParsedExpression
  | NamedContract SourcePos String
  | FunctionContract SourcePos [Contract] (Maybe Contract) Contract
  | ConstructorContract SourcePos String [Contract]
  | FixedArrayContract SourcePos [Contract]
  | ArrayContract SourcePos Contract
  | ObjectContract SourcePos [(String,Contract)]
  deriving (Show)


contractPos :: Contract -> SourcePos
contractPos ctc = case ctc of
  FlatContract p _ -> p
  NamedContract p _ -> p
  FunctionContract p _ _ _ -> p
  ConstructorContract p _ _ -> p
  FixedArrayContract p _ -> p
  ArrayContract p _ -> p
  ObjectContract p _ -> p

data InterfaceItem 
  = InterfaceExport String SourcePos Contract
  | InterfaceAlias String Contract
  | InterfaceStatement { interfaceStatement :: ParsedStatement }
  | InterfaceInstance String SourcePos Contract -- ^always an object contract
  deriving (Show)

isInterfaceStatement (InterfaceStatement _) = True
isInterfaceStatement _ = False

isInterfaceExport (InterfaceExport{}) = True
isInterfaceExport _ = False

isInterfaceAlias (InterfaceAlias{}) = True
isInterfaceAlias _ = False

isInterfaceInstance (InterfaceInstance{}) = True
isInterfaceInstance _ = False
