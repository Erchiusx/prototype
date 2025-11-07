module Language.Prototype.Frontend.Lexer.Symbol where

import Data.Aeson
import Data.Text (pack)
import Language.Prototype.Frontend.Lexer.Types
  ( Lexer'Unit
  , Token' (..)
  )

data Paren'Type
  = Round
  | Square
  | Curly
  deriving (Show, Eq)

data Symbols
  = Bang -- !
  | Hash -- #
  | Bar --  |
  | Colon -- :
  | Comma -- ,
  | Dot -- .
  | SemiColon -- ;
  | Slash -- /
  | BackSlash -- \
  | LineBreak -- \n
  | Paren'Symbol Paren'
  | Raw'Symbol String
  deriving (Show, Eq)

data Paren' = Paren' Bool Paren'Type
  deriving (Show, Eq)

instance ToJSON Symbols where
  toJSON s =
    object
      [ "type" .= pack "symbols"
      , "content" .= show s
      ]

instance Token' Symbols String where
  content = Just . show
type instance Lexer'Unit Symbols = Char
