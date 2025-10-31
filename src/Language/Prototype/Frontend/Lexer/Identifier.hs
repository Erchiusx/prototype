module Language.Prototype.Frontend.Lexer.Identifier where

import Language.Prototype.Frontend.Lexer.Types
  ( Token'
  )
import Text.Megaparsec qualified as M

newtype Identifier = Identifier String
  deriving (Show, Eq)

instance Token' Identifier
