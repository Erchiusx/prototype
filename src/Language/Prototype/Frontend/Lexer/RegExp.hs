module Language.Prototype.Frontend.Lexer.RegExp where

import Language.Prototype.Frontend.Lexer.Types
  ( Lexer'Environment'
  , Token'
  )
import Language.Prototype.Frontend.Lexer.Scanner (Char'Unit)

newtype RegExp = RegExp String
  deriving (Show, Eq)

instance Token' RegExp
instance Lexer'Environment' RegExp Char'Unit
