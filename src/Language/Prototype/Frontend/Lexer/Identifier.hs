module Language.Prototype.Frontend.Lexer.Identifier where

import Language.Prototype.Frontend.Lexer.Types
  ( Token'
  )

newtype Identifier = Identifier String
  deriving (Show, Eq)

data Keyword
  = Let
  | Mut
  | Fn
  | Symbol
  deriving (Read, Show)

instance Token' Identifier
