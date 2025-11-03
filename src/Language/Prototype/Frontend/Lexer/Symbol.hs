module Language.Prototype.Frontend.Lexer.Symbol where

import Language.Prototype.Frontend.Lexer.Types
  ( Lexer'Unit
  , Token'
  )

data Paren'Type
  = Round
  | Square
  | Curly
  deriving (Show, Eq)

data Symbol
  = Bang -- !
  | Hash -- #
  | Bar
  | Colon -- :
  | Comma -- ,
  | Dot -- .
  | SemiColon -- ;
  | Slash -- /
  | BackSlash -- \
  | Paren'Symbol Paren'
  | Symbol String
  deriving (Show, Eq)

data Paren' = Paren' Bool Paren'Type
  deriving (Show, Eq)

instance Token' Symbol
pattern Paren :: Char -> Paren'
pattern Paren c <- (match'paren -> c)
  where
    Paren = \case
      '(' -> Paren' True Round
      ')' -> Paren' False Round
      '[' -> Paren' True Square
      ']' -> Paren' False Square
      '{' -> Paren' True Curly
      '}' -> Paren' False Curly
      _ -> error "invalid parenthesis character"
match'paren :: Paren' -> Char
match'paren (Paren' is'open ptype) = case (is'open, ptype) of
  (True, Round) -> '('
  (False, Round) -> ')'
  (True, Square) -> '['
  (False, Square) -> ']'
  (True, Curly) -> '{'
  (False, Curly) -> '}'

type instance Lexer'Unit Symbol = Char
