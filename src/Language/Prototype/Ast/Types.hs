module Language.Prototype.Ast.Types
  ( Ast' (..)
  , Ast (..)
  , anyToken
  , Parser
  , unexpected
  , option
  , many
  , Char'Unit
  )
where

import Language.Prototype.Token (Parser, Char'Unit)
import Text.Parsec
  ( anyToken
  , many
  , option
  , unexpected
  )

class Ast' a where
  expect :: Parser a

data Ast
  = forall a. Ast' a => Ast a
