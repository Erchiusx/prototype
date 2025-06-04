module Language.Prototype.Ast.Block (Block (..)) where

import Language.Prototype.Ast.Types
import Language.Prototype.Token qualified as T

data
  Block
    statement
    expression
  = Block [statement] expression

instance
  (Ast' statement, Ast' expression)
  => Ast' (Block statement expression)
  where
  expect = do
    T.Token'Environment T.Begin'Brace <- anyToken
    statements <- many $ expect @statement
    T.Token'Environment T.Bar <- anyToken
    expression <- expect @expression
    return $ Block statements expression
