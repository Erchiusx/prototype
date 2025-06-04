module Language.Prototype.Ast.Identifier where

import Language.Prototype.Ast.Types
  ( Ast' (..)
  , anyToken
  )
import Language.Prototype.Token qualified as T

data Identifier = Identifier T.Token

identifier :: T.Token -> Maybe Identifier
identifier t@(T.Token'Identifier _) = Just $ Identifier t
identifier _ = Nothing

instance Ast' Identifier where
  expect = do
    token <- anyToken
    case identifier token of
      Nothing -> fail ""
      Just t -> return t
