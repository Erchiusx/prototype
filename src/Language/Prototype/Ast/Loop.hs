module Language.Prototype.Ast.Loop where

import Language.Prototype.Ast.Types
import Language.Prototype.Token qualified as T

data
  Loop
    statement
    expression
    block
    case'
    binding
  = For'Loop
      { init :: Maybe statement
      , binding :: [binding]
      , body :: block statement expression
      }
  | While'Loop
      { condition :: expression
      , body :: block statement expression
      }
  | Functional'Loop case'

instance
  ( Ast' statement
  , Ast' expression
  , Ast' case'
  , Ast' binding
  , Ast' (block statement expression)
  )
  => Ast'
      (Loop statement expression block case' binding)
  where
  expect = do
    T.Token'Keyword k <- anyToken
    case k of
      T.For -> do
        init <-
          option Nothing $ Just <$> expect @statement
        bindings <- many $ expect @binding
        body <- expect @(block statement expression)
        return $ For'Loop init bindings body
      T.While -> do
        condition <- expect @expression
        body <- expect @(block statement expression)
        return $ While'Loop condition body
      T.Loop -> do
        case' <- expect @case'
        return $ Functional'Loop case'
      _ -> unexpected $ show k
