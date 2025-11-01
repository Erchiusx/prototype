module Language.Prototype.Frontend.Lexer.String where

import Language.Prototype.Frontend.Lexer.Scanner
  ( Char'Unit
  , char'unit
  )
import Language.Prototype.Frontend.Lexer.Types
  ( Lexer'Environment' (..)
  , Lexer'Unit
  , Token'
  , just
  , ranged, Lexer, enter, Lexer'Environment (Lexer'State)
  )
import Text.Megaparsec (MonadParsec(lookAhead))

pattern Open :: Bool
pattern Open = False
pattern Close :: Bool
pattern Close = True

data String'Component
  = Interpolation Bool
  | Raw [Char'Unit]

newtype Env'String env'expression = Env'String env'expression

type instance
  Lexer'Unit (Env'String env'expression) =
    Char'Unit
instance Token' String'Component
instance (Lexer'Environment' env'expression) => Lexer'Environment' (Env'String env'expression) where
  scanner _ = char'unit @(Env'String env'expression)
  fulfill = just . yield
  yield e@(Env'String env'expression) = ranged $ do
    scanned <- read'string'literal
    case scanned of
      ([], (True, '{')) -> do
        enter $ Lexer'State env'expression
        return $ Interpolation Open
      (str, (False, '"')) -> return $ Raw str
      _ -> fail "error condition: unknown terminator of string"
   where
    read'string'literal :: Lexer ([Char'Unit], Char'Unit)
    read'string'literal = go []

    go cs = do
      c <- lookAhead $ scanner e
      case c of 
        (True, '{') -> return (reverse cs, c)
        (False, '"') -> return (reverse cs, c)
        _ -> do 
          _ <- scanner e
          go $ c:cs
