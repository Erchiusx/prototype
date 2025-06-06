module Language.Prototype.Token.Operator (Operator (..))
where

import Data.String (IsString (..))
import Language.Prototype.Token.Types

newtype Operator
  = Operator String
  deriving Show

instance IsString Operator where
  fromString = Operator

instance Token' Operator where
  read'token [] = lift Nothing
  read'token s = do
    let operator'chars = ":<>/?!+-_*&=|." :: String
    let (r, w) = (not . (`elem` operator'chars)) `break` s
    return (r, fromString w)

instance HasField "name" Operator String where
  getField = show

instance Lexer'Environment Operator Operator Char where
  scan = plain'scanner
  begin = plain'scanner
  ender = const False
  close s = do
    (s', o) <- read'token s
    return (s', Nothing, o)
