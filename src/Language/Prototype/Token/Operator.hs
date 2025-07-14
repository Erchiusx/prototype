module Language.Prototype.Token.Operator (Operator (..))
where

import Control.Monad (guard)
import Data.String (IsString (..))
import Language.Prototype.Token.Types

newtype Operator
  = Operator String
  deriving Show

instance IsString Operator where
  fromString = Operator

operator'chars :: String
operator'chars = ":<>/?!+-_*&=|."

instance Token' Operator where
  scan'token [] = nothingT
  scan'token s = do
    let (r, w) = (not . (`elem` operator'chars)) `break` s
    guard $ r /= ""
    return (fromString r, w)

instance Lexer'Environment' Operator Char where
  scan'unit = scan'unit @Plain

  is'ender = not . (`elem` operator'chars)
