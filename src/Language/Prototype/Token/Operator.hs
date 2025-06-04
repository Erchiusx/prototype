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
  read'token [] = return Nothing
  read'token s = do
    let operator'chars = ":<>/?!+-_*&=|." :: String
    let (r, w) = (not . (`elem` operator'chars)) `break` s
    return $ return (r, fromString w)
