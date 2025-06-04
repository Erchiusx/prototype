module Language.Prototype.Token.Identifier (Identifier (..)) where

import Control.Monad (guard)
import Data.String (IsString (..))
import Language.Prototype.Token.Types

newtype Identifier
  = Identifier String
  deriving Show

instance IsString Identifier where
  fromString = Identifier

instance Token' Identifier where
  read'token [] = return Nothing
  read'token s@(h : _) = return $ do
    guard $ h `elem` ['a' .. 'z'] ++ ['A' .. 'Z']
    let (w, r) =
          ( not
              . ( `elem`
                    ['a' .. 'z']
                      ++ ['A' .. 'Z']
                      ++ ['0' .. '9']
                      ++ "'"
                )
          )
            `break` s
    return (r, fromString w)
