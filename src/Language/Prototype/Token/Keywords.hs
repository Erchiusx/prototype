module Language.Prototype.Token.Keywords (Keyword (..)) where

import Control.Monad (guard)
import Data.String (IsString (fromString))
import Language.Prototype.Token.Types

$( genKeywords
    [ "case"
    , "of"
    , "let"
    , "var"
    , "in"
    , "for"
    , "while"
    , "loop"
    , "else"
    , "break"
    , "continue"
    , "function"
    , "return"
    , "async"
    , "await"
    , "yield"
    , "throw"
    , "catch"
    , "finally"
    , "resume"
    , "generator"
    ]
 )

instance Token' Keyword where
  read'token [] = lift Nothing
  read'token s@(h : _) = do
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
    guard $ w `elem` keywords
    return $ (r, fromString w)

instance HasField "name" Keyword String where
  getField = show
instance Lexer'Environment Keyword Keyword Char where
  scan = plain'scanner
  begin = plain'scanner
  close s = do
    (s', k) <- read'token s
    return (s', Nothing, k)
  ender = const False
