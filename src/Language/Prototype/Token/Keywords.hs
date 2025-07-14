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

instance Lexer'Unit Keyword where
  size = length . show
  wrap'lines = const 0
instance Lexer'Environment' Keyword Keyword where
  scan'unit [] = nothingT
  scan'unit s@(h:_) = do
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
    return $ (fromString w, r)
  is'ender = const False

instance Token' Keyword where
  scan'token = scan'unit' @Keyword
