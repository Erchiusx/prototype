module Language.Prototype.Frontend.Lexer.Scanner
  ( plain
  , Char'Unit
  , char'unit
  ) where

import Language.Prototype.Frontend.Lexer.Types
  ( Scanner
  )
import Text.Megaparsec qualified as M

plain :: Scanner Char
plain =
  M.optional M.anySingle

-- Char'Unit represents a character and a boolean indicating whether it was escaped.
type Char'Unit = (Bool, Char)
char'unit :: Scanner Char'Unit
char'unit = do
  mc <- M.optional M.anySingle
  case mc of
    Nothing -> return Nothing
    Just c ->
      if c == '\\'
        then do
          mc2 <- M.optional M.anySingle
          case mc2 of
            Nothing -> return $ Just (False, c)
            Just c2 -> return $ Just (True, c2)
        else return $ Just (False, c)
