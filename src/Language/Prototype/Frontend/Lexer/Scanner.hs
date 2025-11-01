module Language.Prototype.Frontend.Lexer.Scanner
  ( plain
  , Char'Unit
  , char'unit
  , normal'repr
  , expand'repr
  ) where

import Language.Prototype.Frontend.Lexer.Types
  ( Lexer'Unit
  , Scanner
  )
import Text.Megaparsec qualified as M

plain
  :: forall a. Lexer'Unit a ~ Char => Scanner a
plain = M.anySingle

-- Char'Unit represents a character and a boolean indicating whether it was escaped.
type Char'Unit = (Bool, Char)
char'unit
  :: forall a. Lexer'Unit a ~ Char'Unit => Scanner a
char'unit = do
  mc <- M.anySingle
  case mc of
    '\\' -> do
      c <- M.anySingle
      return (True, c)
    _ -> return (False, mc)

normal'repr :: [Char'Unit] -> String
normal'repr = map snd

expand'repr :: [Char'Unit] -> String
expand'repr = concatMap $ \(x, y) -> if x then ['\\', y] else [y]
