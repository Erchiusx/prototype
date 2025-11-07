module Language.Prototype.Frontend.Lexer.Scanner
  ( plain
  , Char'Unit
  , char'unit
  , Char'Units (..)
  ) where

import Data.Aeson
  ( ToJSON (toJSON)
  , Value (String)
  )
import Data.Text (pack)
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
newtype Char'Units = Char'Units [Char'Unit]
  deriving (Show, Eq)
char'unit
  :: forall a. Lexer'Unit a ~ Char'Unit => Scanner a
char'unit = do
  mc <- M.anySingle
  case mc of
    '\\' -> do
      c <-
        M.anySingle
          M.<?> "expected character after escape backslash"
      return (True, c)
    _ -> return (False, mc)

instance ToJSON Char'Units where
  toJSON (Char'Units l) = String $ pack $ do
    (e, c) <- l
    if e
      then
        ['\\', c]
      else
        return c
