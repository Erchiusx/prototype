module Language.Prototype.Frontend.Lexer.RegExp where

import Data.Aeson
import Data.Text (pack)
import Language.Prototype.Frontend.Lexer.Scanner
  ( Char'Unit
  , Char'Units (Char'Units)
  , char'unit
  )
import Language.Prototype.Frontend.Lexer.Types
  ( Lexer
  , Lexer'Environment' (scanner, yield)
  , Lexer'Unit
  , Token'
  , content
  , ranged
  , rewind
  )

newtype RegExp = RegExp Char'Units
  deriving (Show, Eq)

instance ToJSON RegExp where
  toJSON (RegExp regex) =
    object
      [ "content" .= regex
      , "type" .= pack "regex"
      ]

data Env'RegExp = Env'RegExp
type instance Lexer'Unit Env'RegExp = Char'Unit
instance Token' RegExp Char'Units where
  content (RegExp regex) = Just regex
instance Lexer'Environment' Env'RegExp where
  scanner _ = char'unit @Env'RegExp
  yield _ =
    ranged $
      RegExp . Char'Units <$> go []
   where
    go :: [Char'Unit] -> Lexer [Char'Unit]
    go r = do
      c <- scanner Env'RegExp
      case c of
        (False, '/') -> do
          rewind
          return $ reverse r
        _ -> go $ c : r
