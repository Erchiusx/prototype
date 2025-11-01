module Language.Prototype.Frontend.Lexer.RegExp where

import Language.Prototype.Frontend.Lexer.Scanner
  ( Char'Unit
  , char'unit
  , expand'repr
  )
import Language.Prototype.Frontend.Lexer.Types
  ( Lexer
  , Lexer'Environment' (fulfill, scanner, yield)
  , Lexer'Unit
  , Token'
  , ranged
  , rewind
  )

newtype RegExp = RegExp String
  deriving (Show, Eq)

data Env'RegExp = Env'RegExp
type instance Lexer'Unit Env'RegExp = Char'Unit
instance Token' RegExp
instance Lexer'Environment' Env'RegExp where
  scanner _ = char'unit @Env'RegExp
  yield _ =
     ranged $
      RegExp . expand'repr <$> go []
   where
    go :: [Char'Unit] -> Lexer [Char'Unit]
    go r = do
      c <- scanner Env'RegExp
      case c of
        (False, '/') -> do
          rewind
          return $ reverse r
        _ -> go $ c : r
  fulfill _ = do
    token <- yield Env'RegExp
    return [token]
