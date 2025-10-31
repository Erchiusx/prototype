module Language.Prototype.Frontend.Lexer.Types
  ( Token (..)
  , Token'
  , Lex'Error (..)
  , Lex'Error'
  , Lexer'Environment (..)
  , Lexer'Environment' (..)
  , Scanner
  , Lexer
  )
where

import Data.Data (TypeRep)
import Data.Text (Text)
import Text.Megaparsec qualified as M

class (Ord a, Eq a) => Lex'Error' a

data Lex'Error = forall a. Lex'Error' a => Lex'Error
  { lex'error'pos :: M.SourcePos
  , lex'error'msg :: a
  , lex'error'level :: Int
  , lex'error'type :: TypeRep
  }

instance Eq Lex'Error where
  Lex'Error
    { lex'error'type = t1
    , lex'error'level = l1
    }
    == Lex'Error
      { lex'error'type = t2
      , lex'error'level = l2
      } = t1 == t2 && l1 == l2

instance Ord Lex'Error where
  compare e1 e2 =
    case compare (lex'error'level e1) (lex'error'level e2) of
      EQ ->
        compare (lex'error'pos e1) (lex'error'pos e2)
      other -> other

class Token' a where
  try :: Lexer (Maybe Token)

data Token = forall a. Token' a => Token
  { tokenPos :: M.SourcePos
  , tokenVal :: a
  }

type Scanner unit = Lexer (Maybe unit)

class Lexer'Environment' a b | a -> b where
  close :: Lexer ()
  scanner :: Scanner b

data Lexer'Environment
  = forall a b. Lexer'Environment' a b => Lexer'State a

type Lexer =
  M.ParsecT
    Lex'Error
    Text
    (M.State [Lexer'Environment])
