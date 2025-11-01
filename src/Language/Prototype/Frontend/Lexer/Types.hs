module Language.Prototype.Frontend.Lexer.Types
  ( Token (..)
  , Token'
  , Lex'Error (..)
  , Lex'Error'
  , Lexer'Unit
  , Lexer'Environment (..)
  , Lexer'Environment' (..)
  , Scanner
  , Lexer
  , drop'space
  , rewind
  , enter
  , ranged
  , fulfill'current'env
  , just
  )
where

import Control.Monad.Identity (Identity)
import Control.Monad.State
import Control.Monad.State.Class qualified as ST
import Data.Aeson (ToJSON (..), object, (.=))
import Data.Data (TypeRep)
import Data.Text (Text)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

class (Ord a, Eq a, Show a) => Lex'Error' a

data Lex'Error = forall a. (Lex'Error' a, ToJSON a) => Lex'Error
  { lex'error'pos :: M.SourcePos
  , lex'error'msg :: a
  , lex'error'level :: Int
  , lex'error'type :: TypeRep
  }

instance ToJSON Lex'Error where
  toJSON Lex'Error{..} =
    object
      [ "error_pos" .= show lex'error'pos
      , "error_msg" .= lex'error'msg
      , "error_level" .= lex'error'level
      , "lex'error'type" .= show lex'error'type
      ]

instance Eq Lex'Error where
  (==)
    Lex'Error
      { lex'error'type = t1
      , lex'error'level = l1
      }
    Lex'Error
      { lex'error'type = t2
      , lex'error'level = l2
      } = t1 == t2 && l1 == l2

instance Ord Lex'Error where
  compare e1 e2 =
    case compare (lex'error'level e1) (lex'error'level e2) of
      EQ ->
        compare (lex'error'pos e1) (lex'error'pos e2)
      other -> other

class Token' a

data Token = forall a. Token' a => Token
  { tokenRange :: (M.SourcePos, M.SourcePos)
  , tokenVal :: a
  }

type family Lexer'Unit env

type Scanner env = Lexer (Lexer'Unit env)

class Lexer'Environment' env where
  fulfill :: env -> Lexer [Token] -- either opens a new environment or closes this environment
  scanner :: env -> Scanner env -- reads a unit
  yield :: env -> Lexer Token -- yields one token

data Lexer'Environment
  = forall a. Lexer'Environment' a => Lexer'State a

type Lexer =
  M.ParsecT
    Lex'Error
    Text
    (State [Lexer'Environment])

-- some utilities
lex'space :: Lexer ()
lex'space = L.space space1 mempty mempty

drop'space :: Lexer a -> Lexer a
drop'space = L.lexeme lex'space

rewind :: Lexer ()
rewind = do
  _ : st <- ST.get
  ST.put st

enter :: Lexer'Environment -> Lexer ()
enter h = do
  st <- ST.get
  ST.put $ h : st

ranged
  :: forall a e s m
   . ( Token' a
     , M.Stream s
     , M.TraversableStream s
     , Ord e
     )
  => M.ParsecT e s m a
  -> M.ParsecT e s m Token
ranged p = do
  start <- M.getSourcePos
  a <- p
  end <- M.getSourcePos
  return $
    Token
      { tokenVal = a
      , tokenRange = (start, end)
      }

fulfill'current'env :: Lexer [Token]
fulfill'current'env = do
  (Lexer'State s) : _ <- ST.get
  fulfill s

just
  :: forall e s m a
   . M.MonadParsec e s m => m a -> m [a]
just p = do
  a <- p
  return [a]