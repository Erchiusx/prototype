module Language.Prototype.Frontend.Lexer.Types
  ( Token (..)
  , Token' (..)
  , Lex'Error (..)
  , Lex'Error'
  , Lexer'Unit
  , Lexer'Environment (..)
  , Lexer'Environment' (..)
  , Scanner
  , Lexer
  , drop'space
  , rewind
  , enchanted
  , enter
  , parade
  , ranged
  , fulfill'current'env
  , is'Instance
  , skip'space
  , pattern Close
  , pattern Open
  , M.getInput
  )
where

import Control.Monad.State
import Control.Monad.State.Class qualified as ST
import Data.Aeson
  ( ToJSON (..)
  , Value (String)
  , object
  , (.=)
  )
import Data.Data
  ( Proxy (Proxy)
  , TypeRep
  , Typeable
  , typeOf
  , typeRep
  )
import Data.String.Interpolate (i)
import Data.Text (Text, pack)
import Text.Megaparsec qualified as M
import Text.Megaparsec.Char (space1)
import Text.Megaparsec.Char.Lexer qualified as L

class (Ord a, Eq a, Show a) => Lex'Error' a

data Lex'Error = forall a. (Lex'Error' a, ToJSON a) => Lex'Error
  { lex'error'msg :: a
  , lex'error'level :: Int
  , lex'error'type :: TypeRep
  }

instance Show Lex'Error where
  show Lex'Error{..} =
    [i|
    Lexer Error detected:
      level #{lex'error'level}
      type #{lex'error'type}
      message
        #{lex'error'msg}
  |]

instance ToJSON Lex'Error where
  toJSON Lex'Error{..} =
    object
      [ "error_msg" .= lex'error'msg
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
        compare (lex'error'type e1) (lex'error'type e2)
      other -> other

class
  ( Show c
  , ToJSON a
  ) =>
  Token' a c
    | a -> c
  where
  content :: a -> Maybe c

data Token = forall a c. (Token' a c, Typeable a) => Token
  { tokenRange :: (M.SourcePos, M.SourcePos)
  , tokenVal :: a
  }

pos'json'repr
  :: (M.SourcePos, M.SourcePos) -> Value
pos'json'repr (begin, end) =
  String $
    pack
      [i|#{
        M.sourceName begin
      }:#{
        M.unPos $ M.sourceLine begin
      }:#{
        M.unPos $ M.sourceColumn begin
      }-#{
        M.unPos $ M.sourceLine end
      }:#{
        M.unPos $ M.sourceColumn end
      }|]

instance ToJSON Token where
  toJSON Token{..} =
    object
      [ "range" .= pos'json'repr tokenRange
      , "token" .= tokenVal
      ]

type family Lexer'Unit env

type Scanner env = Lexer (Lexer'Unit env)

class (Typeable env, Show env) => Lexer'Environment' env where
  fulfill :: env -> Lexer [Token] -- either opens a new environment or closes this environment
  fulfill = fmap (: []) . yield'
  scanner :: env -> Scanner env -- reads a unit
  yield :: env -> Lexer Token -- yields one token
  yield' :: env -> Lexer Token
  yield' e =
    drop'space $
      yield e
  transform :: env -> env
  transform = id

data Lexer'Environment
  = forall a.
    Lexer'Environment' a =>
    Lexer'State a

instance Show Lexer'Environment where
  show (Lexer'State a) = show a

type Lexer =
  M.ParsecT
    Lex'Error
    Text
    (State [Lexer'Environment])

-- some utilities
lex'space :: Lexer ()
lex'space =
  L.space
    space1
    (L.skipLineComment "-*")
    (L.skipBlockComment "{-" "-}")

drop'space :: Lexer a -> Lexer a
drop'space = L.lexeme lex'space

rewind :: Lexer ()
rewind = do
  _ : st <- ST.get
  ST.put st

enchanted :: Lexer Lexer'Environment
enchanted = do
  t : _ <- ST.get
  return t

enter :: Lexer'Environment' e => e -> Lexer ()
enter h = do
  st <- ST.get
  ST.put $ Lexer'State h : st

parade :: Lexer'Environment' e => e -> Lexer Token
parade e = do
  enter e
  yield e

ranged
  :: forall a e s m c
   . ( Token' a c
     , M.Stream s
     , M.TraversableStream s
     , Ord e
     , Typeable a
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

pattern Open :: Bool
pattern Open = False
pattern Close :: Bool
pattern Close = True

-- util
is'Instance
  :: forall a b. (Typeable a, Typeable b) => b -> Bool
is'Instance b = typeOf b == typeRep (Proxy @a)

skip'space :: Lexer ()
skip'space = drop'space $ return ()
