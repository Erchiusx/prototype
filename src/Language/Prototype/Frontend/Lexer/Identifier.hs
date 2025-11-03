module Language.Prototype.Frontend.Lexer.Identifier
  ( Identifier (..)
  , Keyword (..)
  , Env'Identifier (..)
  ) where

import Control.Applicative qualified as A
import Data.Char (isAlpha, isAlphaNum)
import Language.Prototype.Frontend.Lexer.Scanner
  ( plain
  )
import Language.Prototype.Frontend.Lexer.Types
  ( Lexer
  , Lexer'Environment' (..)
  , Lexer'Unit
  , Token'
  , ranged
  , rewind
  )
import Text.Megaparsec qualified as M
import Text.Read (readMaybe)

newtype Identifier = Identifier String
  deriving (Show, Eq)

data Keyword
  = Let
  | Mut
  | Fn
  | Symbol
  | Match
  | Yield
  | Resume
  | Async
  | Await
  | Do
  deriving (Read, Show)

instance Token' Identifier
instance Token' Keyword
data Env'Identifier = Env'Identifier
type instance Lexer'Unit Env'Identifier = Char
instance Lexer'Environment' Env'Identifier where
  scanner _ = plain @Env'Identifier
  yield _ =
    (<* rewind) -- exit this environment after finishing the identifier
      $ ranged
      $ constructed
      $ A.liftA2
        (:) -- prepare to use leader and component to build the identifier string
        (M.satisfy isAlpha) -- first character must be from leader set
      $ M.many
      $ M.satisfy (\c -> isAlphaNum c || c == '\'') -- subsequent characters can be from component set or apostrophe
   where
    constructed
      :: Lexer String -> Lexer (Either Keyword Identifier)
    constructed lexeme = do
      str <- lexeme
      case readMaybe @Keyword str of
        Just kw -> return $ Left kw
        Nothing -> return $ Right $ Identifier str

  fulfill _ = do
    token <- yield Env'Identifier
    return [token]
