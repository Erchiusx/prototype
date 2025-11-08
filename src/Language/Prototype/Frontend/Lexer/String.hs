module Language.Prototype.Frontend.Lexer.String
  ( String'Component (..)
  , String'Component'Type (..)
  , Env'String (..)
  )
where

import Control.Monad (guard)
import Data.Aeson
  ( KeyValue ((.=))
  , ToJSON (toJSON)
  , object
  )
import Data.Text (pack)
import Data.Typeable (typeOf)
import Language.Prototype.Frontend.Lexer.Scanner
  ( Char'Unit
  , Char'Units (..)
  , char'unit
  )
import Language.Prototype.Frontend.Lexer.Types
  ( Lexer
  , Lexer'Environment (Lexer'State)
  , Lexer'Environment' (..)
  , Lexer'Unit
  , Token' (..)
  , enchanted
  , enter
  , ranged
  , rewind
  , pattern Open
  )
import Text.Megaparsec
  ( ErrorItem (EndOfInput)
  , MonadParsec (lookAhead)
  , unexpected
  )

data String'Component
  = Interpolation Bool
  | Raw Char'Units

data Env'String env'expression
  = Env'String env'expression String'Component'Type
  deriving Show

data String'Component'Type
  = Enter'String
  | Close'Interpolation
  deriving Show

instance ToJSON String'Component where
  toJSON (Interpolation o) =
    object
      [ "type" .= pack "interpolation-flag"
      , "content" .= o
      ]
  toJSON (Raw c) =
    object
      [ "type" .= pack "string-component"
      , "content" .= c
      ]

type instance
  Lexer'Unit (Env'String env'expression) =
    Char'Unit
instance Token' String'Component (Either Bool Char'Units) where
  content (Interpolation t) = Just $ Left t
  content (Raw repr) = Just $ Right repr

instance
  Lexer'Environment' env'expression
  => Lexer'Environment' (Env'String env'expression)
  where
  transform (Env'String e _) = Env'String e Close'Interpolation
  scanner _ = char'unit @(Env'String env'expression)
  yield e@(Env'String env'expression component'type) = ranged $ do
    leader <- scanner e
    guard $
      case component'type of
        Close'Interpolation -> leader == (False, '}')
        Enter'String -> leader `elem` [(False, '"'), (True, '{')]
    if leader == (True, '{')
      then do
        enter env'expression
        return $ Interpolation Open
      else do
        scanned <- read'string'literal
        case scanned of
          (str, (False, '"')) -> rewind >> return (Raw $ Char'Units str)
          (str, (True, '{')) ->
            return (Raw $ Char'Units str)
          _ ->
            unexpected EndOfInput
   where
    read'string'literal
      :: Lexer ([Char'Unit], Char'Unit)
    read'string'literal = go []

    go cs = do
      c <- lookAhead $ scanner e
      case c of
        (True, '{') -> return (reverse cs, c)
        (False, '"') -> scanner e >> return (reverse cs, c)
        _ -> do
          _ <- scanner e
          go $ c : cs
  fulfill e = do
    h <- yield' e
    (Lexer'State e') <- enchanted
    if typeOf e == typeOf e'
      then do
        l <- yield' e
        return [h, l]
      else return [h]
