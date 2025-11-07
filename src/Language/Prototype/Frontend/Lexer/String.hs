module Language.Prototype.Frontend.Lexer.String
  ( String'Component (..)
  , String'Component'Type (..)
  , Env'String (..)
  )
where

import Data.Aeson
import Data.Text (pack)
import Language.Prototype.Frontend.Lexer.Scanner
  ( Char'Unit
  , Char'Units (..)
  , char'unit
  )
import Language.Prototype.Frontend.Lexer.Types
  ( Lexer
  , Lexer'Environment' (..)
  , Lexer'Unit
  , Token' (..)
  , enter
  , ranged
  , pattern Open
  )
import Text.Megaparsec
  ( ErrorItem (EndOfInput)
  , MonadParsec (lookAhead)
  , satisfy
  , unexpected
  )

data String'Component
  = Interpolation Bool
  | Raw Char'Units

data Env'String env'expression
  = Env'String env'expression String'Component'Type

data String'Component'Type
  = Enter'String
  | Close'Interpolation

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
  scanner _ = char'unit @(Env'String env'expression)
  yield e@(Env'String env'expression component'type) = ranged $ do
    _ <-
      satisfy
        ( ==
            case component'type of
              Close'Interpolation -> '}'
              Enter'String -> '"'
        )
    scanned <- read'string'literal
    case scanned of
      ([], (True, '{')) -> do
        enter env'expression
        return $ Interpolation Open
      (str, (False, '"')) -> return $ Raw $ Char'Units str
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
        (False, '"') -> return (reverse cs, c)
        _ -> do
          _ <- scanner e
          go $ c : cs
