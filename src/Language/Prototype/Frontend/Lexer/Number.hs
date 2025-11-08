module Language.Prototype.Frontend.Lexer.Number where

import Data.Aeson
import Data.Text (pack)
import Language.Prototype.Frontend.Lexer.Identifier
  ( Env'Identifier (..)
  )
import Language.Prototype.Frontend.Lexer.Scanner
  ( plain
  )
import Language.Prototype.Frontend.Lexer.Types
  ( Lexer'Environment'
      ( fulfill
      , scanner
      , yield
      , yield'
      )
  , Lexer'Unit
  , Token (Token)
  , Token' (..)
  , ranged
  , rewind
  , skip'space
  )
import Text.Megaparsec
  ( anySingle
  , lookAhead
  , many
  , satisfy
  )

newtype Number'Component = Number'Component String
newtype Number'Postfix = Number'Postfix String

instance ToJSON Number'Component where
  toJSON (Number'Component str) =
    object
      [ "content" .= pack str
      , "type" .= pack "number-content"
      ]

instance ToJSON Number'Postfix where
  toJSON (Number'Postfix str) =
    object
      [ "content" .= pack str
      , "type" .= pack "number-postfix"
      ]

data Env'Number
  = Env'Number'Component
  | Env'Number'Postfix
  deriving Show
instance Token' Number'Component String where
  content (Number'Component n) = Just n
instance Token' Number'Postfix String where
  content (Number'Postfix n) = Just n

type instance Lexer'Unit Env'Number = Char
instance Lexer'Environment' Env'Number where
  scanner _ = plain @Env'Number
  yield Env'Number'Component =
    ranged $
      fmap Number'Component $
        many $
          satisfy
            (`elem` ['0' .. '9'])
  yield Env'Number'Postfix = ranged $ do
    (Token _ postfix) <- yield Env'Identifier
    (Just p) <- return $ do
      repr <- show <$> content postfix
      return $ Number'Postfix repr
    rewind
    return p
  fulfill Env'Number'Postfix = (: []) <$> yield' Env'Number'Postfix
  fulfill Env'Number'Component = do
    component <- yield Env'Number'Component
    next <- lookAhead anySingle
    if next `elem` '\'' : ['a' .. 'z'] ++ ['A' .. 'Z']
      then do
        postfix <- yield' Env'Number'Postfix
        return [component, postfix]
      else do
        skip'space
        return [component]
