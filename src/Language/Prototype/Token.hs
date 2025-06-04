module Language.Prototype.Token
  ( Source (..)
  , module Language.Prototype.Token.Types
  , parse
  , Parser
  , Token (..)
  , module Language.Prototype.Token.Environment
  , module Language.Prototype.Token.Identifier
  , module Language.Prototype.Token.Keywords
  , module Language.Prototype.Token.Literal
  , module Language.Prototype.Token.Operator
  )
where

import Control.Monad.State (State, evalState)
import Language.Prototype.Token.Environment
import Language.Prototype.Token.Identifier
import Language.Prototype.Token.Keywords
import Language.Prototype.Token.Literal
import Language.Prototype.Token.Operator
import Language.Prototype.Token.Types hiding
  ( Token
  )
import Text.Parsec
  ( ParseError
  , Parsec
  , SourceName
  , Stream (..)
  , anyToken
  , many
  , runParser
  , runParserT
  )

newtype Source = Source String
data Token
  = Token'Literal Literal
  | Token'Environment Environment
  | Token'Keyword Keyword
  | Token'Operator Operator
  | Token'Identifier Identifier
  deriving Show

class Token' a => Tokenable a where
  make'token :: a -> Token

$( genInstances
    [ "Literal"
    , "Environment"
    , "Keyword"
    , "Operator"
    , "Identifier"
    ]
 )

instance Stream Source (State String'State) Token where
  uncons (Source []) = return Nothing
  uncons (Source s@(h : s'))
    | elem @[] h " \t" = uncons $ Source s'
    | otherwise =
        try'parsers
          [ treat <$> read'token @Literal s
          , treat <$> read'token @Environment s
          , treat <$> read'token @Operator s
          , treat <$> read'token @Keyword s
          , treat <$> read'token @Identifier s
          ]
   where
    try'parsers
      :: [State String'State (Maybe a)]
      -> State String'State (Maybe a)
    try'parsers [] = return Nothing
    try'parsers (p : ps) = do
      res <- p
      case res of
        Nothing -> try'parsers ps
        Just _ -> p

to'token'stream
  :: (Stream s m a, Show a)
  => s -> m (Either ParseError [a])
to'token'stream s = runParserT (many anyToken) () "" s

treat
  :: forall a
   . Tokenable a
  => Maybe (String, a)
  -> Maybe (Token, Source)
treat Nothing = Nothing
treat (Just (s, a)) = return (make'token a, Source s)

type Parser = Parsec [Token] ()

parse
  :: String
  -> Parser a
  -> SourceName
  -> Either ParseError a
parse s p n = do
  let res =
        evalState @String'State
          (to'token'stream $ Source s)
          0
  case res of
    Left e -> Left e
    Right tokens -> runParser p () n tokens
