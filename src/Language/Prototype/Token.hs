module Language.Prototype.Token
  ( Source (..)
  , Parser
  , Token (..)
  , module Language.Prototype.Token.Types
  , module Language.Prototype.Token.Environment
  , module Language.Prototype.Token.Keywords
  , module Language.Prototype.Token.Literal
  , module Language.Prototype.Token.Operator
  , parse
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Identity
  ( Identity (runIdentity)
  )
import Data.Maybe (fromJust)
import GHC.Stack (HasCallStack)
import Language.Prototype.Token.Environment
import Language.Prototype.Token.Keywords
import Language.Prototype.Token.Literal
import Language.Prototype.Token.Operator
import Language.Prototype.Token.Types
import Text.Parsec
  ( ParseError
  , Parsec
  , SourceName
  , runParserT
  )
import Control.Monad.State (evalStateT)

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

-- treat
--   :: forall a f
--    . ( Tokenable a
--      , Applicative f
--      )
--   => f a
--   -> f Token
-- treat = (make'token <$>)

type Parser = Parsec [Token] ()

-- to'token'stream :: String -> Maybe [Token]
-- to'token'stream s = evalStateT @Maybe lexer []
--  where
--   step [] = lift Nothing
--   step s' =
--     asum
--       [ treat <$> read'token @Literal s'
--       , treat <$> read'token @Environment s'
--       , treat <$> read'token @Operator s'
--       , treat <$> read'token @Keyword s'
--       , treat <$> read'token @Identifier s'
--       ]
--   lexer = s `go` []
--   go ss ts =
--     ( do
--         (s', t) <- step ss
--         go s' $ t : ts
--     )
--       <|> return ts

to'token'stream
  :: String -> Either Lexer'Error [Token]
to'token'stream s = fromJust $ evalStateT @Maybe lexer []
 where
  lexer :: State' (Either Lexer'Error [Token])
  lexer = do
    res <- s `go` Right []
    return $ do
      ts <- res
      return $ reverse ts

  go
    :: String
    -> (Either Lexer'Error [Token])
    -> State' (Either Lexer'Error [Token])
  go _ (Left e) = return $ Left e
  go ss (Right ts) =
    ( do
        (s', t) <- step ss
        case t of
          Left e ->
            go s' $ Left e
          Right t' ->
            go s' $ Right $ t' : ts
    )
      <|> return (Right ts)

  step
    :: HasCallStack
    => String
    -> State' (String, Either Lexer'Error Token)
  step [] = lift Nothing
  step s' = simple'step s'

  simple'step
    :: String
    -> State' (String, Either Lexer'Error Token)
  simple'step input =
    basic'lexer @Keyword input
      <|> basic'lexer @Operator input
      <|> ( do
              (str, _, identifier) <- close @Identifier input
              case identifier of
                Nothing -> lift Nothing
                Just id' -> return (str, return $ make'token id')
          )
      <|> basic'lexer @Digit'Postfix input
      <|> basic'lexer @Template'String input
      <|> ( do
              (str, _, env) <- close @Environment input
              return . (str,) $ make'token <$> env
          )

  basic'lexer
    :: forall n s r
     . (Lexer'Environment n s r, Tokenable s)
    => String
    -> State' (String, Either Lexer'Error Token)
  basic'lexer input =
    close @n input
      >>= return . ((return . make'token) <$>) . drop'middle

data Parser'Error
  = Lexer'Error Lexer'Error
  | Parse'Error ParseError

parse
  :: SourceName
  -> Parser a
  -> String
  -> Either Parser'Error a
parse name p source = do
  case to'token'stream source of
    Left e -> Left $ Lexer'Error e
    Right tokens -> do
      case runIdentity $ runParserT p () name tokens of
        Left e -> Left $ Parse'Error e
        Right a -> Right a
