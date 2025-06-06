module Language.Prototype.Token
  ( Source (..)
  , module Language.Prototype.Token.Types
  , Parser
  , Token (..)
  , module Language.Prototype.Token.Environment
  , module Language.Prototype.Token.Keywords
  , module Language.Prototype.Token.Literal
  , module Language.Prototype.Token.Operator
  )
where

import Control.Applicative ((<|>))
import Control.Monad.State
  ( MonadState (get)
  , evalStateT
  )
import Data.Maybe (fromJust)
import GHC.Stack (HasCallStack)
import Language.Prototype.Token.Environment
import Language.Prototype.Token.Keywords
import Language.Prototype.Token.Literal
import Language.Prototype.Token.Operator
import Language.Prototype.Token.Types
import Text.Parsec (Parsec)


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
        go s' $ Right $ t : ts
    )
      <|> return (Right ts)

  step
    :: HasCallStack => String -> State' (String, Token)
  step [] = lift Nothing
  step s = do
    envs <- get
    case envs of
      [] -> simple'step s
      (Lexer'State e) : _ -> case e.name of
        "string interpolation" -> simple'step s
        'B':'e':'g':'i':'n':'\'':_ -> do undefined
        _ -> undefined

  simple'step :: String -> State' (String, Token)
  simple'step input =
    basic'lexer @Keyword input
      <|> basic'lexer @Operator input
      <|> ( do
              (str, _, identifier) <- close @Identifier input
              case identifier of
                Nothing -> lift Nothing
                Just id' -> return (str, make'token id')
          )
      <|> basic'lexer @Digit'Postfix input
      <|> basic'lexer @Template'String input

  basic'lexer
    :: forall n s r
     . (Lexer'Environment n s r, Tokenable s)
    => String -> State' (String, Token)
  basic'lexer input =
    close @n input
      >>= return . (make'token <$>) . drop'middle
