module Language.Prototype.Frontend.Lexer (lex, test'lex, try'lex, Expression (..)) where

import Control.Monad (guard)
import Control.Monad.State qualified as ST
import Data.Aeson (ToJSON (..), Value (String))
import Data.Data (Typeable, typeOf)
import Data.Text (Text)
import GHC.Stack (HasCallStack)
import Language.Prototype.Frontend.Lexer.Identifier
import Language.Prototype.Frontend.Lexer.Number
import Language.Prototype.Frontend.Lexer.RegExp
import Language.Prototype.Frontend.Lexer.Scanner
import Language.Prototype.Frontend.Lexer.String
import Language.Prototype.Frontend.Lexer.Symbol
import Language.Prototype.Frontend.Lexer.Types
import Text.Megaparsec qualified as M
import Prelude hiding (lex)

data Empty'Token = Empty'Token deriving Show
instance ToJSON Empty'Token where
  toJSON Empty'Token = String ""
instance Token' Empty'Token () where
  content _ = Just ()
data Expression (a :: Maybe Paren'Type) = Expression
  deriving Show
type instance Lexer'Unit (Expression a) = Char
instance Typeable a => Lexer'Environment' (Expression a) where
  scanner :: Expression a -> Scanner (Expression a)
  scanner _ = plain @(Expression a)
  yield e = do
    begin <- M.getSourcePos
    c <- M.lookAhead M.anySingle
    case c of
      _ | c `elem` ['0' .. '9'] -> do
        enter Env'Number'Component
        ranged $ return Empty'Token
      _ | c `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] -> do
        enter Env'Identifier
        ranged $ return Empty'Token
      '"' -> do
        enter
          (Env'String (Expression @Nothing) Enter'String)
        ranged $ return Empty'Token
      '/' -> do
        enter Env'RegExp
        ranged $ return Empty'Token
      '\n' -> do
        _ <- M.many $ M.satisfy (== '\n')
        ranged . return $ LineBreak
      '}' -> (M.<?> "end of Curly Braces") $ do
        end <- M.getSourcePos
        if typeOf e == typeOf (Expression @(Just Curly))
          then do
            rewind
            return $
              Token (begin, end) $
                Paren'Symbol $
                  Paren' Close Curly
          else do
            _ : (Lexer'State e') : _ <- ST.get
            -- trace (show e') $
            if typeOf e'
              == typeOf
                (Env'String (Expression @Nothing) Enter'String)
              then do
                rewind
                rewind
                enter
                  ( Env'String
                      (Expression @Nothing)
                      Close'Interpolation
                  )
                return $ Token (begin, end) $ Interpolation Close
              else
                fail ""
      _ -> ranged $ do
        let symbol'chars :: String = "!#%^&*()-_=+[]{};:\\|,.<>/?"
        raw'symbol <-
          M.many $ M.satisfy (`elem` symbol'chars)
        guard $ raw'symbol /= []
        case raw'symbol of
          "!" -> return Bang
          "#" -> return Hash
          "|" -> return Bar
          ":" -> return Colon
          ";" -> return SemiColon
          "." -> return Dot
          "," -> return Comma
          "/" -> return Slash
          "\\" -> return BackSlash
          "(" -> do
            enter $ Expression @(Just Round)
            return $
              Paren'Symbol $
                Paren' Open Round
          ")" -> do
            -- i <- getInput
            -- trace
            --   ( show
            --       (i, typeOf e, typeOf (Expression @(Just Round)))
            --   ) $
            -- return ()
            (M.<?> "end of Round Paren") $
              guard $
                typeOf e == typeOf (Expression @(Just Round))
            rewind
            return $
              Paren'Symbol $
                Paren' Close Round
          "[" -> do
            enter $ Expression @(Just Square)
            return $
              Paren'Symbol $
                Paren' Open Square
          "]" -> do
            (M.<?> "end of Square Paren") $
              guard $
                typeOf e == typeOf (Expression @(Just Square))
            rewind
            return $
              Paren'Symbol $
                Paren' Close Square
          "{" -> do
            enter $ Expression @(Just Curly)
            return $
              Paren'Symbol $
                Paren' Open Curly
          _ ->
            return $
              Raw'Symbol raw'symbol
  fulfill e = (concat <$>) $ M.many $ do
    (Lexer'State e') : _ <- ST.get
    filter (not . is'token)
      <$> if typeOf e == typeOf e'
        then
          (: []) <$> yield' e
        else
          fulfill e'
   where
    is'token Token{..} = typeOf tokenVal == typeOf Empty'Token

lexer :: Lexer (Text, [Token])
lexer = do
  (Lexer'State e) : _ <- ST.get
  tokens <- fulfill e
  rest <- M.getInput
  return (rest, tokens)

lex
  :: String
  -> Text
  -> Either
       (M.ParseErrorBundle Text Lex'Error)
       (Text, [Token])
lex name file =
  ST.evalState
    (M.runParserT (skip'space >> lexer) name file)
    [Lexer'State $ Expression @Nothing]

test'lex
  :: HasCallStack
  => String
  -> Text
  -> [Lexer'Environment]
  -> Either
       (M.ParseErrorBundle Text Lex'Error)
       (Text, [Token])
test'lex name file =
  ST.evalState
    (M.runParserT lexer name file)

try'lex
  :: HasCallStack
  => String
  -> Text
  -> Lexer a
  -> Either
       (M.ParseErrorBundle Text Lex'Error)
       a
try'lex name file p =
  ST.evalState
    (M.runParserT p name file)
    []
