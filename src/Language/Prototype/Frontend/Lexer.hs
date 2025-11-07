module Language.Prototype.Frontend.Lexer () where

import Language.Prototype.Frontend.Lexer.Identifier
import Language.Prototype.Frontend.Lexer.Number
import Language.Prototype.Frontend.Lexer.RegExp
import Language.Prototype.Frontend.Lexer.Scanner
import Language.Prototype.Frontend.Lexer.String
import Language.Prototype.Frontend.Lexer.Symbol
import Language.Prototype.Frontend.Lexer.Types
import Text.Megaparsec qualified as M

data Expression = Expression
type instance Lexer'Unit Expression = Char
instance Lexer'Environment' Expression where
  scanner _ = plain @Expression
  yield _ = do
    c <- M.lookAhead M.anySingle
    case c of
      _ | c `elem` ['0' .. '9'] -> do
        enter Env'Number'Component
        yield Env'Number'Component
      _ | c `elem` ['a' .. 'z'] ++ ['A' .. 'Z'] -> do
        enter Env'Identifier
        yield Env'Identifier
      _ | c == '"' -> do
        enter
          (Env'String Expression Enter'String)
        yield (Env'String Expression Enter'String)
      _ | c == '/' -> do
        enter Env'RegExp
        yield Env'RegExp
      _ | c == '\n' -> do
        _ <- M.many $ M.satisfy (== '\n')
        ranged . return $ LineBreak
      _ -> do
        let symbol'chars :: String = "!#%^&*()-_=+[]{};:\\|,.<>/?"
        raw'symbol <-
          M.many $ M.satisfy (`elem` symbol'chars)
        ranged . return $ case raw'symbol of
          "!" -> Bang
          "#" -> Hash
          "|" -> Bar
          ":" -> Colon
          ";" -> SemiColon
          "." -> Dot
          "," -> Comma
          "/" -> Slash
          "\\" -> BackSlash
          "(" -> Paren'Symbol $ Paren' Open Round
          ")" -> Paren'Symbol $ Paren' Close Round
          "[" -> Paren'Symbol $ Paren' Open Square
          "]" -> Paren'Symbol $ Paren' Close Square
          "{" -> Paren'Symbol $ Paren' Open Curly
          "}" -> Paren'Symbol $ Paren' Close Curly
          _ -> Raw'Symbol raw'symbol
  fulfill e = undefined

-- lex
--   :: String
--   -> Either
--        (ParseErrorBundle String Lex'Error)
--        [Token]
-- lex name =
--   evalState (runParserT lexer name "") []
