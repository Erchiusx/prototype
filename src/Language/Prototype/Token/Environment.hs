module Language.Prototype.Token.Environment
  ( Environment (..)
  )
where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Monad.State
  ( MonadState (..)
  , modify
  )
import Language.Prototype.Token.Types

data Environment
  = Begin'Paren
  | End'Paren
  | Begin'Bracket
  | End'Bracket
  | Begin'Brace
  | End'Brace
  | Bar
  | Backtick
  | Comma
  | Semi
  | Line'Break
  | Dot
  deriving Show

instance Token' Environment where
  read'token [] = lift Nothing
  read'token (s : ss) =
    case s of
      '|' -> return (ss, Bar)
      '(' -> return (ss, Begin'Paren)
      ')' -> return (ss, End'Paren)
      '[' -> return (ss, Begin'Bracket)
      ']' -> return (ss, End'Bracket)
      '{' -> return (ss, Begin'Brace)
      '}' -> return (ss, End'Brace)
      '`' -> return (ss, Backtick)
      ',' -> return (ss, Comma)
      ';' -> return (ss, Semi)
      '\n' -> return (ss, Line'Break)
      '.' -> return (ss, Dot)
      _ -> lift Nothing

to'env :: Char -> Environment
to'env s =
  case s of
    '|' -> Bar
    '(' -> Begin'Paren
    ')' -> End'Paren
    '[' -> Begin'Bracket
    ']' -> End'Bracket
    '{' -> Begin'Brace
    '}' -> End'Brace
    '`' -> Backtick
    ',' -> Comma
    ';' -> Semi
    '\n' -> Line'Break
    '.' -> Dot
    _ -> undefined

instance HasField "name" Environment String where
  getField = show

instance
  Lexer'Environment
    Environment
    (Either Lexer'Error Environment)
    Char
  where
  begin [] = lift Nothing
  begin (c : cs) = do
    guard $ elem @[] c "|()[]{}`,.;\n"
    case c of
      '(' -> modify (Lexer'State Begin'Paren :)
      '[' -> modify (Lexer'State Begin'Bracket :)
      '{' -> modify (Lexer'State Begin'Brace :)
      ')' -> drop'env Begin'Paren
      ']' -> drop'env Begin'Bracket
      '}' -> drop'env Begin'Brace
      _ -> return ()
    return (cs, c)
   where
    drop'env target = do
      (Lexer'State e) : _ <- get
      guard $ e.name == target.name
      modify $ drop 1
  scan = plain'scanner
  ender c = elem @[] c "|([{}]),.;\n"
  close [] = lift Nothing
  close (c : cs) =
    ( begin @Environment (c : cs)
        >>= \(s, c') -> return (s, Just c', return $ to'env c)
    )
      <|> do
        (Lexer'State e) : _ <- get
        return
          ( c : cs
          , Nothing
          , Left $
              Unmatched'Environment e.name $
                show $
                  to'env c
          )
