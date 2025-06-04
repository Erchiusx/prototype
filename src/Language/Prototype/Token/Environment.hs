module Language.Prototype.Token.Environment (Environment (..))
where

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
  read'token [] = return Nothing
  read'token (s : ss) = return $
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
      _ -> Nothing
