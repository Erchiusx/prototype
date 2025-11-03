module Language.Prototype.Frontend.Lexer where
import Language.Prototype.Frontend.Lexer.Types

import Language.Prototype.Frontend.Lexer.Identifier
import Language.Prototype.Frontend.Lexer.RegExp
import Language.Prototype.Frontend.Lexer.Scanner
import Language.Prototype.Frontend.Lexer.String
import Language.Prototype.Frontend.Lexer.Symbol

data Expression = Expression
type instance Lexer'Unit Expression = Char
instance Lexer'Environment' Expression where
  scanner _ = plain @Expression
  yield _ = undefined
  fulfill _ = undefined

data TopLevel = TopLevel
type instance Lexer'Unit TopLevel = Char
instance Lexer'Environment' TopLevel where
  scanner _ = plain @TopLevel
  yield _ = undefined
  fulfill _ = undefined

data Pattern = Pattern
type instance Lexer'Unit Pattern = Char
instance Lexer'Environment' Pattern where
  scanner _ = plain @Pattern
  yield _ = undefined
  fulfill _ = undefined
data TypeAnnotation = TypeAnnotation
type instance Lexer'Unit TypeAnnotation = Char
instance Lexer'Environment' TypeAnnotation where
  scanner _ = plain @TypeAnnotation
  yield _ = undefined
  fulfill _ = undefined
data TopLevel'TypeAnnotation = TopLevel'TypeAnnotation
type instance Lexer'Unit TopLevel'TypeAnnotation = Char
instance Lexer'Environment' TopLevel'TypeAnnotation where
  scanner _ = plain @TopLevel'TypeAnnotation
  yield _ = undefined
  fulfill _ = undefined
data FunctionTypeAnnotation = FunctionTypeAnnotation
type instance Lexer'Unit FunctionTypeAnnotation = Char
instance Lexer'Environment' FunctionTypeAnnotation where
  scanner _ = plain @FunctionTypeAnnotation
  yield _ = undefined
  fulfill _ = undefined
