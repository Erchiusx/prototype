module Language.Prototype.Ast.Expression where

data
  Expression
    identifier
    literal
    operator
  = Exp'Val identifier
  | Exp'lit literal
  | Exp'Arithmatic [Expression identifier literal operator] [operator]


