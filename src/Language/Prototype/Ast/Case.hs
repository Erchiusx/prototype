module Language.Prototype.Ast.Case where

newtype Case p b = Case [Branch p b]

data Branch pattern' block = Branch pattern' block
