module Language.Prototype.Ast.Decl where

import Language.Prototype.Ast.Types

data
  Decl
    identifier
    function
  = Decl'Var identifier
  | Decl'Fn function

instance Ast' (Decl i f) where
