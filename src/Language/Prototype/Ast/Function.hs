module Language.Prototype.Ast.Function where

import Language.Prototype.Ast.Identifier
import Language.Prototype.Ast.Types

data
  Function
    param
    block
  = Function
  { param :: param
  , body :: block
  , name :: Maybe Identifier
  }

instance (Ast' p, Ast' b) => Ast' (Function p b)
