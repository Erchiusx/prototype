module Language.Prototype.Ast.Literal where
import Language.Prototype.Token qualified as T
import Language.Prototype.Ast.Identifier qualified as I
import Language.Prototype.Ast.Types
import Text.Parsec(manyTill)

data Literal
  = Num'Literal String (Maybe I.Identifier)
  | String'Literal [Char'Unit]
  | Template'String [[Char'Unit]] [Ast]

instance Ast' Literal where
  expect = do
    T.Token'Literal lt <- anyToken
    case lt of
      T.Num'Literal s identifier -> return $ Num'Literal s (identifier >>= I.identifier . T.make'token)
      T.String'Literal s -> return $ String'Literal s
      T.Partial'String'Literal s -> do
        undefined
      T.Close'String'Literal s -> undefined
