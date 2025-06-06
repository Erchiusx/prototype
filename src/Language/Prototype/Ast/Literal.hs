module Language.Prototype.Ast.Literal where
import Language.Prototype.Token qualified as T
import Language.Prototype.Ast.Types
import Text.Parsec(manyTill)

data Literal
  = Num'Literal String
  | String'Literal String
  | Template'String [String] [Ast]

-- instance Ast' Literal where
--   expect = do
--     T.Token'Literal lt <- anyToken
--     case lt of
--       T.Num'Literal s -> return $ Num'Literal s
--       T.String'Literal s -> return $ String'Literal s
--       T.Partial'String'Literal s -> do
--         undefined