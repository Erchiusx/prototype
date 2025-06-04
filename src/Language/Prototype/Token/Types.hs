module Language.Prototype.Token.Types
  ( Token (..)
  , Token' (..)
  , genKeywords
  , String'State (..)
  , genInstances
  )
where

import Control.Monad.State (State)
import Data.Char (toUpper)
import Data.String (IsString (..))
import Language.Haskell.TH

newtype String'State = String'State Integer
  deriving (Eq, Ord)
instance Num String'State where
  String'State a + String'State b = String'State (a + b)
  String'State a * String'State b = String'State (a * b)
  String'State a - String'State b = String'State (a - b)
  abs (String'State a) = String'State $ abs a
  signum (String'State a) = String'State (signum a)
  fromInteger a = String'State a

class Show a => Token' a where
  read'token
    :: String
    -> State String'State (Maybe (String, a))


data Token
  = forall a.
    Token' a =>
    Token a

instance Show Token where
  show (Token a) = show a

-- 将字符串转为构造子名：首字母大写
toConName :: String -> Name
toConName s = mkName $ capitalize s
 where
  capitalize [] = []
  capitalize (x : xs) = toUpper x : xs -- 强行大写首字母（ASCII）

-- Template Haskell 生成代码
genKeywords :: [String] -> Q [Dec]
genKeywords keywords = do
  let
    cons = map (\kw -> NormalC (toConName kw) []) keywords

    sigDecl =
      SigD
        (mkName "keywords")
        $ AppT ListT
        $ ConT ''String

    bindDecl =
      ValD
        (VarP $ mkName "keywords")
        (NormalB $ ListE (map (LitE . StringL) keywords))
        []

    dataDecl =
      DataD
        []
        (mkName "Keyword")
        []
        Nothing
        cons
        [DerivClause Nothing [ConT ''Show, ConT ''Eq]]

    fromStringClauses = map mkClause keywords ++ [fallback]
    mkClause kw =
      Clause
        [LitP (StringL kw)]
        (NormalB (ConE (toConName kw)))
        []
    fallback =
      Clause
        [WildP]
        ( NormalB
            ( AppE
                (VarE 'error)
                (LitE (StringL "Unknown keyword"))
            )
        )
        []

    instDecl =
      InstanceD
        Nothing
        []
        (AppT (ConT ''IsString) (ConT (mkName "Keyword")))
        [FunD 'fromString fromStringClauses]

  return [sigDecl, bindDecl, dataDecl, instDecl]

genInstances :: [String] -> Q [Dec]
genInstances typeNames =
  return $ (flip map) typeNames $ \typeName ->
    InstanceD
      Nothing
      []
      ( AppT
          (ConT $ mkName "Tokenable")
          (ConT $ mkName typeName)
      )
      [ FunD (mkName "make'token") $
          return $
            Clause
              []
              (NormalB $ ConE $ mkName $ "Token'" ++ typeName)
              []
      ]
