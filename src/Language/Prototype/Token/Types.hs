module Language.Prototype.Token.Types
  ( Token' (..)
  , genKeywords
  , genInstances
  , fullfill
  , Lexer'Environment (..)
  , Lexer'State (..)
  , Scanner
  , Char'Unit
  , to'repr
  , string'scanner
  , plain'scanner
  , identifier'scanner
  , lift
  , State'
  , Has'Name
  , HasField (..)
  , drop'middle
  , Lexer'Error (..)
  )
where

import Control.Monad.State
  ( MonadTrans (lift)
  , StateT
  )
import Data.Char (toUpper)
import Data.String (IsString (..))
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack)
import Language.Haskell.TH

class Show a => Token' a where
  read'token
    :: String
    -> State' (String, a)

data Lexer'Error
  = Unmatched'Environment
  { expected :: String
  , unexpected :: String
  }

type Scanner s =
  String -> State' (String, s)

type State' = StateT [Lexer'State] Maybe

fullfill
  :: forall n r s
   . ( Eq s
     , Lexer'Environment n r s
     )
  => String
  -> State' (String, (Maybe s), [s])
fullfill = go []
 where
  go
    :: [s]
    -> String
    -> State' (String, (Maybe s), [s])
  go acc ss = do
    (s', c) <- scan @n @r ss
    if ender @n @r c
      then
        return (s', Just c, reverse (c : acc))
      else
        go (c : acc) s'

type Has'Name n = HasField "name" n String
class
  HasField "name" n String =>
  Lexer'Environment n r s
    | n -> s
    , n -> r
  where
  scan :: Scanner s
  begin :: Scanner s
  ender :: s -> Bool
  close
    :: HasCallStack
    => String
    -> State' (String, Maybe s, r)

drop'middle :: (a, b, c) -> (a, c)
drop'middle (a, _, c) = (a, c)

instance IsString (n -> String) where
  fromString = const

data Lexer'State
  = forall s r n.
    ( Lexer'Environment n s r
    , HasField "name" n String
    ) =>
    Lexer'State n

type Char'Unit = (Bool, Char)

to'repr :: [Char'Unit] -> String
to'repr [] = ""
to'repr ((True, c) : cs) = '\\' : c : to'repr cs
to'repr ((False, c) : cs) = c : to'repr cs

string'scanner
  :: Scanner Char'Unit
string'scanner [] = lift Nothing
string'scanner ('\\' : []) = lift Nothing
string'scanner ('\\' : c : r) = lift $ Just (r, (True, c))
string'scanner (c : r) = lift $ Just (r, (False, c))

plain'scanner :: Scanner Char
plain'scanner [] = lift Nothing
plain'scanner (c : cs) = lift $ Just (cs, c)

identifier'scanner :: Scanner Char
identifier'scanner [] = lift Nothing
identifier'scanner (c : cs)
  | c
      `elem` ['a' .. 'z']
        ++ ['A' .. 'Z']
        ++ ['0' .. '9']
        ++ "'" =
      lift $
        Just (cs, c)
  | otherwise = lift $ Nothing

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
