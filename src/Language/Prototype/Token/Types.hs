module Language.Prototype.Token.Types
  ( Lexer'State
  , Lexer'State' (..)
  , top'env
  , Lexer'Environment' (..)
  , Lexer'Environment (..)
  , Token' (..)
  , (<|>)
  , nothingT
  , move
  , Plain
  , Lexer'Unit (..)
  , genKeywords
  )
where

-- data Surround'Type
--   = Curly
--   | Bracket
--   | Round

-- data Digit'Literal'Part
--   = Digit'Literal'Front
--   | Digit'Literal'Trail
--   | Digit'Literal'Postfix
-- re-export modules
import Control.Applicative ((<|>))
import Control.Monad.Trans qualified as Trans

-- import for usage
import Control.Monad.State
  ( StateT
  , get
  , modify
  , put
  )
import Data.Char (toUpper)
import Data.String (IsString (..))
import Language.Haskell.TH.Syntax
import Text.Parsec
  ( SourcePos
  , incSourceColumn
  , incSourceLine
  )

data Lexer'State' a
  = Lexer'State
  { position :: a
  , environments :: [Lexer'Environment]
  }
  deriving Show

type Lexer'State = Lexer'State' SourcePos

instance Functor Lexer'State' where
  fmap f (Lexer'State pos envs) =
    Lexer'State
      (f pos)
      envs

type State = StateT Lexer'State Maybe
nothingT :: State a
nothingT = Trans.lift Nothing

move :: Int -> Int -> State ()
move a b =
  modify
    ( (`incSourceColumn` a)
        . (`incSourceLine` b)
        <$>
    )

top'env :: State Lexer'Environment
top'env = do
  Lexer'State{environments = top : _} <- get
  return top

class Lexer'Unit unit where
  size :: unit -> Int
  wrap'lines :: unit -> Int

class
  Lexer'Unit unit =>
  Lexer'Environment' a unit
    | a -> unit
  where
  scan'unit :: String -> State (unit, String)
  scan'unit' :: String -> State (unit, String)
  scan'unit' s = do
    (u, s') <- scan'unit @a s
    move (size u) (wrap'lines u)
    return (u, s')
  is'ender :: unit -> Bool

data Lexer'Environment
  = forall a u.
    ( Show a
    , Show u
    , Lexer'Environment' a u
    ) =>
    Lexer'Environment a

instance Show Lexer'Environment where
  show (Lexer'Environment a) = show a

class Token' a where
  scan'token :: String -> State (a, String)

data Plain = Plain deriving Show
instance Lexer'Unit Char where
  size = const 1
  wrap'lines = (\t -> if t then 1 else 0) . (== '\n')
instance Lexer'Environment' Plain Char where
  scan'unit [] = nothingT
  scan'unit (c : cs) = return (c, cs)

  is'ender = (`elem` ("([{\"" ++ ['0' .. '9']))

-- Plain
-- Surround Surround'Type
-- Comment { inline'comment :: Bool }
-- String'Literal
-- String'Interpolation
-- Digit'Literal Digit'Literal'Part

genKeywords :: [String] -> Q [Dec]
genKeywords kws = do
  let
    keywords = map ((`NormalC` []) . make'keyword'name) kws
    data'declare =
      DataD
        []
        (mkName "Keyword")
        []
        Nothing
        keywords
        [DerivClause Nothing [ConT ''Show]]
    sig'declare =
      SigD
        (mkName "keywords")
        $ AppT ListT
        $ ConT ''String
    bind'declare =
      ValD
        (VarP $ mkName "keywords")
        (NormalB $ ListE (map (LitE . StringL) kws))
        []
    from'string'clauses = map make'clause kws ++ [fallback]
    make'clause kw =
      Clause
        [LitP (StringL kw)]
        (NormalB (ConE $ make'keyword'name kw))
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

    inst'declare =
      InstanceD
        Nothing
        []
        (AppT (ConT ''IsString) (ConT (mkName "Keyword")))
        [FunD 'fromString from'string'clauses]
  return
    [ data'declare
    , sig'declare
    , bind'declare
    , inst'declare
    ]
 where
  make'keyword'name :: String -> Name
  make'keyword'name (c : cs) = mkName $ "Keyword'" ++ toUpper c : cs
