module Language.Prototype.Token.Literal
  ( Literal (..)
  , Identifier (..)
  , Template'String (..)
  , String'Interpolation (..)
  , Digit'Postfix (..)
  ) where

import Control.Applicative (asum)
import Control.Monad (guard)
import Control.Monad.State
  ( MonadState (get)
  , modify
  )
import Data.Char (isAscii)
import Data.Maybe (listToMaybe)
import Data.String (IsString (fromString))
import Language.Prototype.Token.Types

data Literal
  = Num'Literal String (Maybe Identifier)
  | String'Literal [Char'Unit]
  | Partial'String'Literal [Char'Unit]
  | Close'String'Literal [Char'Unit]
  deriving Show

newtype Identifier
  = Identifier String
  deriving Show

instance IsString Identifier where
  fromString = Identifier

instance Token' Identifier where
  read'token [] = lift Nothing
  read'token s@(h : _) = do
    guard $ h `elem` ['a' .. 'z'] ++ ['A' .. 'Z']
    let (w, r) =
          ( not
              . ( `elem`
                    ['a' .. 'z']
                      ++ ['A' .. 'Z']
                      ++ ['0' .. '9']
                      ++ "'"
                )
          )
            `break` s
    return (r, fromString w)

instance HasField "name" Identifier String where
  getField = "identifier"
instance
  Lexer'Environment
    Identifier
    (Maybe Identifier)
    Char
  where
  scan = identifier'scanner
  begin = \s -> do
    (cs, c) <- identifier'scanner s
    guard $ isAscii c
    return (cs, c)
  close [] = lift Nothing
  close s@(h : _) = do
    guard $ h `elem` ['a' .. 'z'] ++ ['A' .. 'Z']
    let (w, r) =
          ( not
              . ( `elem`
                    ['a' .. 'z']
                      ++ ['A' .. 'Z']
                      ++ ['0' .. '9']
                      ++ "'"
                )
          )
            `break` s
    return (r, listToMaybe r, Just $ fromString w)
  ender =
    not
      . ( `elem`
            '\''
              : ['a' .. 'z']
              ++ ['A' .. 'Z']
              ++ ['0' .. '9']
        )

digit :: Scanner Char
digit [] = lift Nothing
digit (c : cs)
  | elem c $ ['0' .. '9'] ++ ",." = return (cs, c)
  | otherwise = lift Nothing

data Digit'Front'Part
instance HasField "name" Digit'Front'Part String where
  getField = "digit front part"
instance Lexer'Environment Digit'Front'Part String Char where
  scan = digit
  begin = \s -> do
    (cs, c) <- digit s
    guard (c `elem` ['0' .. '9'])
    return (cs, c)
  ender = not . (`elem` ['0' .. '9'] ++ ",")
  close = fullfill @Digit'Front'Part

data Digit'Back'Part
instance HasField "name" Digit'Back'Part String where
  getField = "digit back part"
instance Lexer'Environment Digit'Back'Part String Char where
  scan = digit
  begin = digit
  ender = not . (`elem` ['0' .. '9'] ++ ",")
  close = \s ->
    let (l, r) = (not . (`elem` ['0' .. '9'] ++ ",")) `break` s
     in return (r, listToMaybe r, l)

data Digit'Postfix = Digit'Postfix
instance HasField "name" Digit'Postfix String where
  getField = "digit postfix"

instance
  Lexer'Environment
    Digit'Postfix
    Literal
    Char
  where
  scan = undefined
  ender = undefined
  begin = begin @Digit'Front'Part @String
  close = \s -> do
    (rest, dot, front) <- close @Digit'Front'Part s
    let
      get'postfix
        :: String
        -> Maybe Char
        -> String
        -> State' (String, Maybe Char, Literal)
      get'postfix s' c number = do
        _ <- begin @Identifier s'
        (rest', t, i) <-
          close @Identifier s'
        case i of
          Nothing -> return (s', c, Num'Literal number i)
          Just r ->
            return $
              (Num'Literal number) . Just <$> (rest', t, r)
    case dot of
      Just '.' -> do
        (rest', c, back) <- close @Digit'Back'Part rest
        get'postfix rest' c $ front ++ back
      _ ->
        get'postfix rest dot front

-- data Plain'String = Plain'String
-- instance HasField "name" Plain'String String where
--   getField = "plain string"
-- instance
--   Lexer'Environment
--     Plain'String
--     (Literal)
--     Char'Unit
--   where
--   scan = string'scanner
--   begin = \s -> do
--     (s', (False, '"')) <- string'scanner s
--     return $ (s',) (False, '"')
--   ender = (== (False, '"'))
--   close = \s -> do
--     (s', _) <-
--       begin @Plain'String s
--     (with'quote, end, consumed) <-
--       fullfill @Plain'String s'
--     let without'quote = case end of
--           Nothing -> with'quote
--           Just _ -> tail without'quote
--     return
--       (without'quote, end, String'Literal $ consumed)

data Template'String = Template'String
instance HasField "name" Template'String String where
  getField = "template string"
instance
  Lexer'Environment
    Template'String
    Literal
    Char'Unit
  where
  scan = string'scanner
  begin = \s -> do
    (s', c) <- string'scanner s
    n <- get
    case n of
      [] -> do
        guard $ c == (False, '"')
        return (s', c)
      Lexer'State e : _
        | e.name == "string interpolation" -> do
            guard $ c `elem` [(False, '}'), (False, '"')]
            return (s', c)
        | otherwise -> lift Nothing
  ender = (`elem` [(False, '"'), (True, '{')])
  close s = do
    (s', (False, c')) <- begin @Template'String s
    case c' of
      '"' -> do
        (with'quote, end, consumed) <-
          fullfill @Template'String s'
        case end of
          Nothing -> do
            return
              ( with'quote
              , end
              , Partial'String'Literal consumed
              )
          Just c -> do
            let without'quote = drop 1 with'quote
            if c == (False, '{')
              then do
                modify (Lexer'State Template'String :)
                return
                  ( without'quote
                  , end
                  , Partial'String'Literal consumed
                  )
              else
                return
                  ( without'quote
                  , end
                  , String'Literal consumed
                  )
      '}' -> do
        (with'quote, end, consumed) <-
          fullfill @Template'String s'
        case end of
          Nothing -> do
            modify $ drop 1
            return
              (with'quote, end, Close'String'Literal consumed)
          Just c -> do
            let without'quote = drop 1 with'quote
            if c == (False, '{')
              then
                return
                  ( without'quote
                  , end
                  , Partial'String'Literal consumed
                  )
              else do
                modify $ drop 2
                return
                  ( without'quote
                  , end
                  , Close'String'Literal consumed
                  )
      _ -> undefined

data String'Interpolation = String'Interpolation
instance HasField "name" String'Interpolation String where
  getField = "string interpolation"

instance Lexer'Environment String'Interpolation () Char

instance Token' Literal where
  read'token s =
    asum
      [ drop'middle <$> close @Digit'Postfix s
      , drop'middle <$> close @Template'String s
      ]
