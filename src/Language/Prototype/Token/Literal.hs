module Language.Prototype.Token.Literal where

import Control.Monad.State
  ( MonadState (get)
  , State
  , modify
  )
import GHC.Stack (HasCallStack)
import Language.Prototype.Token.Types
  ( String'State
  , Token' (..)
  )

data Literal
  = Num'Literal String
  | String'Literal String
  | Partial'String'Literal String
  deriving Show

type Char'Unit = (Bool, Char)

read'char :: String -> Maybe (String, Char'Unit)
read'char [] = Nothing
read'char ('\\' : []) = Nothing
read'char ('\\' : c : r) = Just (r, (True, c))
read'char (c : r) = Just (r, (False, c))

rec'read'char
  :: String
  -> [Char'Unit]
  -> Maybe (String, Char'Unit, [Char'Unit])
rec'read'char s stoppers = case read'char s of
  Nothing -> Nothing
  Just (r, c)
    | c `elem` stoppers ->
        return (r, c, [])
    | otherwise ->
        ((c :) <$>) `fmap` rec'read'char r stoppers

to'repr :: [Char'Unit] -> String
to'repr [] = ""
to'repr ((True, c) : cs) = '\\' : c : to'repr cs
to'repr ((False, c) : cs) = c : to'repr cs

instance Token' Literal where
  read'token = read'literal

read'literal
  :: HasCallStack
  => String
  -> State String'State (Maybe (String, Literal))
read'literal [] = return Nothing
read'literal s@(h : r)
  | h `elem` ['0' .. '9'] = do
      let (l, rest) =
            ( `elem`
                ( "~!@#$%^&*()_+-=[]\\{}|;':\",/<>? \t\n"
                    :: String
                )
            )
              `break` s
      return $ Just (rest, Num'Literal l)
  | h == '"' =
      return $ do
        (rest, _, units) <- rec'read'char r [(False, '"')]
        return (rest, String'Literal $ to'repr units)
  | h == '`' = do
      let res = do
            (rest, stopper, units) <-
              rec'read'char r [(False, '`'), (True, '{')]
            case stopper of
              (False, '`') ->
                return
                  (rest, String'Literal $ to'repr units)
              (True, '{') ->
                return
                  ( rest
                  , Partial'String'Literal $
                      to'repr units
                  )
              _ -> undefined
      case res of
        Nothing -> return Nothing
        Just (_, (String'Literal _)) -> return res
        Just (_, (Partial'String'Literal _)) -> do
          modify (+ 1)
          return res
        _ -> undefined
  | h == '}' = do
      state <- get
      if state == 0
        then return Nothing
        else do
          let res = do
                (rest, stopper, units) <-
                  rec'read'char r [(False, '`'), (True, '{')]
                case stopper of
                  (False, '`') ->
                    return
                      (rest, String'Literal $ to'repr units)
                  (True, '{') ->
                    return
                      ( rest
                      , Partial'String'Literal $
                          to'repr units
                      )
                  _ -> undefined
          case res of
            Nothing -> return Nothing
            Just (_, (Partial'String'Literal _)) ->
              return res
            Just (_, (String'Literal _)) -> do
              modify (+ (-1))
              return res
            _ -> undefined
  | otherwise = return Nothing
