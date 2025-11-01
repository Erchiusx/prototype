module Main (main) where

import Language.Prototype.Frontend.Lexer.Identifier

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  print $ read @Keyword "Let"
