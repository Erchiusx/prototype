module Main (main) where

import Data.String.Template (str)
import Language.Prototype.Token

main :: IO ()
main = do
  print $
    [str|4
    abc
    |]
