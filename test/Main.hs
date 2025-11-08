module Main (main) where

import Data.Aeson (encode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy as BL
import Data.Text.IO qualified as T
import Language.Prototype.Frontend.Lexer qualified as L
import Language.Prototype.Frontend.Lexer.RegExp
  ( Env'RegExp (Env'RegExp)
  )
import Language.Prototype.Frontend.Lexer.Types
  ( skip'space, getInput, Lexer'Environment (Lexer'State)
  )
import qualified Data.Text
import Language.Prototype.Frontend.Lexer.String (Env'String(Env'String), String'Component'Type (Enter'String))

main :: IO ()
main = do
  file <- T.readFile "test/test.p"
  -- T.putStrLn $ Data.Text.take 18 file
  case L.lex "test.p" file  of
    Left err -> print err
    Right res -> BL.writeFile "test/test.json" $ encodePretty res
