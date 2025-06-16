module Data.String.Template where
import Language.Haskell.TH
import Language.Haskell.TH.Quote

str :: QuasiQuoter
str = QuasiQuoter
  { quoteExp  = \s -> do
      let
        h:l = lines s
        spaces = read @Int h
      return $ LitE $ StringL $ unlines $ drop spaces `map` l
  , quotePat  = unsupported "pattern"
  , quoteType = unsupported "type"
  , quoteDec  = unsupported "declaration"
  }
  where
    unsupported ctx = error $ ctx ++ " context is not supported by [str]"
