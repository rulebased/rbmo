module Krdf.Utils(basename, mkQuery) where

import qualified Data.Text.Lazy as T
import Swish.RDF.Parser.N3 (parseN3fromText)

-- shamelessly stolen from System.Environment's implementation
basename :: String -> String
basename f = go f f
  where
    go acc [] = acc
    go acc (x:xs)
      | isPathSeparator x = go xs xs
      | otherwise         = go acc xs
    isPathSeparator :: Char -> Bool
    isPathSeparator '/'  = True
    isPathSeparator _    = False

mkQuery lines = qparse $ T.pack $ unlines lines
  where qparse = either error id . parseN3fromText
    

