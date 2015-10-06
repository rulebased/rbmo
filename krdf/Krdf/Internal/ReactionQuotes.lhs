\hide{
\begin{code}
{-
    Reaction Networks -- QuasiQuote syntax
    Copyright (C) 2015 William Waites

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}
module Krdf.Internal.ReactionQuotes(reactions, reactionTest) where

import Prelude hiding (takeWhile)

import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Control.Applicative ((<|>), many)
import Data.Attoparsec.Text
import Data.Text(pack, unpack)
\end{code}
}

\begin{code}
-- | parse an integer
integral :: Parser Exp
integral = do
  i <- decimal
  return $ LitE $ integerL i

-- | parse a decimal/rational number
real :: Parser Exp
real = do
  r <- rational
  return $ LitE $ rationalL r
  
-- | parse a datatype constructor
constructor :: Parser Exp
constructor = do
  first <- satisfy $ inClass "A-Z"
  rest  <- takeWhile (inClass "A-Za-z0-9")
  return $ ConE (mkName (first:unpack rest))

-- | parse a variable
variable :: Parser Exp
variable = do
  first <- satisfy $ inClass "a-z"
  rest  <- takeWhile (inClass "A-Za-z0-9")
  return $ VarE $ mkName (first:unpack rest)

-- | parses "2A" or just "A"
speciesCount :: Parser Exp
speciesCount = counted <|> onlyone
  where counted = do c <- integral
                     _ <- many space
                     s <- constructor
                     return $ TupE [s, c]
        onlyone = do s <- constructor
                     return $ TupE [s, LitE $ integerL 1]

-- | parses "speciesCount + speciesCount + ..."
reactionClause :: Parser Exp
reactionClause = do
  sc <- sepBy speciesCount $ (many space) >> char '+' >> (many space)
  return $ ListE sc
  
-- | parses @ + numeric rate or symbolic variablerate
rate :: Parser Exp
rate = char '@' >> (real <|> variable)

-- | parses entire reaction "lhs -> rhs @k"
reactionExpression :: Parser Exp
reactionExpression = do
  lhs <- reactionClause
  _ <- many space
  _ <- string $ pack "->"
  _ <- many space
  rhs <- reactionClause
  _ <- many space
  k <- rate

  let lhsSym        = mkName "lhs"
      rhsSym        = mkName "rhs"
      rateSym       = mkName "rate"
      -- subObptimal requiring fromOccurList to be exposed
      -- but it makes it a lot easier to build up the multisets
      -- and means we don't need to know internal details about
      -- the MultiSet libraray. this function is re-exported by
      -- Krdf.Reaction so simply importing that unqualified
      -- should be enough to make the whole reactions QQ thing
      -- work
      fromOccurList = mkName "fromOccurList"
  return $ RecConE (mkName "Reaction") [
    (lhsSym, AppE (VarE fromOccurList) lhs),
    (rhsSym, AppE (VarE fromOccurList) rhs),
    (rateSym, k)
    ]

-- | parse a list of reactions, strip leading and trailing spaces
-- | and clamp to end of input
reactionParser :: Parser Exp
reactionParser = do
  _ <- many space
  rs <- sepBy reactionExpression $ (many space) >> char ',' >> (many space)
  _ <- many space
  _ <- endOfInput
  return $ ListE rs

-- | The quasiquote structure that's actually used. This glues the
-- | parser to the [reactions| 2A + B -> C @1.0 |] syntax. The syntax
-- | return a list of reactions which are specified as comma separated
-- | statements as above.
reactions :: QuasiQuoter
reactions = QuasiQuoter {
  quoteExp = \s -> case parseOnly reactionParser (pack s) of
                    Left err -> error err
                    Right q -> return q
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

-- | helper function for debugging parsing of reactions from the REPL
reactionTest :: String -> IO ()
reactionTest s = case parseOnly reactionParser (pack s) of
  Right r  -> print $ ppr r
  Left err -> error $ err
\end{code}

% Local Variables:
% compile-command: "cd ../..; cabal build && cabal test"
% End:
