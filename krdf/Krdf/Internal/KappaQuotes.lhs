\hide{
\begin{code}
{-
    Reaction Networks -- Kappa Quasi Quotes
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
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Krdf.Internal.KappaQuotes(agent, complex, rule) where

import Data.Attoparsec.Text
import Data.Text(pack)
import Control.Applicative(many)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Krdf.Internal.KappaAST
import Krdf.Internal.KappaParser
\end{code}
}

\begin{code}
strip :: Parser a -> Parser a
strip p = do
  _ <- many space
  a <- p
  _ <- many space
  return a

agent :: QuasiQuoter
agent = QuasiQuoter {
  quoteExp = \s -> case parseOnly (strip agent_sig) (pack s) of
                    Left err -> error err
                    Right q -> lift q
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

complex_q :: Parser [AgentP]
complex_q = strip $ agent_pat `sepBy` (many space >> char ',' >> many space)

complex :: QuasiQuoter
complex = QuasiQuoter {
  quoteExp = \s -> case parseOnly complex_q (pack s) of
                    Left err -> error err
                    Right q -> lift q
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

rule :: QuasiQuoter
rule = QuasiQuoter {
  quoteExp = \s -> case parseOnly (strip rule_pat) (pack s) of
                    Left err -> error err
                    Right q -> lift q
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }
\end{code}

% Local Variables:
% compile-command: "cd ../..; stack test"
% End:
