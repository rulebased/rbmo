\hide{
\begin{code}
{-
    Reaction Networks
    Copyright (C) 2014,2015 William Waites

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
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, NamedFieldPuns, Rank2Types, QuasiQuotes #-}
module Krdf.Reaction(
  Species,
  Reaction(..),
  reactions,
  reactionTest,
  fromOccurList
  ) where

import Data.MultiSet(MultiSet, fromOccurList)
import Krdf.Internal.ReactionQuotes (reactions, reactionTest)
\end{code}
}

\begin{code}
class Ord s => Species s

data Reaction s a = Reaction { lhs  :: MultiSet s
                             , rhs  :: MultiSet s
                             , rate :: a }
                  deriving (Show, Eq)
\end{code}

% Local Variables:
% compile-command: "cd ..; cabal build && cabal test"
% End:
