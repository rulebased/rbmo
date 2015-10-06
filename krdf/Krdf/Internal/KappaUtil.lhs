\hide{
\begin{code}
{-
    Reaction Networks -- Kappa
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
module Krdf.Internal.KappaUtil where

import Data.Text(Text)
import Krdf.Internal.KappaAST
import qualified Data.HashMap.Lazy as H

site :: Text -> AgentP -> Maybe (LinkP, StateP)
site x (AgentP _ sp) = H.lookup x sp

sites :: AgentP -> [Text]
sites (AgentP _ sp) = H.keys sp

state :: Text -> AgentP -> Maybe StateP
state x a = case site x a of
  Nothing -> Nothing
  Just (_, s) -> Just s

links :: AgentP -> [Text]
links (AgentP _ ss) = map edge $ filter attached $ H.elems ss
  where
    attached (Link _, _) = True
    attached _           = False
    edge (Link l, _)     = l
    edge _               = undefined
\end{code}
}
% Local Variables:
% compile-command: "cd ../..; cabal build && cabal test"
% End:
