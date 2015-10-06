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
module Krdf.Kappa (
  kappaParser
  , parseKappa
  , agent
  , complex
  , rule
  , site, sites
  , links
  , state
  , decmap
  , norma
  , checka
  , consistent
  , collect
  , deriveDec
  , normalise
  , Statement(..)
  , AgentD(..)
  , VarD(..)
  , TokD(..)
  , Rule(..)
  , AgentP(..)
  , SiteP
  , LinkP(..)
  , StateP(..)
  , TokE(..)
  , Expr(..)
  , Obs(..)
  , Init(..)
  ) where

import Prelude hiding (lookup)
import Krdf.Internal.KappaParser(kappaParser, parseKappa)
import Krdf.Internal.KappaQuotes(agent, complex, rule)
import Krdf.Internal.KappaUtil(site, sites, state, links)
import Krdf.Internal.KappaAST
import qualified Data.HashMap.Lazy as H
import qualified Data.List as L
import Data.Text(Text)
\end{code}
}

\begin{code}
type DecMap = H.HashMap Text AgentD

-- | make a "compiled" map of agent name -> declaration.
decmap :: [AgentD] -> DecMap
decmap ds = H.fromList [ (n, a) | a@(AgentD n _) <- ds ]

-- | normalise an agent pattern according to the
-- | appropriate declarations.
norma :: DecMap -> AgentP -> AgentP
norma dmap a@(AgentP name ss) =
  case H.lookup name dmap of
   Nothing              -> a
   Just (AgentD _ decs) -> AgentP name allsites
     where
       defdec []         = Undefined
       defdec (sstate:_) = State sstate
       merge (_, s) (l, Undefined) = (l, s)
       merge _      (l,s)          = (l, s)
       defaults = H.map (\dec -> (MaybeBound, defdec dec)) decs
       allsites = H.unionWith merge defaults ss

-- | Normalisa a collection of statements. This means filtering
-- | out agent declarations and constructing a `DecMap` and then
-- | going through each of the agent patterns in the lhs/rhs of
-- | each of the rules and expanding them using `norma`
normalise :: [Statement] -> [Statement]
normalise statements = norm statements
  where
    dm = decmap $ map dec $ filter isDec statements
    dec (AD d)   = d
    dec _        = undefined
    isDec (AD _) = True
    isDec _      = False
    norm []        = []
    norm (RD r:ss)  = (normRule r:norm ss)
    norm (s:ss)    = (s:norm ss)
    normRule r = RD $ r { lhs = (map (norma dm) lagents, ltoks)
                        , rhs = (map (norma dm) ragents, rtoks) }
      where (lagents, ltoks) = lhs r
            (ragents, rtoks) = rhs r
    
checka :: DecMap -> AgentP -> Maybe [(Text, Maybe Bool)]
checka dmap (AgentP an ss) =
  fmap checkSites $ H.lookup an dmap
  where
    checkSites (AgentD _ decs) = map (checkSite decs) (H.toList ss)
    checkSite decs (sn, (_, Undefined))  =
      (sn, fmap (\_ -> True) (H.lookup sn decs))
    checkSite decs (sn, (_, State s)) =
      (sn, fmap (\t -> elem s t) (H.lookup sn decs))

consistent :: DecMap -> AgentP -> Bool
consistent dmap a =
  case (fmap checkall . checka dmap) a of
   -- No declaration for this agent
   Nothing           -> False
   -- No declaration for a site
   Just Nothing      -> False
   -- There is a site with an invalid state
   Just (Just False) -> False
   -- All sites are valid
   Just (Just True)  -> True
  where
    checkall = fmap (foldl (&&) True) . mapM (\(_, c) -> c)

collect :: [AgentP] -> [[AgentP]]
collect []     = []
collect (c:cs) =
  (withE:(collect withoutE))
  where hasLink e = L.elem e . links
        sharesLink c1 c2 = any (\e -> hasLink e c2) (links c1)
        (withE, withoutE) = L.partition (sharesLink c) (c:cs)

{- TODO
match :: [AgentP] -> [AgentP] -> Bool
match [] [] = True
match [] _  = False
match _  [] = False
-}
-- splitL l (hasL, notL) a =
--   if L.elem l $ links a
--   then (hasL ++ [a], notL)
--   else (hasL, notL ++ [a])
\end{code}

\hide{
\begin{code}
-- | Derive agent declarations from a list of agent patterns
-- | as would be found in rules.
deriveDec :: [AgentP] -> [AgentD]
deriveDec agents =
  -- create a list (name, site, state) observed from the list of agents
  let aa = [ (name, s, state s a) | a@(AgentP name _) <- agents, s <- sites a ]
  -- separate into a list of lists by agent, and make a declaration for each
  in map mkagent (L.groupBy groupA $ L.sort aa)
  where
    -- predicate for grouping by agent (used above)
    groupA (n1, _, _) (n2, _, _) = n1 == n2
    -- predicate for grouping by site (used below)
    groupS (_, s1, _) (_, s2, _) = s1 == s2
    -- make an agent declaration
    mkagent :: [(Text, Text, Maybe StateP)] -> AgentD
    mkagent a =
      let (name, _, _):_ = a in
      AgentD name $ H.fromList $ map mksite $ L.groupBy groupS a
    -- make a site declaration
    mksite :: [(Text, Text, Maybe StateP)] -> (Text, [Text])
    mksite ss =
      let
        (_, sn, _):_ = ss
        states = [ st | (_, _, sx@(Just (State st))) <- ss, justState sx ]
      in
       -- an internal state that only appears once isn't worth the bother
       (sn, if length states > 1 then states else [])
    -- predicate for filtering out explicit states
    justState (Just _) = True
    justState _        = False
\end{code}
}

% Local Variables:
% compile-command: "cd ..; cabal build; cabal test"
% End:
