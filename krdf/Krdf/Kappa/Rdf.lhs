\hide{
\begin{code}
{-# LANGUAGE NamedFieldPuns, OverloadedStrings #-}
module Krdf.Kappa.Rdf (
  annotations
  , materialise
  , turtle
  ) where

import Prelude hiding (concat)
import qualified Data.Map as M
import qualified Data.HashMap.Lazy as H
import qualified Data.Set as S
import Data.Text(Text, concat, append, pack, unpack)
import Data.Text.Lazy(fromStrict)
import Krdf.Kappa(Statement(..), Rule(..), AgentP(..), SiteP, LinkP(..), StateP(..))
import Krdf.Kappa.Vocabulary
import qualified Swish.RDF as RDF
import Swish.Namespace(makeScopedName)
import Swish.QName(newLName)
import Swish.RDF.Graph(RDFGraph, RDFLabel(..), arc, addArc, namespaces, newNode)
import Swish.RDF.Parser.Turtle(parseTurtlefromText)
\end{code}
}

\begin{code}
-- | Extract the explicit RDF/turtle annotations from a list of statements
turtle :: [Statement] -> Text
turtle = concat . map onlyText . filter isRdf
  where
    isRdf (RDF _)    = True
    isRdf _          = False
    onlyText (RDF t) = append t (pack "\n")
    onlyText _       = undefined

-- | Translate the explicit RDF/turtle annotations into an RDF graph
annotations :: [Statement] -> RDFGraph
annotations statements =
  case parseTurtlefromText (fromStrict $ turtle statements) of
   Left err -> error err
   Right g  -> g

-- | Materialise information latent in the Kappa representation into RDF
-- | TODO: possibly handle statement types other than rule declarations
materialise :: [Statement] -> RDFGraph -> RDFGraph
materialise [] g = g
materialise (RD rule:rest) g = materialise rest (ruleToRDF rule g)
materialise (_:rest)       g = materialise rest g

-- | Produce RDF statements about a rule, and add them to the graph
-- | TODO: we should also materialise the rate expression, but for
-- |       now just leave a dangling blank node...
ruleToRDF :: Rule -> RDFGraph -> RDFGraph
ruleToRDF Rule { desc, lhs, rhs } g = rhsg
  where
    nrule     = lname g desc  -- rdf resource for the rule name
    brule     = newBnode g "rule" -- bnode as base for bindings
    nrate     = newBnode g "rate" -- bnode for rate
    (alhs, _) = lhs -- a half-rule contains agents and tokens
    (arhs, _) = rhs -- ditto
    -- the top level statements about the rule. minimally, its
    -- type, and linkage for rate expression and lhs, rhs parts
    triples   = [ arc nrule RDF.resRdfType (Res rbmoRule)
                , arc nrule (Res rbmorate) nrate
                ]
    ruleg     = foldl raddArc g triples
    lhsg      = agentPats alhs brule nrule (Res rbmolhs) ruleg
    rhsg      = agentPats arhs brule nrule (Res rbmorhs) lhsg

-- | Simple tail-recursive utility to iterate through agent patterns
agentPats :: [AgentP] -> RDFLabel -> RDFLabel -> RDFLabel -> RDFGraph -> RDFGraph
agentPats (ap:as) b r p g = agentPatToRDF ap b r p nextg
  where nextg = agentPats as b r p g
agentPats []      _ _ _ g = g

-- | Produce RDF statements about an agent pattern
-- | The arguments are, the agent itself, a base blank node
-- | from which bindings are to be generated, the anchor
-- | referring to the rule's right/left side, and the graph.
agentPatToRDF :: AgentP -> RDFLabel -> RDFLabel -> RDFLabel -> RDFGraph -> RDFGraph
agentPatToRDF (AgentP name sites) b r p g = siteg
  where
    npat    = newBnode g "pat"
    triples = [ arc r p npat
              , arc npat RDF.resRdfType (Res rbmoPattern)
              , arc npat (Res rbmoagent) (lname g name)
              ]
    ag      = foldl raddArc g triples
    siteg   = sitePats name (H.toList sites) b npat ag

-- | Simple tail-recursive utility to iterate through site patterns
sitePats :: Text -> [(Text, SiteP)] -> RDFLabel -> RDFLabel -> RDFGraph -> RDFGraph
sitePats a (s:ss) b an g = sitePatToRDF a s b an nextg
  where nextg = sitePats a ss b an g
sitePats _ []     _ _  g = g

-- | Produce RDF statements about a site patern
-- | The arguments are similar to `agentPatToRDF` although in
-- | this case, the anchor refers to the agent pattern.
sitePatToRDF :: Text -> (Text, SiteP) -> RDFLabel -> RDFLabel -> RDFGraph -> RDFGraph
sitePatToRDF agent (name, (link, state)) (Blank b) anchor g = siteg
  where
    nsite  = newBnode g "site"
    -- bound state, construct a blank node from the given blank
    -- node b to ensure consistent naming for "rule scope"
    linkState (Link l) =
      [ arc nsite RDF.resRdfType (Res rbmoBoundState)
      , arc nsite (Res rbmoisBoundBy) (Blank $ b ++ "_" ++ unpack l) ]
    -- when we know the site is bound, but we do not know to what
    -- use a new blank node
    linkState Bound =
      [ arc nsite RDF.resRdfType (Res rbmoBoundState)
      , arc nsite (Res rbmoisBoundBy) (newBnode g "binding") ]
    -- unbound and maybebound use special rbmo terms
    linkState Unbound =
      [ arc nsite RDF.resRdfType (Res rbmoUnboundState) ]
    linkState MaybeBound =
      [ arc nsite RDF.resRdfType (Res rbmoUnspecifiedState) ]
    -- internal state
    iState (State s) =
      [ arc nsite (Res rbmointernal) (lname g $ istatename s) ]
    iState _    = []
    -- XXX ugly
    sitename = pack $ (unpack agent) ++ ":" ++ (unpack name)
    istatename s = pack $ (unpack sitename) ++ ":" ++ (unpack s)
    triples = [ arc anchor (Res rbmostatus) nsite
              , arc nsite (Res rbmoisStatusOf) (lname g sitename)
              ] ++ linkState link ++ iState state
    siteg = foldl raddArc g triples
sitePatToRDF _ _ _ _ _ = undefined
\end{code}

\hide{
\begin{code}
{- Various utility functions -}

-- | Construct a local name from the RDF graph's empty namespace
-- | (which is assumed to exist and have been declared). This is
-- | used to construct references to rules and agents defined in
-- | the Kappa file and, presumably, annotated
lname :: RDFGraph -> Text -> RDFLabel
lname g n = Res $ makeScopedName Nothing base name
  where 
    name = case newLName n of
      Just ln -> ln
      Nothing -> error $ "Local name '" ++ (unpack n) ++ "' is not valid"
    base = case M.lookup Nothing $ namespaces g of
      Just u  -> u
      Nothing -> error "The empty prefix (:) is not declared in the source graph"

-- | Get a new, fresh bnode based on the given string, unique for
-- | the graph
newBnode :: RDFGraph -> String -> RDFLabel
newBnode g n = newNode (Blank n) existing
  where
    existing = S.toList $ RDF.allLabels RDF.isBlank g

-- | A version of addArc with arguments reversed suited to fold
raddArc :: RDF.Label lb => RDF.NSGraph lb -> RDF.Arc lb -> RDF.NSGraph lb
raddArc t a = addArc a t
\end{code}
}

% Local Variables:
% compile-command: "cd ../..; cabal build; cabal test"
% End:
