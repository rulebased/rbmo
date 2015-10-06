\hide{
\begin{code}
{-# LANGUAGE OverloadedStrings #-}
module Krdf.Kappa.Vocabulary (
  rbmoURI
  , namespaceRBMO

  , rbmoModel
  , rbmoKappa
  , rbmoBioNetGen

  , rbmoRule
  , rbmorate
  , rbmolhs
  , rbmolhsOf
  , rbmorhs
  , rbmorhsOf

  , rbmoAgent
  , rbmoagent

  , rbmoSite
  , rbmohasSite
  , rbmositeOf
  , rbmoisStatusOf
    
  , rbmoState
  , rbmohasState
  , rbmostateOf
  , rbmostatus
    
  , rbmoisBoundBy
  , rbmointernal
    
  , rbmoBoundState
  , rbmoUnboundState
  , rbmoUnspecifiedState

  , rbmoPattern
    
  , rbmoComplex
  , rbmoExpression
  , rbmoObservable
  ) where

import Data.Maybe(fromMaybe)
import Network.URI(URI, parseURI)
import Swish.Namespace (Namespace, ScopedName, makeNamespace, makeNSScopedName)
import Swish.QName (LName)

\end{code}
}

\begin{code}
------------------------------------------------------------
--  Namespace
------------------------------------------------------------

rbmoURI :: URI
rbmoURI = fromMaybe (error "Internal error processing RBMO URI") $ parseURI
         "http://purl.org/rbm/rbmo#"

-- | Maps @rbmo@ to <http://purl.org/rbm/rbmo#>
namespaceRBMO :: Namespace
namespaceRBMO = makeNamespace (Just "rbmo") rbmoURI

------------------------------------------------------------
--  Terms
------------------------------------------------------------

toR :: LName -> ScopedName
toR = makeNSScopedName namespaceRBMO

rbmoModel :: ScopedName
rbmoModel = toR "Model"

rbmoKappa :: ScopedName
rbmoKappa = toR "Kappa"

rbmoBioNetGen :: ScopedName
rbmoBioNetGen = toR "BioNetGen"

rbmoRule :: ScopedName
rbmoRule = toR "Rule"

rbmoAgent :: ScopedName
rbmoAgent = toR "Agent"

rbmoSite :: ScopedName
rbmoSite = toR "Site"

rbmoState :: ScopedName
rbmoState = toR "State"

rbmoPattern :: ScopedName
rbmoPattern = toR "Pattern"

rbmoComplex :: ScopedName
rbmoComplex = toR "Complex"

rbmoExpression :: ScopedName
rbmoExpression = toR "Expression"

rbmoObservable :: ScopedName
rbmoObservable = toR "Observable"

rbmorate :: ScopedName
rbmorate = toR "rate"

rbmolhs :: ScopedName
rbmolhs = toR "lhs"

rbmorhs :: ScopedName
rbmorhs = toR "rhs"

rbmorhsOf :: ScopedName
rbmorhsOf = toR "rhsOf"

rbmolhsOf :: ScopedName
rbmolhsOf = toR "lhsOf"

rbmoagent :: ScopedName
rbmoagent = toR "agent"

rbmoisBoundBy :: ScopedName
rbmoisBoundBy = toR "isBoundBy"

rbmointernal :: ScopedName
rbmointernal = toR "internal"

rbmoBoundState :: ScopedName
rbmoBoundState = toR "BoundState"

rbmoUnboundState :: ScopedName
rbmoUnboundState = toR "UnboundState"

rbmoUnspecifiedState :: ScopedName
rbmoUnspecifiedState = toR "UnspecifiedState"

rbmohasSite :: ScopedName
rbmohasSite = toR "hasSite"

rbmositeOf :: ScopedName
rbmositeOf = toR "siteOf"

rbmoisStatusOf :: ScopedName
rbmoisStatusOf = toR "isStatusOf"

rbmohasState :: ScopedName
rbmohasState = toR "hasState"

rbmostateOf :: ScopedName
rbmostateOf = toR "stateOf"

rbmostatus :: ScopedName
rbmostatus= toR "status"
\end{code}

% Local Variables:
% compile-command: "cd ../..; cabal build; cabal test"
% End:
