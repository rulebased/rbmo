\hide{
\begin{code}
{-
    Reaction Networks -- Kappa Parser
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
{-# LANGUAGE TemplateHaskell #-}
module Krdf.Internal.KappaAST where
import Language.Haskell.TH.Syntax
import Data.List(intersperse, sort)
import Data.Text(Text, pack, unpack)
import Data.HashMap.Lazy(HashMap, toList)
\end{code}
}

\section{The Kappa Datatypes}

\subsection{Agent patterns}
The basic element of Kappa is the \emph{agent}. An agent has a name
and numerous sites. The sites likewise have names and also have
so-called \emph{internal states}. Sites can be \emph{bound} and
linked to sites on other agents, or \emph{unbound}. Expressions
in Kappa are not written in terms of concrete agents but in terms
of \emph{patterns} that may match occurrences of an agent.

An \emph{agent pattern} is a mapping
$N_a \rightarrow \left(N_{a,s} \rightarrow S_{a,s}\right)^x$
from the space of agent names, $N_a$ to a list of
mappings names for its sites, $N_{a,s}$ to the sites
themselves, $S_{a,s}$. In code this can be written as,
\begin{code}
data AgentP  = AgentP Text (HashMap Text SiteP)
             deriving (Eq)
\end{code}
where a hash-table or dictionary is used to implement the mapping
from state names to states.

This definition is sufficient to allow automatic derivation of an
equality check among patterns, but later we will also have use for an
ordering. Since the hash table does not have a defined ordering, we
must supply one,
\begin{code}
instance Ord AgentP where
  compare (AgentP name1 sites1) (AgentP name2 sites2) =
    compare (name1, toSortedList sites1) (name2, toSortedList sites2)
    where toSortedList = sort . toList
\end{code}
The instance is pretty straightforward, just convert the hash tables
to a list of $\left(\mathrm{name}, \mathrm{value}\right)$ tuples and
sort that as the basis for comparison.

The sites themselves have a link state and an internal state,
and neither, either or both may be specified. Furthermore a link
state can be said to be bound to something specific, or bound
to anything. These data type declarations,
\begin{code}
data LinkP   = Bound | Unbound | MaybeBound | Link Text
             deriving (Eq, Ord)
data StateP  = State Text | Undefined
             deriving (Eq, Ord)
\end{code}
set up the semantic mapping to the standard Kappa syntax as
shown in Figure~\ref{fig:corrstate}.
\begin{figure}[ht]
  \centering
  \begin{subfigure}[b]{0.4\textwidth}
    \begin{tabular}{rcl}
      \texttt{x} &$\corresponds$& \icode{Unbound}\\
      \texttt{x!\_} &$\corresponds$& \icode{Bound}\\
      \texttt{x?} &$\corresponds$& \icode{MaybeBound}\\
      \texttt{x!1} &$\corresponds$& \icode{Link "1"}
    \end{tabular}
    \caption{Link states.}
    \label{fig:corrlinkstate}
  \end{subfigure}\hspace{0.1\textwidth}
  \begin{subfigure}[b]{0.4\textwidth}
    \begin{tabular}{rcl}
      \texttt{x} &$\corresponds$& \icode{Undefined}\\
      \texttt{x\~{}foo} &$\corresponds$& \icode{State "foo"}
    \end{tabular}
    \caption{Internal states.}
    \label{fig:corrintstate}
  \end{subfigure}
  \caption{Correspondences to Kappa syntax}
  \label{fig:corrstate}
\end{figure}

With these definitions in hand, the definition of an agent pattern
is completed with,
\begin{code}
type SiteP   = (LinkP, StateP)
\end{code}
which simply says that a site is made up of a link state and an
internal state. With appropriate parsers and serialisers, this
structure is sufficient to hold Kappa expressions. A few examples
are given in Figure~\ref{fig:exampleexpr}.
\begin{figure}[h]
  \centering
  \begin{tabular}{rcl}
    \texttt{A()} &$\corresponds$& \icode{AgentP "A" (fromList [])}\\
    \texttt{A(x!\_, y\~{}1!2)} &$\corresponds$&
\begin{minipage}[t]{0.4\textwidth}
\begin{hcode}
AgentP "A" (fromList [
    ("x", (Bound, Undefined)),
    ("y", (Link "2", State "1"))
])
\end{hcode}
\end{minipage}\\
    \texttt{A(x\~{}1,y?)}& $\corresponds$&
\begin{minipage}[t]{0.4\textwidth}
\begin{hcode}
AgentP "A" (fromList [
    ("x", (Unbound, State "1")),
    ("y", (MaybeBound, Undefined))
])
\end{hcode}
\end{minipage}
  \end{tabular}
  \caption{Examples of Kappa expressions.}
  \label{fig:exampleexpr}
\end{figure}

\subsection{Agent declarations}
Kappa also has the concept of an \emph{agent declaration}. They provide
a list of sites and possible internal states for the sites. The utility
of this is twofold. First it allows for checking that agent patterns
given in the bodies of rules are consistent -- if we have a list of
possible states, we can check that the patterns do not try to reference
invalid states. Second it supports the concept of ``don't care, don't
write''. If a rule does not depend on an internal state for a particular
site, or even the bound or unbound state of a site, one doesn't
include that information in the pattern.

So, in the context of a declaration,
\verb|A(x~1~2~3,y~u~v~w)|,
which says that the agent \texttt{A} has two sites, \texttt{x} and
\texttt{y} which each have possible internal states in
$\left(\mathtt{1}, \mathtt{2}, \mathtt{3}\right)$ and
$\left(\mathtt{u}, \mathtt{v}, \mathtt{w}\right)$ respectively,
these patterns are all equivalent:
\par\vspace{\baselineskip}
\begin{center}
\begin{tabular}{ccc}
  \texttt{A()} & \texttt{A(x?)} & \texttt{A(x\~{}1?)}\\
  \texttt{A(x?,y?)} & \texttt{A(y?)} & \texttt{A(y\~{}1?)}\\
  \texttt{A(x?,y\~{}u?)} & \texttt{A(x\~{}1?,y?)} & \texttt{A(x\~{}1?,y\~{}u?)}
\end{tabular}
\end{center}
\par\vspace{\baselineskip}
\noindent Notice that the first internal state in the declaration
provides a default value. Otherwise it would be possible to simply
derive the declarations corresponding to the actual use of the
agent patterns in the rules.

An agent declaration then is a mapping,
$N_a \rightarrow \left(N_{a,s} \rightarrow N_{a,s,i}^y\right)^x$
where $N_{a,s,i}$ are the names of the internal states of
site $s$ of agent $a$. In code this corresponds to
\begin{code}
data AgentD = AgentD Text (HashMap Text [Text])
            deriving (Eq)
\end{code}
where again we use a hash table to look up sites and provide an
instance of \icode{Ord} almost identical to that for
\icode{AgentP}.
\hide{
\begin{code}
instance Ord AgentD where
  compare (AgentD name1 sites1) (AgentD name2 sites2) =
    compare (name1, toSortedList sites1) (name2, toSortedList sites2)
    where toSortedList = sort . toList
\end{code}
}

\subsection{Rules}

\begin{code}
data Rule    = Rule { lhs  :: ([AgentP], [TokE])
                    , rhs  :: ([AgentP], [TokE])
                    , rate :: Expr
                    , desc :: Text
                    }
             deriving (Show, Eq)
\end{code}

\begin{code}
data TokE    = Tok Text Expr
             deriving (Show, Eq, Ord)
\end{code}

\hide{
\begin{code}
defaultRule :: Rule
defaultRule = Rule { lhs = undefined
                   , rhs = undefined
                   , rate = Lit 1.0
                   , desc = pack "" }

data Expr =
  Var Text |
  Lit Double |
  Neg Expr |
  Abs Expr |
  Floor Expr |
  Exp Expr |
  Cos Expr |
  Sin Expr |
  Tan Expr |
  Log Expr |
  Min Expr Expr |
  Max Expr Expr |
  Plus Expr Expr |
  Times Expr Expr |
  Pow Expr Expr |
  Div Expr Expr |
  Mod Expr Expr
  deriving (Show, Eq, Ord)

data VarD = VarD Text Expr deriving (Show, Eq, Ord)
data TokD = TokD Text deriving (Show, Eq, Ord)
data Obs  = Obs Text AgentP deriving (Show, Eq, Ord)
data Init = Init Double AgentP deriving (Show, Eq, Ord)
data Statement =
  AD AgentD | VD VarD | TD TokD | RD Rule | OB Obs | IN Init | RDF Text
  deriving (Show, Eq)

instance Show AgentD where
  showsPrec _ (AgentD name sites) =
    showString (unpack name) . showString "(" . sitesp . showString ")"
    where
      sitesp = foldl (.) (showString "") $
               intersperse (showString ",") (map (showSite) (toList sites))
      showSite (sn, []) = showString (unpack sn)
      showSite (sn, ss) =
        showString (unpack sn) . showString "~" .
        (foldl (.) (showString "") $ itilde (map (showString . unpack ) ss))
      itilde ss = intersperse (showString "~") ss

instance Show AgentP where
  showsPrec n (AgentP name states) =
    showString (unpack name) . showString "(" . statesp . showString ")"
    where
      statesp = foldl (.) (showString "") $ icomma (map showState (toList states))
      showState (sn, (l, s)) =
        showString (unpack sn) . showsPrec n l . showsPrec n s
      icomma ss = intersperse (showString ",") ss

instance Show LinkP where
  showsPrec _ Bound = showString "!_"
  showsPrec _ MaybeBound = showString "?"
  showsPrec _ Unbound = \s -> s
  showsPrec _ (Link l) = showString "!" . showString (unpack l)

instance Show StateP where
  showsPrec _ Undefined  = \s -> s
  showsPrec _ (State s) = showString "~" . showString (unpack s)

packN :: Exp
packN = VarE $ mkName "Data.Text.pack"

unpackN :: Exp
unpackN = VarE $ mkName "Data.Text.unpack"

fromListN :: Exp
fromListN = VarE $ mkName "Data.HashMap.Lazy.fromList"

liftText :: Text -> Exp
liftText t = (AppE packN (LitE (StringL (unpack t))))

sitePN :: Exp
sitePN = ConE $ mkName "Krdf.Kappa.SiteP"

agentPN :: Exp
agentPN = ConE $ mkName "Krdf.Kappa.AgentP"

agentDN :: Exp
agentDN = ConE $ mkName "Krdf.Kappa.AgentD"

ruleDN :: Exp
ruleDN = ConE $ mkName "Krdf.Kappa.RD"

linkedN :: Exp
linkedN = ConE $ mkName "Krdf.Kappa.Link"

stateN :: Exp
stateN = ConE $ mkName "Krdf.Kappa.State"

varN :: Exp
varN = ConE $ mkName "Krdf.Kappa.Var"

litN :: Exp
litN = ConE $ mkName "Krdf.Kappa.Lit"

tokN :: Exp
tokN = ConE $ mkName "Krdf.Kappa.Tok"

siteN :: Exp
siteN = ConE $ mkName "Krdf.Kappa.Site"

ruleN :: Name
ruleN = mkName "Krdf.Kappa.Rule"

lhsN :: Name
lhsN  = mkName "Krdf.Kappa.lhs"

rhsN :: Name
rhsN  = mkName "Krdf.Kappa.rhs"

rateN :: Name
rateN = mkName "Krdf.Kappa.rate"

descN :: Name
descN = mkName "Krdf.Kappa.desc"

instance Lift AgentP where
  lift (AgentP name states) = do
    s <- mapM liftState (toList states)
    return (AppE
            (AppE agentPN (liftText name))
            (AppE fromListN (ListE s))
           )
    where
      liftState (k, (l, s)) = do
        ll <- lift l
        ss <- lift s
        return (TupE [liftText k, (TupE [ll, ss])])
        
instance Lift LinkP where
  lift Bound      = [| Bound |]
  lift Unbound    = [| Unbound |]
  lift MaybeBound = [| MaybeBound |]
  lift (Link s) = return (AppE linkedN (liftText s))

instance Lift StateP where
  lift (State s) = return (AppE stateN (liftText s))
  lift Undefined  = [| Undefined |]

instance Lift Statement where
  lift (AD a)  = [| AD a |]
  lift (RD rule) = do
    s <- lift rule
    return (AppE ruleDN s)
  lift _ = undefined

instance Lift AgentD where
  lift (AgentD name sites) = do
    return (AppE
            (AppE agentDN (liftText name))
            (AppE fromListN (ListE (map liftSite (toList sites)))))
    where
      liftSite (k, ss) =
        (TupE [liftText k, ListE (map liftText ss)])

instance Lift Rule where
  lift (Rule { lhs=lh, rhs=rh, rate=r, desc=d }) = do
    llh <- lift lh
    lrh <- lift rh
    lr  <- lift r
    return (RecConE ruleN [ (lhsN, llh)
                          , (rhsN, lrh)
                          , (rateN, lr)
                          , (descN, liftText d) ])

instance Lift TokE where
  lift (Tok name expr) = do
    e <- lift expr
    return (AppE (AppE tokN (liftText name)) e)


instance Lift Expr where
  lift (Var name) = return e
    where e = (AppE varN (liftText name))
  lift (Lit d) = return e
    where e = (AppE litN (LitE (RationalL (toRational d))))
  lift (Neg e) = [| Neg e |]
  lift (Abs e) = [| Abs e |]
  lift (Floor e) = [| Floor e |]
  lift (Exp e) = [| Exp e |]
  lift (Cos e) = [| Cos e |]
  lift (Sin e) = [| Sin e |]
  lift (Tan e) = [| Tan e |]
  lift (Log e) = [| Log e |]
  lift (Min e1 e2) = [| Min e1 e2 |]
  lift (Max e1 e2) = [| Max e1 e2 |]
  lift (Plus e1 e2) = [| Plus e1 e2 |]
  lift (Times e1 e2) = [| Times e1 e2 |]
  lift (Pow e1 e2) = [| Pow e1 e2 |]
  lift (Div e1 e2) = [| Div e1 e2 |]
  lift (Mod e1 e2) = [| Mod e1 e2 |]

\end{code}
}

% Local Variables:
% compile-command: "cd ../..; cabal build; cabal test"
% End:
