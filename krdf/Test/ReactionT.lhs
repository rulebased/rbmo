\hide{
\begin{code}
{-# LANGUAGE QuasiQuotes #-}
module Test.ReactionT (reactionTests) where

import Krdf.Reaction
import Test.HUnit
\end{code}
}
\begin{code}
reactionTests :: Test
reactionTests = "Reaction" ~: TestList [
  "QQ" ~: TestList testQuasiQuotes
  ]

data Reactant = A | B | C | D deriving (Eq, Enum, Ord, Show)
k :: Double
k = 1

testQuasiQuotes :: [Test]
testQuasiQuotes = map (\(l,q) -> l ~=? q) $ zip literal quoted
  where literal =
          [ Reaction { lhs  = fromOccurList [(A,1), (B,2)],
                       rhs  = fromOccurList [(C,3), (D,1)],
                       rate = (2::Double) }
          , Reaction { lhs  = fromOccurList [(B,2), (C,1)],
                       rhs  = fromOccurList [(A,1)],
                       rate = k }
          ]
        quoted =
          [reactions|
            1A + 2B -> 3C + D @2
          , 2B + C -> A @k
          |]
\end{code}
%% Local Variables:
%% compile-command: "cd ..; cabal build && cabal test"
%% End:
