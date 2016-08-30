\hide{
\begin{code}
{-# LANGUAGE QuasiQuotes #-}
module Test.KappaT (kappaTests) where

import Krdf.Kappa
import Data.Attoparsec.Text(parseOnly)
import qualified Data.HashMap.Lazy
import qualified Data.HashMap.Lazy as H
import qualified Data.List as L
import Data.Text(pack)
import Test.HUnit hiding (State)
\end{code}
}
\begin{code}
kappaTests :: Test
kappaTests = "Kappa" ~: TestList [
  "Parser" ~: TestList testParseKappa
  , "RuleQQ" ~: TestList testRuleQQ
  , "EnumState" ~: TestList testEnumState
  , "NormAgent" ~: TestList testNormAgent
  , "CheckAgent" ~: TestList testCheckAgent
  , "Consistency" ~: TestList testConsistency
  , "Links" ~: TestList testLinks
  ]

testParseKappa :: [Test]
testParseKappa = map (\(s,l) -> l ~=? parse s) $ k
  where k =
          [ ("%agent: A(z,y~u~p,x~0~1~2)",
             [AD $ AgentD "A" (H.fromList [ ("x", ["0", "1", "2"])
                                   , ("y", ["u", "p"])
                                   , ("z", [])
                                   ])
             ])
          , ("%agent: A()", [AD $ AgentD "A" (H.fromList [])])
          , ("%token: atp\n%token: adp",
             [TD $ TokD "atp", TD $ TokD "adp"])
          , ("%var: 'V' 1\n%var: V 2e3",
             [VD $ VarD "V" (Lit 1.0), VD $ VarD "V" (Lit 2000.0)])
          , ("%var: v [exp](-u)",
             [VD $ VarD "v" (Exp (Neg (Var "u")))])
          , ("%var: v [max]([cos](u), [abs](-w))",
             [VD $ VarD "v" (Max (Cos (Var "u")) (Abs (Neg (Var "w"))))])
          , ("%var: 'k11' 'k10'*6",
             [VD $ VarD "k11" (Times (Var "k10") (Lit 6.0))])
          , ("'my rule' A(x!2), B(y) -> B(y~0) @1",
             [RD Rule {
                 lhs = ([ AgentP "A" (H.fromList [("x", ((Link "2"), Undefined))])
                        , AgentP "B" (H.fromList [("y", (Unbound, Undefined))])
                        ], []),
                 rhs = ([AgentP "B" (H.fromList [("y", (Unbound, (State "0")))])
                        ], []),
                 rate = Lit 1.0, rateC = Lit 0.0,
                 desc = pack "my rule"
             } ])
          , ("'bi rule' A(x!_) <-> B(y) @k_1, 'k_2'",
             [ RD Rule {
                  lhs = ([ AgentP "A" (H.fromList [("x", (Bound, Undefined))])], []),
                  rhs = ([ AgentP "B" (H.fromList [("y", (Unbound, Undefined))])], []),
                  rate = Var "k_1", rateC = Lit 0.0,
                  desc = pack "bi rule"
                  }
             , RD Rule {
                  lhs = ([ AgentP "B" (H.fromList [("y", (Unbound, Undefined))])], []),
                  rhs = ([ AgentP "A" (H.fromList [("x", (Bound, Undefined))])], []),
                  rate = Var "k_2", rateC = Lit 0.0,
                  desc = pack "bi rule"
                  }
             ])
          , ("'hybrid rule' S(x!1~u),K(y!1) -> S(x~p),K(y) @k",
             [ RD Rule {
                  lhs = ([ AgentP "S" (H.fromList [("x", ((Link "1"), (State "u")))])
                         , AgentP "K" (H.fromList [("y", ((Link "1"), Undefined))])], []),
                  rhs = ([ AgentP "S" (H.fromList [("x", (Unbound, (State "p")))])
                         , AgentP "K" (H.fromList [("y", (Unbound, Undefined))])], []),
                  rate = Var "k", rateC = Lit 0.0,
                  desc = pack "hybrid rule"
                  }
             ])
          , ("'circuit rule' S(x!1~u),K(y!1) -> S(x~p),K(y) @ 'k1' ('k2')",
             [ RD Rule {
                  lhs = ([ AgentP "S" (H.fromList [("x", ((Link "1"), (State "u")))])
                         , AgentP "K" (H.fromList [("y", ((Link "1"), Undefined))])], []),
                  rhs = ([ AgentP "S" (H.fromList [("x", (Unbound, (State "p")))])
                         , AgentP "K" (H.fromList [("y", (Unbound, Undefined))])], []),
                  rate = Var "k1", rateC = Var "k2",
                  desc = pack "circuit rule"
                  }
             ]
            )
          , ("'hybrid rule' S(x~u!1),K(y!1) | 0.1:atp -> S(x~p),K(y) | 0.1:adp @ 'k'",
             [ RD Rule {
                  lhs = ([ AgentP "S" (H.fromList [("x", ((Link "1"), (State "u")))])
                         , AgentP "K" (H.fromList [("y", ((Link "1"), Undefined))])
                         ], [Tok "atp" (Lit 0.1)]),
                  rhs = ([ AgentP "S" (H.fromList [("x", (Unbound, (State "p")))])
                         , AgentP "K" (H.fromList [("y", (Unbound, Undefined))])
                         ], [Tok "adp" (Lit 0.1)]),
                  rate = Var "k", rateC = Lit 0.0,
                  desc = pack "hybrid rule"
                  }
             ])
          , ("%obs: 'FreeProm'  A(s1,s2)",
             [ OB $ Obs "FreeProm" (AgentP "A" (H.fromList [("s1", (Unbound, Undefined))
                                                           , ("s2", (Unbound, Undefined))]))
             ])
          , ("%init: 500 KinA()",
             [ IN $ Init 500.0 [AgentP "KinA" (H.fromList [])] ])
          ]
        parse s = case parseOnly kappaParser (pack s) of
          Right r  -> r
          Left err -> error $ err

testRuleQQ :: [Test]
testRuleQQ = [l ~=? s | (l,s) <- t]
  where t = [ ([rule| 'homodimer' A(x), A(x) -> A(x!1), A(x!1) @1.0 |],
               [ Rule {
                  lhs = ([ AgentP "A" (H.fromList [("x", (Unbound, Undefined))])
                         , AgentP "A" (H.fromList [("x", (Unbound, Undefined))])], []),
                  rhs = ([ AgentP "A" (H.fromList [("x", (Link "1", Undefined))])
                         , AgentP "A" (H.fromList [("x", (Link "1", Undefined))])], []),
                  rate = Lit 1.0, rateC = Lit 0.0,
                  desc = pack "homodimer"
                 } ])
            ]

testEnumState :: [Test]
testEnumState = [l ~=? s | (l,s) <- k]
  where k = [
          (deriveDec [complex| A(x!1), A(x!1~2,y~p), A(x~1), B(u,v,w~1) |],
           [[agent| A(x~1~2,y) |], [agent| B(w,u,v) |]])
          ]

testNormAgent :: [Test]
testNormAgent = [
  map (norma decs) [complex| A() |] ~=? [complex| A(x~0?,y?) |]
  , map (norma decs) [complex| A(x~1) |] ~=? [complex| A(x~1,y?) |]
  , map (norma decs) [complex| A(x!a) |] ~=? [complex| A(x~0!a,y?) |]
  , map (norma decs) [complex| A(x~1!a) |] ~=? [complex| A(x~1!a,y?) |]
  , L.sort (H.elems decs) ~=? L.sort (deriveDec [complex| A(x!_), A(x~0,y), A(x~1) |])
  , map (norma decs) [complex| A(x!a) |] ~=?
    (map (norma decs) . map (norma decs)) [complex| A(x!a) |]
  ]
  where decs = decmap [[agent| A(x~0~1,y) |]]

testCheckAgent :: [Test]
testCheckAgent = [
  chk [complex| B() |] ~=? Nothing
  , chk [complex| A() |] ~=? Just []
  , chk [complex| A(z) |] ~=? Just [("z", Nothing)]
  , chk [complex| A(x) |] ~=? Just [("x", Just True)]
  , chk [complex| A(x~0) |] ~=? Just [("x", Just True)]
  , chk [complex| A(x~2) |] ~=? Just [("x", Just False)]
  , chk [complex| A(x,y) |] ~=? Just [("x", Just True),
                                      ("y", Just True)]
  , chk [complex| A(x,y~0) |] ~=? Just [("x", Just True),
                                        ("y", Just False)]
  ]
  where
    chk  = checka decs . head
    decs = decmap [[agent| A(x~0~1,y) |]]

testConsistency :: [Test]
testConsistency = [
  chk [complex| B() |] ~=? False
  , chk [complex| A() |] ~=? True
  , chk [complex| A(z) |] ~=? False
  , chk [complex| A(x) |] ~=? True
  , chk [complex| A(x~0) |] ~=? True
  , chk [complex| A(x~2) |] ~=? False
  , chk [complex| A(x,y) |] ~=? True
  , chk [complex| A(x,y~0) |] ~=? False
  ]
  where
    chk = consistent decs . head
    decs = decmap [[agent| A(x~0~1,y) |]]

testLinks :: [Test]
testLinks = [
  f [complex| A(x!1), B(x!1) |] ~=? ["1"]
  , f [complex| A(x), B(x!1), C(y!2), D(x!2,y!1) |] ~=? ["1", "2"]
  , collect [complex| A(x!1), B(y!1) |] ~=? [[complex|  A(x!1), B(y!1) |]]
  , collect [complex| A(x!1), B(y!1), C(z!2) |] ~=?
    [ [complex| A(x!1), B(y!1) |]
    , [complex| C(z!2) |]
    ]
  , collect [complex| A(x!1), B(y!2), C(z!1) |] ~=?
    [ [complex| A(x!1), C(z!1) |]
    , [complex| B(y!2) |]
    ]
  , collect [complex| A(x!1), B(y!2), C(z!1), D(u!2) |] ~=?
    [ [complex| A(x!1), C(z!1) |]
    , [complex| B(y!2), D(u!2) |]
    ]
  ]
  where f = L.nub . concat . map links 
\end{code}
%% Local Variables:
%% compile-command: "cd ..; stack test"
%% End:
