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
module Krdf.Internal.KappaParser where

import Prelude hiding (takeWhile)

import Control.Applicative ((<|>), many)
import Data.Attoparsec.Text
import Data.HashMap.Lazy(fromList)
import Data.Text(Text, pack, cons)
import Krdf.Internal.KappaAST
\end{code}
}

\begin{code}
-- | Utility parser -- parse a token/name
tok :: Parser Text
tok = do
  first <- satisfy $ inClass "A-Za-z"
  rest  <- takeWhile (inClass "A-Za-z0-9_+-")
  return $ cons first rest

-- | Utility parser -- parse a single-quoted token/name
qtok :: Parser Text
qtok = quoted <|> tok
  where quoted = do
          _ <- char '\''
          t <- tok
          _ <- char '\''
          return t

-- | Utility parser -- parse a state token
stok :: Parser Text
stok = takeWhile (inClass "A-Za-z0-9_+-")

-- | Utility parser -- parse a single-quoted string
qstr :: Parser Text
qstr = do
  _ <- char '\''
  s <- manyTill anyChar (char '\'')
  return (pack s)

-- | Utility parser -- eat a comment until end of line
comment :: Parser ()
comment = do
  _ <- char '#'
  _ <- endOfLine <|> (notChar '^' >> manyTill anyChar endOfLine >> return ())
  return ()

-- | Utility parser -- eat up until end of line
eol :: Parser String
eol = manyTill space $ comment <|> endOfLine

-- | Parse an Agent signature
agent_dec :: Parser [Statement]
agent_dec = do
  _ <- (string $ pack "%agent:") >> many space
  agent <- agent_sig
  return [AD agent]

agent_sig :: Parser AgentD
agent_sig = do
  name  <- tok
  _     <- char '('
  sites <- site_sig `sepBy` (char ',' >> many space)
  _     <- char ')'
  return $ AgentD name (fromList sites)

-- | Parse a site+state signature. Used by @agent_dec@
site_sig :: Parser (Text, [Text])
site_sig = isl <|> bare
  where bare = do
          name <- tok
          return (name, [])
        isl = do
          name <- tok
          _    <- char '~'
          isls <- stok `sepBy1` (char '~')
          return (name, isls)

-- | Parse a token declaration.
tok_dec :: Parser [Statement]
tok_dec = do
  _    <- string $ pack "%token:"
  _    <- many space
  name <- qtok
  return [TD $ TokD name]

-- | Parse a variable declaration
var_dec :: Parser [Statement]
var_dec = do
  _ <- (string $ pack "%var:") >> many space
  n <- qtok
  _ <- many space
  v <- expr
  return [VD $ VarD n v]

e_var :: Parser Expr
e_var = do
  n <- qtok
  return $ Var n

e_lit :: Parser Expr
e_lit = do
  n <- double
  return $ Lit n

e_bracket :: Parser Expr
e_bracket = do
  _ <- char '('
  e <- expr
  _ <- char ')'
  return e

e_neg :: Parser Expr
e_neg = do
  _ <- char '-'
  e <- expr
  return $ Neg e

e_op :: (Expr -> Expr) -> String -> Parser Expr
e_op m s = do
  _ <- string (pack s)
  e <- expr
  return $ m e

e_binop :: (Expr -> Expr -> Expr) -> String -> Parser Expr
e_binop m s = do
  _ <- string (pack s)
  _ <- char '(' >> many space
  e1 <- expr
  _ <- many space >> char ',' >> many space
  e2 <- expr
  _ <- many space >> char ')'
  return $ m e1 e2

e_infix :: (Expr -> Expr -> Expr) -> String -> Parser (Expr -> Expr -> Expr, Expr)
e_infix m s = do
  _ <- many space >> string (pack s) >> many space
  e2 <- expr
  return (m, e2)

e_minus :: Parser (Expr -> Expr -> Expr, Expr)
e_minus = do
  _ <- many space >> char '-' >> many space
  e2 <- expr
  return (Plus, (Neg e2))
  
-- | Parse an algebraic expression
expr :: Parser Expr
expr = do
  e1 <- e_bracket <|>
        e_var <|>
        e_lit <|>
        e_neg <|>
        e_op Abs "[abs]" <|>
        e_op Floor "[int]" <|>
        e_op Exp "[exp]" <|>
        e_op Cos "[cos]" <|>
        e_op Sin "[sin]" <|>
        e_op Tan "[tan]" <|>
        e_op Log "[log]" <|>
        e_binop Min "[min]" <|>
        e_binop Max "[max]"
  inf <- (Right <$> expr_infix) <|> (Left <$> return ())
  case inf of
   Right (m, e2) -> return $ m e1 e2
   Left  _       -> return e1

expr_infix :: Parser (Expr -> Expr -> Expr, Expr)
expr_infix =
  e_infix Plus "+" <|>
  e_minus <|>
  e_infix Times "*" <|>
  e_infix Pow "^" <|>
  e_infix Div "/" <|>
  e_infix Mod "[mod]"

-- | Parse an expression surrounded by round brackets
bkt_expr :: Parser Expr
bkt_expr = do
  _ <- char '('
  _ <- many space
  e <- expr
  _ <- many space
  _ <- char ')'
  return e

-- | Parse a rule
rule_pat :: Parser [Rule]
rule_pat = do
  d  <- qstr <|> tok
  _  <- many1 space
  rs <- circ_rule <|> bi_rule <|> pure_rule
  return $ map (\r -> r { desc = d }) rs

rule_dec :: Parser [Statement]
rule_dec = do
  rs <- rule_pat
  return $ fmap RD rs

-- | Parse a link pattern: ? | !n | !_ | empty
link_pat :: Parser LinkP
link_pat = bound <|> linked <|> mbound <|> unbound
  where linked = do
          _ <- char '!'
          l <- stok
          return $ Link l
        bound = string (pack "!_") >> (return Bound)
        mbound = char '?' >> (return MaybeBound)
        unbound = return Unbound

-- | Parse a state pattern: name (link_pat)? (~(internal_state)*)
-- | There is a bug in the spec here. The grammar says the link state
-- | and internal state come in the order as above. But the prose gives
-- | an example with S(x~u!1), or in other words the internal state
-- | first. Here we accept both.
site_pat :: Parser (Text, (LinkP, StateP))
site_pat = statefirst <|> linkfirst <|> justlink
  where st = do
          _ <- char '~'
          stok
        linkfirst = do
          name <- tok
          link <- link_pat
          state <- st
          return (name, (link, State state))
        statefirst = do
          name <- tok
          state <- st
          link <- link_pat
          return (name, (link, State state))
        justlink = do
          name <- tok
          link <- link_pat
          return (name, (link, Undefined))

-- | Parse an agent expression (pattern) as it exists in a rule
agent_pat :: Parser AgentP
agent_pat = do
  name   <- tok
  _      <- many space >> char '('
  states <- site_pat `sepBy` (many space >> char ',' >> many space)
  _      <- char ')'
  return $ AgentP name (fromList states)

tok_expr :: Parser TokE
tok_expr = do
  e <- expr
  _ <- many space >> char ':' >> many space
  t <- qtok
  return $ Tok t e
  
tok_exprs :: Parser [TokE]
tok_exprs = do
  _ <- many space >> char '|' >> many space
  toks <- tok_expr `sepBy` (many space >> char ',' >> many space)
  return toks
  
-- | Parse a forward rule (->)
pure_rule :: Parser [Rule]
pure_rule = do
  l  <- agent_pat `sepBy` (many space >> char ',' >> many space)
  lt <- tok_exprs <|> return []
  _  <- many space >> string (pack "->") >> many space
  r  <- agent_pat `sepBy` (many space >> char ',' >> many space)
  rt <- tok_exprs <|> return []
  _  <- many space >> char '@' >> many space
  k  <- expr
  return [ defaultRule { lhs = (l, lt), rhs = (r, rt), rate = k }]

-- | Parse a bidirectional rule (<->)
bi_rule :: Parser [Rule]
bi_rule = do
  l <- agent_pat `sepBy` (many space >> char ',' >> many space)
  lt <- tok_exprs <|> return []
  _ <- many space >> string (pack "<->") >> many space
  r <- agent_pat `sepBy` (many space >> char ',' >> many space)
  rt <- tok_exprs <|> return []
  _ <- many space >> char '@' >> many space
  kf <- expr
  _ <- many space >> char ',' >> many space
  kr <- expr <|> bkt_expr
  return [ defaultRule { lhs = (l, lt), rhs = (r, rt), rate = kf }
         , defaultRule { lhs = (r, rt), rhs = (l, lt), rate = kr }
         ]

-- | Parse a circuit rule (two rates, one enclosed in round brackets)
circ_rule :: Parser [Rule]
circ_rule = do
  l  <- agent_pat `sepBy` (many space >> char ',' >> many space)
  lt <- tok_exprs <|> return []
  _  <- many space >> string (pack "->") >> many space
  r  <- agent_pat `sepBy` (many space >> char ',' >> many space)
  rt <- tok_exprs <|> return []
  _  <- many space >> char '@' >> many space
  k  <- expr
  _  <- many space >> char '(' >> many space
  kc <- expr
  _  <- many space >> char ')'
  return [ defaultRule { lhs = (l, lt), rhs = (r, rt), rate = k, rateC = kc }]

-- | Parse an observation declaration
obs_dec :: Parser [Statement]
obs_dec = do
  _ <- (string $ pack "%obs:") >> many space
  n <- qtok
  _ <- many space
  a <- agent_pat
  return [OB $ Obs n a]

-- | Initialisation
init_dec :: Parser [Statement]
init_dec = do
  _ <- (string $ pack "%init:") >> many space
  n <- double
  _ <- many space
  a <- agent_pat `sepBy` (char ',' >> many space)
  return [IN $ Init n a]

-- | Parse an RDF line
rdf_line :: Parser [Statement]
rdf_line = do
  _   <- (string $ pack "#^ ")
  rdf <- takeWhile (\c -> c /= '\n')
  return [RDF rdf]

-- | A statement is a declaration of some sort or a rule
statement :: Parser [Statement]
statement =  rdf_line <|> agent_dec <|> var_dec <|> tok_dec <|> rule_dec <|> obs_dec <|> init_dec

-- | Parse a list of statements, stripping out comments and blank lines
kappaParser :: Parser [Statement]
kappaParser = do
  _  <- many eol
  rs <- statement `sepBy` many eol
  _  <- many eol
  _  <- endOfInput
  return (concat rs)

parseKappa :: Text -> [Statement]
parseKappa text =
  case parseOnly kappaParser text of
   Right r  -> r
   Left err -> error err
\end{code}

% Local Variables:
% compile-command: "cd ../..; cabal build; cabal test"
% End:
