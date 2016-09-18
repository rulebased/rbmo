module Main where

import Prelude hiding(readFile)
import Data.Text(Text(..), pack)
import Data.Text.IO(readFile, hPutStr)
import Krdf.Kappa
import Krdf.Kappa.Rdf
import Krdf.Utils(basename, mkQuery)
import Options.Applicative
import System.IO(stdout)
import Swish.RDF(RDFGraph, emptyRDFGraph, getNamespaces, setNamespaces)
import Swish.RDF.Query
import Swish.VarBinding (VarBinding(vbMap))

import qualified Swish.RDF.Formatter.Turtle as Turtle
import qualified Swish.RDF.Formatter.NTriples as NTriples
import qualified Swish.RDF.Formatter.N3 as N3

data Config = Cfg { filename  :: String
                  , templates :: String
                  }

args :: Parser Config
args = Cfg
       <$> strOption
       ( short 'f'
         <> long "filename"
         <> metavar "FILENAME"
         <> help "Kappa file to read" )
       <*> strOption
       ( short 't'
         <> long "templates"
         <> metavar "TEMPLATES"
         <> help "Template directory")

readTemplate :: Config -> String -> IO Text
readTemplate cfg url = readTemplateFromFile (templates cfg) url

readTemplateFromFile :: String -> String -> IO Text
readTemplateFromFile directory url =
  readFile $ directory ++ "/" ++ filename
  where filename = basename url

queryParts g = rdfQueryFind query g
  where

    query  = mkQuery [ "@prefix rbmc: <http://purl.org/rbm/rbmc/>."
                     , "@prefix rbmo: <http://purl.org/rbm/rbmo/>."
                     , "?model a rbmo:Model; rbmc:parts ?parts."
                     ]

readParts :: Config -> IO ()
readParts cfg = do
  input <- readFile $ filename cfg
  let kappa = parseKappa input
  let meta  = annotations kappa
  let parts = queryParts meta
  hPutStr stdout $ pack $ show parts

main :: IO ()
main = do
  execParser opts >>= readParts
  where
    opts = info (helper <*> args)
      ( fullDesc
     <> progDesc "Assemble a Kappa Biological Part"
     <> header "Kappa Part Assembler" )

-- Local Variables:
-- compile-command: "stack build"
-- End:

