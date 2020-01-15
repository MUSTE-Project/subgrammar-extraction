module Subgrammar.Common where

import PGF
-- import qualified GF.Grammar.Canonical
import qualified GF
import GF.Support
import System.FilePath
import System.Directory
import Canonical

type Example = String
type Forest = [Tree]
data Grammar = Grammar { pgf :: PGF, concs :: [FilePath]} -- The PGF and file pathes to all concrete syntaxes
data Formula = Variable String | Conj [Formula] | Disj [Formula] | Neg Formula | Imp Formula Formula deriving (Show)
type Solution = (Double,[String])

-- Given a grammar translate an example into a set of syntax trees
examplesToForest :: Grammar -> Language -> [Example] -> [Forest]
examplesToForest grammar language examples =
  [parse (pgf grammar) language (startCat $ pgf grammar) example | example <- examples]

-- helper to convert a tree to a list of rules
flatten :: Tree -> [String]
flatten tree = maybe [] (\(f,ts) -> (show $ showCId f):(concatMap flatten ts)) $ unApp tree

generateGrammar :: Grammar -> Solution -> IO Grammar
generateGrammar grammar solution =
  do
    -- read old concrete syntax
    (utc,(concname,gfgram)) <- GF.batchCompile noOptions $ concs grammar
    let absname = GF.srcAbsName gfgram concname
        canon = GF.grammar2canonical noOptions absname gfgram
        -- filter the grammar
        canon' = filterGrammar (snd solution) canon
        -- rename the grammar
        canon'' = renameGrammar (getAbsName canon ++ "Sub") canon'
        concs' = getConcNames canon''
  -- write new concrete syntax
    outdir <- fst <$> splitFileName <$> (canonicalizePath $ head $ concs grammar)
    writeGrammar outdir canon''
  -- compile new pgf
--    GF.compileToPGF 
  -- load new pgf
    grammar' <- readPGF undefined
    return $ Grammar grammar' concs'
