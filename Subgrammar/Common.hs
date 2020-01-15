module Subgrammar.Common where

import PGF
-- import qualified GF.Grammar.Canonical
import qualified GF
import GF.Support
import System.FilePath
import System.Directory
import Canonical
import System.FilePath((</>),(<.>))

type Example = String
type Forest = [Tree]
data Grammar = Grammar { pgf :: PGF, concs :: [FilePath]} -- The PGF and file pathes to all concrete syntaxes
data Formula = Variable String | Conj [Formula] | Disj [Formula] | Neg Formula | Imp Formula Formula deriving (Show)
type Solution = (Double,[String])

-- Given a grammar translate an example into a set of syntax trees
examplesToForests :: Grammar -> Language -> [Example] -> [Forest]
examplesToForests grammar language examples =
  [parse (pgf grammar) language (startCat $ pgf grammar) example | example <- examples]

-- helper to convert a tree to a list of rules
flatten :: Tree -> [String]
flatten tree = maybe [] (\(f,ts) -> (showCId f):(concatMap flatten ts)) $ unApp tree

generateGrammar :: Grammar -> Solution -> IO Grammar
generateGrammar grammar solution =
  do
    -- read old concrete syntax
    let options = modifyFlags (\f -> f { optLibraryPath = [
                                           "."
                                         , "/home/herb/src/foreign/gf/gf-rgl/src"
                                         , "/home/herb/src/foreign/gf/gf-rgl/src/abstract"
                                         , "/home/herb/src/foreign/gf/gf-rgl/src/english"
                                         , "/home/herb/src/foreign/gf/gf-rgl/src/common"
                                         , "/home/herb/src/foreign/gf/gf-rgl/src/prelude"
                                         ]})
    (utc,(concname,gfgram)) <- GF.batchCompile options $ concs grammar
    let absname = GF.srcAbsName gfgram concname
        canon = GF.grammar2canonical noOptions absname gfgram
        -- filter the grammar
        canon' = filterGrammar (snd solution) canon
        -- rename the grammar
        canon'' = renameGrammar (getAbsName canon ++ "Sub") canon'
        concs' = getConcNames canon''
    -- write new concrete syntax
    outdir <- fst <$> splitFileName <$> (canonicalizePath $ head $ concs grammar)
    let outdir' = outdir </> "sub"
    writeGrammar outdir' canon''
    -- compile and load new pgf
    pgf' <- GF.compileToPGF options [outdir' </> c <.> "gf" | c <- concs']
    let options' = modifyFlags (\f -> f { optOutputDir = Just outdir' })
    GF.writePGF options' pgf'
    return $ Grammar pgf' concs'
