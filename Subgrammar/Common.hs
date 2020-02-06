module Subgrammar.Common where

import PGF
import qualified GF
import GF.Support
import System.FilePath
import System.Directory
import Canonical
import System.FilePath((</>),(<.>))
import Data.LinearProgram
import qualified Data.Map.Lazy as Map

import System.Clock

-- | Examples are strings
type Example = String

-- | Forests are lists of trees
type Forest = [Tree]

-- | A grammar is a combination of a pgf file and the pathes to the concrete syntaxes
data Grammar = Grammar { pgf :: PGF, concs :: [FilePath]} -- The PGF and file pathes to all concrete syntaxes

-- | A solution is the score of the optimal solution together with the list of included rules
type Solution = (Double,[String])

-- | An objective function is a combination of a function and a direction
data ObjectiveFunction a = OF { fun :: [(String,[(String,a)])] -> ObjectiveFunc String Int, direction :: Direction }

-- | A problem contains trees, rules and a linear programming logical formula
type Problem = LP String Int -- Problem { trees :: [(String,[String])], rules :: [String] , formula :: LPM String Int ()}

-- Constants -> Have to be updated
path_to_exemplum :: String
path_to_exemplum = "../mulle-grammars/exemplum"
rgl_path :: String
rgl_path = "../gf-rgl/src"
rgl_subdirs :: String
rgl_subdirs = "abstract common prelude english"

-- | Objective function counting the number of trees
numTrees :: ObjectiveFunction a
numTrees = OF numTreesOF Min
  where
    numTreesOF :: [(String,[(String,a)])] -> ObjectiveFunc String Int
    numTreesOF tags = linCombination [(1,t) | (_,ts) <- tags,(t,_) <- ts]

-- | Solves a problem using a given objective function
solve :: Problem ->  IO Solution
solve problem =
  do
    -- Uses the MIP solver to get real binary variables, the simplex solver can return numbers between 0 and 1
    (_,solution) <- glpSolveVars mipDefaults problem
    return $ maybe (-1,[]) (\(val,vars) -> (val,[var | (var,vval) <- Map.toList vars,vval > 0])) solution

-- | Given a grammar translate an example into a set of syntax trees
examplesToForests :: Grammar -> Language -> [Example] -> [Forest]
examplesToForests grammar language examples =
  [parse (pgf grammar) language (startCat $ pgf grammar) example | example <- examples]


-- | Function to create a new grammar from an old grammar and a solution
generateGrammar :: Grammar -> Solution -> IO Grammar
generateGrammar grammar solution =
  do
    let lib_path = ".":rgl_path:[rgl_path</>subdir | subdir <- words rgl_subdirs] :: [FilePath]
        options = modifyFlags (\f -> f { optLibraryPath = lib_path })    
        -- read old concrete syntax
    canon <- loadCanonicalGrammar lib_path $ concs grammar
    let
        -- filter the grammar
        canon' = filterGrammar (snd solution) [] canon
        -- rename the grammar
        canon'' = renameGrammar (getAbsName canon ++ "Sub") canon'
        concs' = getConcNames canon''
    -- write new concrete syntax
    outdir <- fst <$> splitFileName <$> (canonicalizePath $ head $ concs grammar)
    let outdir' = outdir </> "subgrammar"
    createDirectoryIfMissing True outdir'
    writeGrammar outdir' canon''
    -- compile and load new pgf
    pgf' <- GF.compileToPGF options [outdir' </> c <.> "gf" | c <- concs']
    let options' = modifyFlags (\f -> f { optOutputDir = Just outdir' })
    GF.writePGF options' pgf'
    return $ Grammar pgf' concs'

-- | Helper function to time computations
time :: IO () -> IO Integer
time f =
  do
    putStrLn ">Timer> Start"
    t1 <- getTime ProcessCPUTime
    f
    t2 <- getTime ProcessCPUTime
    putStrLn ">Timer> Stop"
    let diff = fromIntegral (sec $ diffTimeSpec t1 t2)
    putStrLn $ ">Timer> Difference " ++ (show diff)
    return diff
    
