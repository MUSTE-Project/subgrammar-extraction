module Subgrammar.CLI where

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Data.Char (isDigit)
import Data.Maybe (fromJust, isNothing)
import Data.List (partition, intersperse)
import Control.Applicative (pure, (<*>))
import Options (Options(..), runCommand, simpleOption)

import Subgrammar.Common (solve, examplesToForests,
                          ObjectiveFunction, numTrees,
                          Grammar(..), problemConstraints, generateGrammar)
import Subgrammar.GFSubtree (forestsToProblem, numRules, numRulesTrees, weightedRules)
import PGF (readPGF, readLanguage)

data Opts = Opts
  { oPgf :: FilePath
  , oLang :: String
  , oSize :: Int
  , oMerged :: Int
  , oObjfun :: String
  , oDebug :: Bool
  }

instance Options Opts where
  defineOptions = pure Opts
    <*> simpleOption "pgf"    ""      "File path to resource grammar PGF."
    <*> simpleOption "lang"   ""      "Name of concrete resource grammar."
    <*> simpleOption "size"   1       "Max size of subtrees."
    <*> simpleOption "merged" 0       "Max nr of merged rules in a tree."
    <*> simpleOption "ofun"   defofun ("Objective function (" ++ objfuns ++ ").")
    <*> simpleOption "debug"  False   "Turn on debugging."
    where defofun = fst (head objectiveFunctions)
          objfuns = concat $ intersperse "; " $ map fst objectiveFunctions


usageString :: String
usageString = ("Usage: subgrammar-extraction --pgf [path-to.pgf] --lang [concrete-grammar] (--help ...)  examples...\n" ++
               "(prepend negative examples with #, e.g. '# a b c')\n")


objectiveFunctions :: [(String, ObjectiveFunction [(String, [String])])]
objectiveFunctions = [("numRules",numRules), ("numTrees",numTrees), ("numRulesTrees",numRulesTrees), ("weightedRules",weightedRules)]


cliMain :: IO ()
cliMain = runCommand $ \opts examples ->
  if ( oPgf  opts == "" ||
       oLang opts == "" ||
       examples   == [] ||
       isNothing (lookup (oObjfun opts) objectiveFunctions)
     ) then
    do hPutStrLn stderr usageString
       exitFailure
  else
    do resPGF <- readPGF (oPgf opts)
       let resGram = Grammar resPGF []
       let resLang = fromJust (readLanguage (oLang opts))
       let (negativeExamples, positiveExamples) = partition (\e -> head e == '#') examples
       let positiveForests = examplesToForests resGram resLang positiveExamples
       let negativeForests = examplesToForests resGram resLang (map tail negativeExamples)
       let Just ofun = lookup (oObjfun opts) objectiveFunctions
       let problem = forestsToProblem (oSize opts) (oMerged opts) positiveForests negativeForests ofun
       let problemSize = length (problemConstraints problem)
       solution <- solve problem
       let newRules = filter isRule (snd solution)
       putStrLn $ "\n>>> Solution"
       mapM_ putStrLn newRules
       -- newGram <- generateGrammar resGram solution False
       -- putStrLn $ ">>> Grammar"
       -- print newGram

  where
    isRule r@('s':d:_) | isDigit d = filter (not . isDigit) r `notElem` ["s", "st", "stp"]
    isRule _ = True

