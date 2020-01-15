module Subgrammar.GFRule where

import Subgrammar.Common
import Data.Maybe
import Data.List
import qualified Data.Map.Lazy as Map
import Control.Monad.LPMonad
import Data.LinearProgram
import Data.LinearProgram.GLPK

data ObjectiveFunction = OF { fun :: Problem -> ObjectiveFunc String Int, direction :: Direction }
data Problem = Problem { trees :: [(String,[String])], rules :: [String] , formula :: LPM String Int ()}

-- Translate a list of forests into a constraint problem
forestsToProblem :: [Forest] -> Problem
forestsToProblem forests = 

  let
    -- helper to add consequtive numbers
    numbered = zip [1..]
    -- add sentence number to forests
    nForests =  numbered forests
    -- list of all sentences with all their trees
    sentenceTrees = [("s" ++ show sn, ["s" ++ show sn ++ "t" ++ show tn | (tn, t) <- numbered ts])| (sn,ts) <- nForests]
    -- list of all trees with all their rules
    treeRules = concat [[("s" ++ show sn ++ "t" ++ show tn,flatten t) | (tn, t) <- numbered ts]| (sn,ts) <- nForests]
    -- List of all sentence variables
    sentences = map fst sentenceTrees
    -- List of all tree variables
    trees = concatMap snd sentenceTrees
    -- List of all rule names
    rules = concatMap snd treeRules 
  in
    Problem sentenceTrees rules $
      do
        geqTo (linCombination [(1,s) | s <- sentences]) $ length sentences
        sequence_ [leqTo (linCombination ((-1,s):[(1,t) | t <- ts])) 0 | (s,ts) <- sentenceTrees]
        sequence_ [leqTo (linCombination ((-(length rs),t):[(1,r) | r <- rs])) 0 | (t,rs) <- treeRules]
        sequence_ $
          [setVarKind s BinVar | s <- sentences] ++
          [setVarKind t BinVar | t <- trees] ++
          [setVarKind r BinVar | r <- rules]

numTrees :: ObjectiveFunction
numTrees = OF numTreesOF Max
  where
    numTreesOF :: Problem -> ObjectiveFunc String Int
    numTreesOF (Problem trees _ _) = linCombination [(1,t) | (s,ts) <- trees,t <- ts]
    
solve :: Problem -> ObjectiveFunction -> IO Solution
solve problem (OF fun direction) =
  do
    (code,solution) <- glpSolveVars simplexDefaults $ execLPM $
      do
        setDirection direction
        setObjective (fun problem)
        formula problem
    return $ maybe (-1,[]) (\(val,vars) -> (val,[var | (var,vval) <- Map.toList vars,vval == 1])) solution
