module Subgrammar.GFRule where

import PGF
import Data.Maybe
import Data.List
import Control.Monad.LPMonad
import Data.LinearProgram
import Data.LinearProgram.GLPK
type Example = String
type Forest = [Tree]
type Grammar = PGF
data Formula = Variable String | Conj [Formula] | Disj [Formula] | Neg Formula | Imp Formula Formula deriving (Show)
data Problem = Problem { trees :: [String], rules :: [String] , formula :: Formula } 
type Solution = [String]
data ObjectiveFunction = OF { fun :: Problem -> ObjectiveFunc String Int, direction :: Direction }

-- Given a grammar translate an example into a set of syntax trees
exampleToForest :: Grammar -> Example -> Forest
exampleToForest grammar example =
  concatMap snd $ parseAllLang grammar (startCat grammar) example

-- Combine several forests to one
combineForests :: [Forest] -> Forest
combineForests = concat

forestToProblem :: Forest -> Problem
forestToProblem forest =
  let
    numberedTrees = zip forest [1..]    
  in
    Problem
    ["t" ++ show n | (_,n) <- numberedTrees]
    (nub $ concatMap treeToRules forest)
    (Disj [Imp (Variable $ "t" ++ show n) (Conj (map Variable $ treeToRules t)) | (t,n) <- numberedTrees])
  where
    treeToRules :: Tree -> [String]
    treeToRules tree = maybe [] (\(f,ts) -> (show $ showCId f):(concatMap treeToRules ts)) $ unApp tree

numTrees :: ObjectiveFunction
numTrees = OF numTreesOF Max
  where
    numTreesOF :: Problem -> ObjectiveFunc String Int
    numTreesOF (Problem trees _ _) = linCombination [(1,t) | t <- trees]
    
solve :: Problem -> ObjectiveFunction -> Solution
solve = undefined
  where
    formulaToLP :: Formula -> LP String Int
    formulaToLP formula = execLPM $ do
--      add 
      return ()

generateGrammar :: Solution -> Grammar
generateGrammar = undefined
