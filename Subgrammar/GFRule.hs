module Subgrammar.GFRule
  ( forestsToProblem
  , numRules
  , numRulesTrees
  , weightedRules
  ) where

{-
Subgrammar extraction based on grammar rule optimization
-}

import Subgrammar.Common
import Data.Maybe
import Control.Monad.LPMonad
import Data.LinearProgram (geqTo,leqTo,linCombination,VarKind(..),ObjectiveFunc,Direction(..),writeLP)
import Data.List
import qualified Data.Map.Lazy as M
import PGF
  
-- Translate a list of forests into a constraint problem
forestsToProblem :: [Forest] -> [Forest] -> ObjectiveFunction [String] -> Problem
forestsToProblem positive_forests negative_forests (OF f dir) = 
  let
    -- helper to add consequtive numbers
    numbered :: [a] -> [(Int,a)]
    numbered = zip [1..]
    -- Hierarchy of tags for sentences, trees and rules
    positive_tags =   [(s_tag, [(t_tag,
                                 flatten t
                                )
                               | (tn,t) <- numbered ts,let t_tag = s_tag ++ "t" ++ show tn]
                       )
                      | (sn,ts) <- numbered positive_forests, let s_tag = "s" ++ show sn] :: [(String,[(String,[String])])]
    negative_trees = [flatten t | ts<- negative_forests,t <- ts]
    -- List of all sentence variables
    positive_sentences = map fst positive_tags
    -- List of all tree variables
    positive_trees = [t | (_,ts) <- positive_tags, (t,_) <- ts]
    -- List of all rule names
    positive_rules = [r | (_,ts) <- positive_tags, (_,rs) <- ts, r <- rs]
    -- List of all rule names
    negative_rules = [r | rs <- negative_trees, r <- rs]
  in
    execLPM $ do
      setDirection dir
      setObjective (f positive_tags)
      -- Positive rules for positive trees
      sequence_ [geqTo (linCombination ((-(length rs),t):[(1,r) | r <- rs])) 0 | (_,ts) <- positive_tags,(t,rs) <- ts]
      -- Positive trees for positive sentences 
      sequence_ [geqTo (linCombination ((-1,s):[(1,t) | (t,_) <- ts])) 0 | (s,ts) <- positive_tags]
      -- Positive sentences
      geqTo (linCombination [(1,s) | s <- positive_sentences]) $ length positive_sentences
      sequence_ [leqTo (linCombination ([(1,r) | r <- rs])) (length rs - 1) | rs <- negative_trees ]
      sequence_ $
          [setVarKind s BinVar | s <- positive_sentences] ++
          [setVarKind t BinVar | t <- positive_trees] ++
          [setVarKind r BinVar | r <- nub (positive_rules ++ negative_rules)]

-- | Objective function to minimize the number of rules
numRules :: ObjectiveFunction [String]
numRules = OF numRulesOF Min
  where
    numRulesOF :: [(String,[(String,[String])])] -> ObjectiveFunc String Int
    numRulesOF tags = linCombination [(1,r) | (_,ts) <- tags,(_,rs) <- ts,r <- rs]

-- | Objective function to minimize the sum of rules and trees
numRulesTrees :: ObjectiveFunction [String]
numRulesTrees = OF numRulesOF Min
  where
    numRulesOF :: [(String,[(String,[String])])] -> ObjectiveFunc String Int
    numRulesOF tags = linCombination $ nub [(1,r) | (_,ts) <- tags,(_,rs) <- ts, r <- rs] ++ nub [(1,t) | (_,ts) <- tags,(t,_) <- ts]

-- | Objective function to minimize the sum of all rules weighted by number of occurences
weightedRules :: ObjectiveFunction [String]
weightedRules = OF numRulesOF Min
  where
    numRulesOF :: [(String,[(String,[String])])] -> ObjectiveFunc String Int
    numRulesOF tags =
      let
        ruleVars = [r | (_,ts) <- tags, (_,rs) <- ts, r <- rs]
        ruleFreq = Prelude.foldl (\m k -> M.alter (maybe (Just 1) (\n -> Just (n + 1))) k m) M.empty $ ruleVars
        ruleCount = length $ nub ruleVars
      in
        linCombination $ nub [(round ((fromIntegral (ruleFreq M.! r) / fromIntegral ruleCount) * 100) ,r) | r <- ruleVars]

