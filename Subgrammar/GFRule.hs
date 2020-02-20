module Subgrammar.GFRule where

import Subgrammar.Common
import Data.Maybe
import Control.Monad.LPMonad
import Data.LinearProgram (geqTo,leqTo,linCombination,VarKind(..),ObjectiveFunc,Direction(..),writeLP)
import System.FilePath((</>))
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
    negative_rules = [r | rs <- negative_trees, r <- rs]
  in
    execLPM $ do
      setDirection dir
      setObjective (f positive_tags)
      geqTo (linCombination [(1,s) | s <- positive_sentences]) $ length positive_sentences
      sequence_ [geqTo (linCombination ((-1,s):[(1,t) | (t,_) <- ts])) 0 | (s,ts) <- positive_tags]
      sequence_ [geqTo (linCombination ((-(length rs),t):[(1,r) | r <- rs])) 0 | (_,ts) <- positive_tags,(t,rs) <- ts]
      sequence_ [leqTo (linCombination ([(1,r) | r <- rs])) (length rs) | rs <- negative_trees ]
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

-- | Test function
test :: IO ()
test = do
  -- load grammar
  putStrLn ">>> Load grammar"
  p <- readPGF $ path_to_exemplum</>"Exemplum.pgf"
  let grammar = Grammar p [path_to_exemplum</>"ExemplumEng.gf"]
  putStrLn $ ">>> Loaded " ++ (show $ length $ functions p) ++ " Rules"
  -- convert examples
  putStrLn ">>> Convert examples to forests"
  let forests = examplesToForests grammar (fromJust $ readLanguage "ExemplumEng") examples
  -- create csp
  putStrLn ">>> Convert forests to CSP"
  let problem = forestsToProblem forests [] numTrees
  putStrLn $ ">>> Got problem:\n" ++ show problem
  writeLP "/tmp/problem.lp" problem
  -- solve problem
  putStrLn ">>> Solve the CSP"
  solution <- solve problem
  putStrLn $ ">>> Got " ++ (show $ length $ snd solution) ++ " rules with a score of " ++ (show $ fst solution) ++ ": \n" ++ show (snd solution)
  -- create new grammar
  putStrLn ">>> Create New Grammar"
  grammar' <- generateGrammar grammar solution
  putStrLn $ ">>> Loaded " ++ (show $ length $ functions $ pgf grammar') ++ " Rules"
  -- check result
  let testResults = testExamples grammar' (fromJust $ readLanguage "ExemplumSubEng") examples
  if (and $ map snd testResults) then
    putStrLn ">>> Success!!!"
  else
    putStrLn $ ">>> Failed covering:\n" ++ (unlines $ map fst $ filter (not . snd) testResults)
  where
    examples = [
      "few bad fathers become big",
      "now John and Paris aren't good now",
      "many cold books come today",
      "now Paris and he today don't read few cold mothers",
      "it is blue",
      "they don't love every mother",
      "now it doesn't become blue in John",
      "John becomes cold",
      "it doesn't come",
      "on Paris now Paris comes",
      "now the bad cold fathers are big",
      "today she doesn't read Paris now now",
      "every computer doesn't break many mothers now",
      "Paris doesn't switch on it now today now",
      "today to it they become good now",
      "many fathers today on Paris don't hit many mothers",
      "to Paris on it today they don't close her now",
      "Paris isn't good today today",
      "it becomes bad already",
      "they don't break her today already today"
      ]
    testExamples :: Grammar -> Language -> [Example] -> [(String,Bool)]
    testExamples g l es = 
      zip es $ map (not.null) $ examplesToForests g l es


testNegative :: IO ()
testNegative =
  do
    -- load grammar
    putStrLn ">>> Load grammar"
    p <- readPGF $ path_to_exemplum</>"ExemplumEng.pgf"
    let grammar = Grammar p [path_to_exemplum</>"ExemplumEng.gf"]
    putStrLn $ ">>> Loaded " ++ (show $ length $ functions p) ++ " Rules"
      -- convert examples
    putStrLn ">>> Convert examples to forests"
    let positive_forests = examplesToForests grammar (fromJust $ readLanguage "ExemplumEng") positive
    let negative_forests = examplesToForests grammar (fromJust $ readLanguage "ExemplumEng") negative
    -- create csp
    putStrLn ">>> Convert forests to CSP"
    let problem = forestsToProblem positive_forests negative_forests numTrees
    putStrLn $ ">>> Got problem:\n" ++ show problem
    writeLP "/tmp/problem.lp" problem
    -- solve problem
    putStrLn ">>> Solve the CSP"
    solution <- solve problem
    putStrLn $ ">>> Got " ++ (show $ length $ snd solution) ++ " rules with a score of " ++ (show $ fst solution) ++ ": \n" ++ show (snd solution)
    -- create new grammar
    putStrLn ">>> Create New Grammar"
    grammar' <- generateGrammar grammar solution
    putStrLn $ ">>> Loaded " ++ (show $ length $ functions $ pgf grammar') ++ " Rules"
    -- check result
    let testPositiveResults = testExamples grammar' (fromJust $ readLanguage "ExemplumSubEng") positive
    let testNegativeResults = testExamples grammar' (fromJust $ readLanguage "ExemplumSubEng") negative
    if ((and $ map snd testPositiveResults) && (all not $ map snd testNegativeResults)) then
      putStrLn ">>> Success!!!"
      else
      do
        putStrLn $ ">>> Failed covering:\n" ++ (unlines $ map fst $ filter (not . snd) testPositiveResults)
        putStrLn $ ">>> Accidental covering:\n" ++ (unlines $ map fst $ filter snd testNegativeResults)
  where
    positive = [
      "few bad fathers become big",
      "now John and Paris aren't good now",
      "many cold books come today",
      "now Paris and he today don't read few cold mothers",
      "John becomes cold",
      "on Paris now Paris comes",
      "now the bad cold fathers are big",
      "every computer doesn't break many mothers now",
      "many fathers today on Paris don't hit many mothers",
      "Paris isn't good today today"
      ]
    negative = [
      "it is blue",
      "they don't love every mother",
      "it doesn't come",
      "today she doesn't read Paris now now",
      "now it doesn't become blue in John",
      "Paris doesn't switch on it now today now",
      "today to it they become good now",
      "to Paris on it today they don't close her now",
      "it becomes bad already",
      "they don't break her today already today"
        ]
    testExamples :: Grammar -> Language -> [Example] -> [(String,Bool)]
    testExamples g l es = 
      zip es $ map (not.null) $ examplesToForests g l es
