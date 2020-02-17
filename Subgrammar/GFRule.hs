module Subgrammar.GFRule where

import Subgrammar.Common
import Data.Maybe
import Control.Monad.LPMonad
import Data.LinearProgram
import System.FilePath((</>))

import PGF
  
-- Translate a list of forests into a constraint problem
forestsToProblem :: [Forest] -> ObjectiveFunction [String] -> Problem
forestsToProblem forests (OF f dir) = 
  let
    -- helper to add consequtive numbers
    numbered :: [a] -> [(Int,a)]
    numbered = zip [1..]
    -- Hierarchy of tags for sentences, trees and rules
    tags =   [(s_tag, [(t_tag,
                        flatten t
                       )
                      | (tn,t) <- numbered ts,let t_tag = s_tag ++ "t" ++ show tn]
              )
             | (sn,ts) <- numbered forests, let s_tag = "s" ++ show sn] :: [(String,[(String,[String])])]
    -- List of all sentence variables
    sentences = map fst tags 
    -- List of all tree variables
    trees = [t | (_,ts) <- tags, (t,_) <- ts]
    -- List of all rule names
    rules = [r | (_,ts) <- tags, (_,rs) <- ts, r <- rs]
  in
    execLPM $ do
      setDirection dir
      setObjective (f tags)
      geqTo (linCombination [(1,s) | s <- sentences]) $ length sentences
      sequence_ [geqTo (linCombination ((-1,s):[(1,t) | (t,_) <- ts])) 0 | (s,ts) <- tags]
      sequence_ [geqTo (linCombination ((-(length rs),t):[(1,r) | r <- rs])) 0 | (_,ts) <- tags,(t,rs) <- ts]
      sequence_ $
          [setVarKind s BinVar | s <- sentences] ++
          [setVarKind t BinVar | t <- trees] ++
          [setVarKind r BinVar | r <- rules]

-- | Objective function to minimize the number of rules
numRules :: ObjectiveFunction [String]
numRules = OF numRulesOF Min
  where
    numRulesOF :: [(String,[(String,[String])])] -> ObjectiveFunc String Int
    numRulesOF tags = linCombination [(1,r) | (_,ts) <- tags,(_,rs) <- ts,r <- rs]

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
  let problem = forestsToProblem forests numTrees
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
