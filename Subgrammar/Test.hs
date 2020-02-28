module Subgrammar.Test where

import Subgrammar.Common
import qualified Subgrammar.GFRule as GFRule
import qualified Subgrammar.GFSubtree as GFSubtree

import PGF
import Data.Maybe
import Data.LinearProgram.GLPK.IO (writeLP)
import System.FilePath((</>))

-- | Tests the algorithm using rules and only positive examples
testRule :: IO ()
testRule = test GFRule.forestsToProblem GFRule.numRules False -- numTrees

-- | Tests the algorithm using rules and both positive and negative examples
testRuleNegative :: IO ()
testRuleNegative = testNegative GFRule.forestsToProblem numTrees False -- GFRule.numRules

-- | Tests the algorithm using subtrees and only positive examples
testSubtree :: IO ()
testSubtree = test (GFSubtree.forestsToProblem 2 3) numTrees False -- GFSubtree.numRules

testSubtreeMerge :: IO ()
testSubtreeMerge = test (GFSubtree.forestsToProblem 2 3) numTrees True -- GFSubtree.numRules 

-- | Tests the algorithm using subtrees and both positive and negative examples
testSubtreeNegative :: IO ()
testSubtreeNegative = testNegative (GFSubtree.forestsToProblem 2 3) numTrees False -- GFSubtree.numRules

testSubtreeNegativeMerge :: IO ()
testSubtreeNegativeMerge = testNegative (GFSubtree.forestsToProblem 2 3) numTrees True -- GFSubtree.numRules

runTest :: ([Forest] -> [Forest] -> ObjectiveFunction a -> Problem) -> (ObjectiveFunction a) -> Grammar -> [Forest] -> [Forest] -> IO Solution
runTest f o grammar pos neg =
  do
    -- create csp
    putStrLn ">>> Convert forests to CSP"
    let problem = f pos neg o
    -- putStrLn $ ">>> Got problem:\n" ++ show problem
    -- writeLP "/tmp/problem.lp" problem
    -- solve problem
    putStrLn ">>> Solve the CSP"
    solution <- solveCPLEX problem
    putStrLn $ ">>> Got " ++ (show $ length $ snd solution) ++ " rules with a score of " ++ (show $ fst solution) ++ ": \n" -- ++ show (snd solution)
    return solution
  
-- | Test function
test :: ([Forest] -> [Forest] -> ObjectiveFunction a -> Problem) -> (ObjectiveFunction a) -> Bool -> IO ()
test f o merge =
  do
    -- load grammar
    putStrLn ">>> Load grammar"
    p <- readPGF $ path_to_exemplum</>"ExemplumEng.pgf"
    let grammar = Grammar p [path_to_exemplum</>"ExemplumEng.gf"]
    putStrLn $ ">>> Loaded " ++ (show $ length $ functions p) ++ " Rules"
    -- convert examples
    putStrLn ">>> Convert examples to forests"
    let forests = examplesToForests grammar (fromJust $ readLanguage "ExemplumEng") examples
    solution <- runTest f o grammar forests []
    -- create new grammar
    putStrLn ">>> Create New Grammar"
    grammar' <- generateGrammar grammar solution False
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
  

testNegative :: ([Forest] -> [Forest] -> ObjectiveFunction a -> Problem) -> (ObjectiveFunction a) -> Bool ->IO ()
testNegative f o merge =
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
    solution <- runTest f o grammar positive_forests negative_forests      
    -- create new grammar
    putStrLn ">>> Create New Grammar"
    grammar' <- generateGrammar grammar solution merge
    putStrLn $ ">>> Loaded " ++ (show $ length $ functions $ pgf grammar') ++ " Rules"
    putStrLn $ (show $ functions $ pgf grammar')
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
      "many cold books come today",
      "it is blue",
      "they don't love every mother",
      "John becomes cold",
      "it doesn't come",
      "every computer doesn't break many mothers now",
      "Paris doesn't switch on it now today now",
      "many fathers today on Paris don't hit many mothers",
      "Paris isn't good today today",
      "it becomes bad already",
      "they don't break her today already today"
      ]
    negative = [
      "now John and Paris aren't good now",
      "now Paris and he today don't read few cold mothers",
      "now it doesn't become blue in John",
      "on Paris now Paris comes",
      "now the bad cold fathers are big",
      "today she doesn't read Paris now now",
      "today to it they become good now",
      "to Paris on it today they don't close her now"
        ]
    testExamples :: Grammar -> Language -> [Example] -> [(String,Bool)]
    testExamples g l es = 
      zip es $ map (not.null) $ examplesToForests g l es


