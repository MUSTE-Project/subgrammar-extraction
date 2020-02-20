module Subgrammar.Test where

import Subgrammar.Common
import qualified Subgrammar.GFRule as GFRule
import qualified Subgrammar.GFSubtree as GFSubtree

import PGF
import Data.Maybe
import Data.LinearProgram.GLPK.IO (writeLP)
import System.FilePath((</>))

testRule :: IO ()
testRule = test GFRule.forestsToProblem GFRule.numRules -- numTrees

testSubtree :: IO ()
testSubtree = test (GFSubtree.forestsToProblem 2 3) GFSubtree.numRules -- numTrees

-- | Test function
test :: ([Forest] -> [Forest] -> ObjectiveFunction a -> Problem) -> (ObjectiveFunction a) -> IO ()
test f o = do
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
  let problem = f forests [] o
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


testNegative :: ([Forest] -> [Forest] -> ObjectiveFunction a -> Problem) -> (ObjectiveFunction a) -> IO ()
testNegative f o =
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
    let problem = f positive_forests negative_forests o
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
