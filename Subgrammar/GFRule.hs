module Subgrammar.GFRule where

import Subgrammar.Common
import Data.Maybe
import Data.List
import qualified Data.Map.Lazy as Map
import Control.Monad.LPMonad
import Data.LinearProgram
import Data.LinearProgram.GLPK

import PGF

data ObjectiveFunction = OF { fun :: Problem -> ObjectiveFunc String Int, direction :: Direction }
data Problem = Problem { trees :: [(String,[String])], rules :: [String] , formula :: LPM String Int ()}

instance Show Problem where
  show p = showProblem p
showProblem :: Problem -> String
showProblem (Problem ts rs f) = "Problem { trees = " ++ show ts ++ ", rules = " ++ show rs ++ ", ++ formula = " ++ (show $ execLPM f) ++ "}"
  
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
        sequence_ [geqTo (linCombination ((-1,s):[(1,t) | t <- ts])) 0 | (s,ts) <- sentenceTrees]
        sequence_ [geqTo (linCombination ((-(length rs),t):[(1,r) | r <- rs])) 0 | (t,rs) <- treeRules]
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
    let lp = execLPM $
          do
            setDirection direction
            setObjective (fun problem)
            formula problem
    (code,solution) <- glpSolveVars simplexDefaults lp
    return $ maybe (-1,[]) (\(val,vars) -> (val,[var | (var,vval) <- Map.toList vars,vval == 1])) solution

test :: IO ()
test = do
  -- load grammar
  putStrLn ">>> Load grammar"
  p <- readPGF "/tmp/Exemplum/Exemplum.pgf"
  let grammar = Grammar p ["/tmp/Exemplum/ExemplumEng.gf"]
  putStrLn $ ">>> Loaded " ++ (show $ length $ functions p) ++ " Rules"
  -- convert examples
  putStrLn ">>> Convert examples to forests"
  let forests = examplesToForests grammar (fromJust $ readLanguage "ExemplumEng") examples
  -- create csp
  putStrLn ">>> Convert forests to CSP"
  let problem = forestsToProblem forests
  putStrLn $ ">>> Got problem:\n" ++ show problem
  -- solve problem
  putStrLn ">>> Solve the CSP"
  solution <- solve problem numTrees
  putStrLn $ ">>> Got " ++ (show $ length $ snd solution) ++ " rules with a score of " ++ (show $ fst solution) ++ ": \n" ++ show (snd solution)
  -- create new grammar
  putStrLn ">>> Create New Grammar"
  grammar' <- generateGrammar grammar solution
  putStrLn $ ">>> Loaded " ++ (show $ length $ functions $ pgf grammar') ++ " Rules"
  -- check result
  let test = testExamples grammar' (fromJust $ readLanguage "ExemplumSubEng") examples
  if (and $ map snd test)  then
    putStrLn ">>> Success!!!"
  else
    putStrLn $ ">>> Failed covering:\n" ++ (unlines $ map fst $ filter (not . snd) test)
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
