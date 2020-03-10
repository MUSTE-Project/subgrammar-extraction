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
testSubtree = test (GFSubtree.forestsToProblem 2 (Just 3)) numTrees False -- GFSubtree.numRules

testSubtreeMerge :: IO ()
testSubtreeMerge = test (GFSubtree.forestsToProblem 2 (Just 1)) GFSubtree.numRules True -- numTrees

-- | Tests the algorithm using subtrees and both positive and negative examples
testSubtreeNegative :: IO ()
testSubtreeNegative = testNegative (GFSubtree.forestsToProblem 2 (Just 3)) numTrees False -- GFSubtree.numRules

testSubtreeNegativeMerge :: IO ()
testSubtreeNegativeMerge = testNegative (GFSubtree.forestsToProblem 2 (Just 3)) numTrees True -- GFSubtree.numRules

dyckRules :: IO ()
-- dyckRules = testDyck GFRule.forestsToProblem GFRule.numRules False False -- numTrees
dyckRules = testDyck GFRule.forestsToProblem numTrees False False -- GFRule.numRules

dyckRulesNegative :: IO ()
--dyckRulesNegative = testDyck GFRule.forestsToProblem GFRule.numRules True False -- numTrees
dyckRulesNegative = testDyck GFRule.forestsToProblem numTrees True False -- GFRule.numRules

dyckSubtrees :: IO ()
dyckSubtrees = testDyck (GFSubtree.forestsToProblem 2 (Just 3)) GFSubtree.numRules False False -- numTrees

dyckSubtreesNegative :: IO ()
dyckSubtreesNegative = testDyck (GFSubtree.forestsToProblem 2 (Just 3)) GFSubtree.numRules True False -- numTrees

dyck2Subtrees :: IO ()
dyck2Subtrees = testDyck2 (GFSubtree.forestsToProblem 3 (Just 2)) GFSubtree.numRules False False -- numTrees
--dyck2Subtrees = testDyck2 (GFSubtree.forestsToProblem 3 Nothing) numTrees False False -- GFSubtree.numRules

dyck2SubtreesNegative :: IO ()
--dyck2SubtreesNegative = testDyck2 (GFSubtree.forestsToProblem 3 (Just 2)) GFSubtree.numRules False False -- numTrees
dyck2SubtreesNegative = testDyck2 (GFSubtree.forestsToProblem 3 (Just 2)) numTrees True False -- GFSubtree.numRules


advSubtrees :: IO ()
advSubtrees = testAdv (GFSubtree.forestsToProblem 2 (Just 3)) GFSubtree.numRules False False -- numTrees
--advSubtrees = testAdv (GFSubtree.forestsToProblem 3 Nothing) numTrees False False -- GFSubtree.numRules

advSubtreesNegative :: IO ()
advSubtreesNegative = testAdv (GFSubtree.forestsToProblem 2 (Just 3)) GFSubtree.numRules True False -- numTrees
--advSubtreesNegative = testAdv (GFSubtree.forestsToProblem 2 (Just 4)) numTrees True False -- GFSubtree.numRules


adv2Subtrees :: IO ()
adv2Subtrees = testAdv2 (GFSubtree.forestsToProblem 2 Nothing) GFSubtree.numRules False False -- numTrees
--adv2Subtrees = testAdv2 (GFSubtree.forestsToProblem 3 Nothing) numTrees False False -- GFSubtree.numRules

adv2SubtreesNegative :: IO ()
adv2SubtreesNegative = testAdv2 (GFSubtree.forestsToProblem 2 Nothing) GFSubtree.numRules True False -- numTrees
--adv2SubtreesNegative = testAdv2 (GFSubtree.forestsToProblem 2 (Just 4)) numTrees True False -- GFSubtree.numRules

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
    putStrLn $ ">>> Got " ++ (show $ length $ filter isRule $ snd solution) ++ " rules with a score of " ++ (show $ fst solution) ++ ": \n" ++ show (filter isRule $ snd solution)
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
    putStrLn $ ">>> Got solution: " ++ show (filter isRule $ snd solution)
    -- create new grammar
    putStrLn ">>> Create New Grammar"
    grammar' <- generateGrammar grammar solution merge
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
      "now John and Paris aren't good now"
--      "many cold books come today"
      -- "now Paris and he today don't read few cold mothers",
      -- "it is blue",
      -- "they don't love every mother",
      -- "now it doesn't become blue in John",
      -- "John becomes cold",
      -- "it doesn't come",
      -- "on Paris now Paris comes",
      -- "now the bad cold fathers are big",
      -- "today she doesn't read Paris now now",
      -- "every computer doesn't break many mothers now",
      -- "Paris doesn't switch on it now today now",
      -- "today to it they become good now",
      -- "many fathers today on Paris don't hit many mothers",
      -- "to Paris on it today they don't close her now",
      -- "Paris isn't good today today",
      -- "it becomes bad already",
      -- "they don't break her today already today"
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
    let positive_forests = examplesToForests grammar (fromJust $ readLanguage "ExemplumEng") positive'
    let negative_forests = examplesToForests grammar (fromJust $ readLanguage "ExemplumEng") negative'
    solution <- runTest f o grammar positive_forests negative_forests
    -- create new grammar
    putStrLn ">>> Create New Grammar"
    grammar' <- generateGrammar grammar solution merge
    putStrLn $ ">>> Loaded " ++ (show $ length $ functions $ pgf grammar') ++ " Rules"
    putStrLn $ (show $ functions $ pgf grammar')
    -- check result
    let testPositiveResults = testExamples grammar' (fromJust $ readLanguage "ExemplumSubEng") positive'
    let testNegativeResults = testExamples grammar' (fromJust $ readLanguage "ExemplumSubEng") negative'
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
    positive' = [
      -- "few bad fathers become big",
      -- "now John and Paris aren't good now",
      -- "many cold books come today",
      -- "now Paris and he today don't read few cold mothers",
      -- "it is blue",
      -- "they don't love every mother",
      -- "now it doesn't become blue in John",
      -- "John becomes cold",
      -- "it doesn't come",
      -- "on Paris now Paris comes",
      -- "now the bad cold fathers are big",
      "every computer doesn't break many mothers now",
      -- "today to it they become good now",
      -- "many fathers today on Paris don't hit many mothers",
      "to Paris on it today they don't close her now", -- problematic
--      "Paris isn't good today today",
      "it becomes bad already"
      ]
    negative' = [
      "they don't break her today already today",
      "Paris doesn't switch on it now today now",
      "today she doesn't read Paris now now"
      ]
    testExamples :: Grammar -> Language -> [Example] -> [(String,Bool)]
    testExamples g l es = 
      zip es $ map (not.null) $ examplesToForests g l es


testDyck :: ([Forest] -> [Forest] -> ObjectiveFunction a -> Problem) -> (ObjectiveFunction a) -> Bool -> Bool ->IO ()
testDyck f o neg merge =
  do
    -- load grammar
    putStrLn ">>> Load grammar"
    p <- readPGF $ "../pgfs/Dyck.pgf"
    let grammar = Grammar p ["/home/herb/src/own/subgrammar-grammars/Dyck/Dyck.gf"]
    putStrLn $ ">>> Loaded " ++ (show $ length $ functions p) ++ " Rules"
      -- convert examples
    putStrLn ">>> Convert examples to forests"
    let positive_forests = examplesToForests grammar (fromJust $ readLanguage "Dyck") positive
    let negative_forests = examplesToForests grammar (fromJust $ readLanguage "Dyck") $ if neg then negative else []
    solution <- runTest f o grammar positive_forests negative_forests
    return ()
    -- -- create new grammar
    -- putStrLn ">>> Create New Grammar"
    -- grammar' <- generateGrammar grammar solution merge
    -- putStrLn $ ">>> Loaded " ++ (show $ length $ functions $ pgf grammar') ++ " Rules"
    -- putStrLn $ (show $ functions $ pgf grammar')
    -- -- check result
    -- let testPositiveResults = testExamples grammar' (fromJust $ readLanguage "Dyck") positive
    -- let testNegativeResults = testExamples grammar' (fromJust $ readLanguage "Dyck") negative
    -- if ((and $ map snd testPositiveResults) && (all not $ map snd testNegativeResults)) then
    --   putStrLn ">>> Success!!!"
    --   else
    --   do
    --     putStrLn $ ">>> Failed covering:\n" ++ (unlines $ map fst $ filter (not . snd) testPositiveResults)
    --     putStrLn $ ">>> Accidental covering:\n" ++ (unlines $ map fst $ filter snd testNegativeResults)
  where
    positive = [
      "( )",
      "[ ]",
      "( ) ( )"
      -- "( ) ( )",
      -- "[ ] [ ]",
      -- "( ( ) )",
      -- "( [ ] )",
      -- "[ ( ) ]",
      -- "[ [ ] ]"
      ]
    negative = [
      -- "[",
      -- "]",
      -- "(",
      -- ")",
      "( ]",
      "[ )",
      "(",
      ")",
      "[",
      "]"
      ]
    testExamples :: Grammar -> Language -> [Example] -> [(String,Bool)]
    testExamples g l es = 
      zip es $ map (not.null) $ examplesToForests g l es


testDyck2 :: ([Forest] -> [Forest] -> ObjectiveFunction a -> Problem) -> (ObjectiveFunction a) -> Bool -> Bool ->IO ()
testDyck2 f o neg merge =
  do
    -- load grammar
    putStrLn ">>> Load grammar"
    p <- readPGF $ "../pgfs/Dyck2.pgf"
    let grammar = Grammar p ["../../subgrammar-grammars/Dyck2/Dyck2Cnc.gf"]
    putStrLn $ ">>> Loaded " ++ (show $ length $ functions p) ++ " Rules"
      -- convert examples
    putStrLn ">>> Convert examples to forests"
    let positive_forests = examplesToForests grammar (fromJust $ readLanguage "Dyck2Cnc") positive
    let negative_forests = examplesToForests grammar (fromJust $ readLanguage "Dyck2Cnc") $ if neg then negative else []
    solution <- runTest f o grammar positive_forests negative_forests
    return ()
    -- create new grammar
    -- putStrLn ">>> Create New Grammar"
    -- grammar' <- generateGrammar grammar solution merge
    -- putStrLn $ ">>> Loaded " ++ (show $ length $ functions $ pgf grammar') ++ " Rules"
    -- putStrLn $ (show $ functions $ pgf grammar')
    -- -- check result
    -- let testPositiveResults = testExamples grammar' (fromJust $ readLanguage "Dyck2Cnc") positive
    -- let testNegativeResults = testExamples grammar' (fromJust $ readLanguage "Dyck2Cnc") negative
    -- if ((and $ map snd testPositiveResults) && (all not $ map snd testNegativeResults)) then
    --   putStrLn ">>> Success!!!"
    --   else
    --   do
    --     putStrLn $ ">>> Failed covering:\n" ++ (unlines $ map fst $ filter (not . snd) testPositiveResults)
    --     putStrLn $ ">>> Accidental covering:\n" ++ (unlines $ map fst $ filter snd testNegativeResults)
  where
    positive = [
      "[ ( ) ]" ,
--      "( [ ] )" 
--      "( ) [ ]" ,
      "[ ] ( )"
      ]
    negative = [
      "( ]",
      "[ )"
      ]
    testExamples :: Grammar -> Language -> [Example] -> [(String,Bool)]
    testExamples g l es = 
      zip es $ map (not.null) $ examplesToForests g l es


testAdv :: ([Forest] -> [Forest] -> ObjectiveFunction a -> Problem) -> (ObjectiveFunction a) -> Bool -> Bool ->IO ()
testAdv f o neg merge =
  do
    -- load grammar
    putStrLn ">>> Load grammar"
    p <- readPGF $ "../pgfs/AAbs.pgf"
    let grammar = Grammar p ["../../subgrammars-grammars/Advtest/AEng.gf"]
    putStrLn $ ">>> Loaded " ++ (show $ length $ functions p) ++ " Rules"
      -- convert examples
    putStrLn ">>> Convert examples to forests"
    let positive_forests = examplesToForests grammar (fromJust $ readLanguage "AEng") positive
    let negative_forests = examplesToForests grammar (fromJust $ readLanguage "AEng") $ if neg then negative else []
    solution <- runTest f o grammar positive_forests negative_forests
    return ()
    -- create new grammar
    -- putStrLn ">>> Create New Grammar"
    -- grammar' <- generateGrammar grammar solution merge
    -- putStrLn $ ">>> Loaded " ++ (show $ length $ functions $ pgf grammar') ++ " Rules"
    -- putStrLn $ (show $ functions $ pgf grammar')
    -- -- check result
    -- let testPositiveResults = testExamples grammar' (fromJust $ readLanguage "Dyck2Cnc") positive
    -- let testNegativeResults = testExamples grammar' (fromJust $ readLanguage "Dyck2Cnc") negative
    -- if ((and $ map snd testPositiveResults) && (all not $ map snd testNegativeResults)) then
    --   putStrLn ">>> Success!!!"
    --   else
    --   do
    --     putStrLn $ ">>> Failed covering:\n" ++ (unlines $ map fst $ filter (not . snd) testPositiveResults)
    --     putStrLn $ ">>> Accidental covering:\n" ++ (unlines $ map fst $ filter snd testNegativeResults)
  where
--     positive = [
--       -- Context
--       "a boy hates a girl",
-- --      "the girl didn't read a book",
--       "a girl hated a boy",
--       "a boy reads a book",
--       -- Main example
--       "today a girl reads a book in a garden",
--       "a girl in a garden reads a book"
--       ]
--     negative = [
--       "in a garden a girl reads a book",
--       "a girl today reads a book"
--       ]
    positive = [
      -- Context
      "the boy hates a girl",
      "the girl didn't read a book",
      "the girls hated boys",
      "boys have read the books",
      -- Main example
      "today a girl reads books in the garden",
      "a girl in the garden reads books",
      "a girl in the garden reads books today"
      ]
    negative = [
      "in the garden a girl reads books",
      "a girl today reads books"
      ]
    testExamples :: Grammar -> Language -> [Example] -> [(String,Bool)]
    testExamples g l es = 
      zip es $ map (not.null) $ examplesToForests g l es


testAdv2 :: ([Forest] -> [Forest] -> ObjectiveFunction a -> Problem) -> (ObjectiveFunction a) -> Bool -> Bool ->IO ()
testAdv2 f o neg merge =
  do
    -- load grammar
    putStrLn ">>> Load grammar"
    p <- readPGF $ "../pgfs/Adv.pgf"
    let grammar = Grammar p ["../../subgrammars-grammars/Adv/AdvEng.gf"]
    putStrLn $ ">>> Loaded " ++ (show $ length $ functions p) ++ " Rules"
      -- convert examples
    putStrLn ">>> Convert examples to forests"
    let positive_forests = examplesToForests grammar (fromJust $ readLanguage "AdvEng") positive
    let negative_forests = examplesToForests grammar (fromJust $ readLanguage "AdvEng") $ if neg then negative else []
    solution <- runTest f o grammar positive_forests negative_forests
    return ()
  where
     positive = [
       "I eat pizza with pineapple",
--       "I eat pizza with cheese",
       "pizza with pineapple is delicious",
--       "I run with scissors",
       "I run today",
       "I sleep now",
       "I run"
      ]
     negative = [
       "I eat pizza with scissors"
       ]
