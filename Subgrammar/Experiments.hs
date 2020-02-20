module Subgrammar.Experiments where

import System.Random (mkStdGen,setStdGen)
import Data.List
import Data.Maybe
import Test.QuickCheck
import PGF
import Subgrammar.Common
import Subgrammar.GFSubtree
import Control.Monad
import Data.LinearProgram.GLPK.IO
import System.IO

-- global parameters
-- how many times reshuffle the sentences
reshufflingCount :: Int
reshufflingCount = 1
-- how many examples in total
maxExampleCount :: Int
maxExampleCount = 20
-- lower bound for the set of examples
minExampleCount :: Int
minExampleCount = 1
-- what tree depths to try
treeDepths :: [Int]
treeDepths = [6]
-- what subtree sizes to try (>1 leads to an explosion in the problem size)
subtreeSizes :: [Int]
subtreeSizes = [2]
-- how many merged rules that are allowed in a tree (the more, the larger problem size)
maxMergedList :: [Int]
maxMergedList = [1,3,5]
-- objective functions to try
objectiveFunctions :: [(String, ObjectiveFunction [(String, [String])])]
objectiveFunctions = [("numTrees",numTrees)] -- ("numRules",numRules)]  -- , ("numTrees",numTrees)]
-- languages to test
testLanguages :: [String]
testLanguages = ["Eng", "Ger", "Fin", "Swe", "Spa"]
-- grammar paths to be combined with the language
grammarsDirectory = "pgfs/"
resourceGrammarPrefix = "Lang"
exemplumGrammarPrefix = "Exemplum"

-- | Returns the rules and the associated precision and recall
recreateFromExamples :: Grammar -> Language -> Grammar -> [Example] -> Int -> Int -> ObjectiveFunction [(String, [String])] -> IO (Integer,[String],Int,Double,Double)
recreateFromExamples g_r lang_r g_0 examples maxSubtreeSize maxMergedPerTree ofun =
  do
    start <- startTimer
    -- putStrLn $ ">>> Working on " ++ show examples
    let forests = examplesToForests g_r lang_r examples
    -- create csp
    when debug $ putStrLn $ ">>> Create problem"
    let problem = forestsToProblem forests maxSubtreeSize maxMergedPerTree ofun
    let problemSize = length (problemConstraints problem)
    -- solve problem
    when debug $ putStrLn $ ">>> Solve problem, size: " ++ show problemSize
    when debug $ writeLP "/tmp/problem.lp" problem
    solution <- solve problem
    -- solution <- solveCPLEX problem
    -- get the results
    when debug $ putStrLn $ ">>> Analyze results"
    let splitted = filter (/= hole) $ concat [split "#" r|r <- snd solution]
    -- Only look at the "rules" in the results that are actually in the resource grammar
    let precision = (fromIntegral $ length (intersect (map showCId $ functions $ pgf g_0) splitted)) / (fromIntegral $ length (intersect (map showCId $ functions $ pgf g_r) splitted))
    let recall = (fromIntegral $ length (intersect (map showCId $ functions $ pgf g_0) splitted)) / (fromIntegral $ length (functions $ pgf g_0))
    timer <- stopTimer start
    return (timer,splitted,problemSize, precision,recall)

-- | Return the examples used, the rules created, precision and recall
recreateGrammar :: Grammar -> Language -> Grammar -> Int -> Int -> Int -> Int-> ObjectiveFunction [(String, [String])] -> IO [(Int,Int,Integer,[String],[String],Int,Double,Double)]
recreateGrammar g_r lang_r g_0 treeDepth maxSubtreeSize maxMergedPerTree repetitions ofun = do
  let gen = mkStdGen 4 -- chosen by a fair dice role
  setStdGen gen
  when debug $ putStrLn "  >>> Generate trees"
  let trees = take maxExampleCount $ nub $ generateRandomDepth gen (pgf g_0) (startCat $ pgf g_0) (Just treeDepth)
  when debug $ putStrLn "  >>> Linearize trees"
  let sentences = [linearize (pgf g_r) lang_r t | t <- trees]
  when debug $ putStrLn "  >>> Randomize sentences"
  shuffledSentences <- sequence (replicate (fromIntegral repetitions) (generate (shuffle sentences)))
  when debug (putStrLn $ show shuffledSentences)
  when debug $ putStrLn "  >>> Start process"
  sequence [(\(timer,rules,psize,prec,recall) -> (count,exampleCount,timer,examples,rules,psize,prec,recall)) <$> recreateFromExamples g_r lang_r g_0 examples maxSubtreeSize maxMergedPerTree ofun
           | (count,shuffled) <- zip [1..] shuffledSentences,
             exampleCount <- [minExampleCount..length shuffled],
             let examples = (take exampleCount shuffled)
           ]

recreateExemplum :: FilePath -> IO ()
recreateExemplum outFile = 
  do
    when debug $ putStrLn $ ">>> Load RGLs: " ++ show testLanguages
    resourceGrammars <- mapM readPGF [grammarsDirectory ++ resourceGrammarPrefix ++ lang ++ ".pgf" | lang <- testLanguages]
    let resourceLanguages = [resourceGrammarPrefix ++ lang | lang <- testLanguages]
    when debug $ putStrLn ">>> Load Exemplum"
    exemplumGrammars <- mapM readPGF [grammarsDirectory ++ exemplumGrammarPrefix ++ lang ++ ".pgf" | lang <- testLanguages]
    when debug $ putStrLn ">>> Work Work Work"
    withFile outFile WriteMode
      (\handle ->
          do
            hSetBuffering handle NoBuffering
            hPutStrLn handle "Language;ShuffleNo;ExampleCount;TreeDepth;SubtreeSize;MaxMergedPerTree;ObjectiveFunction;Time;ProblemSize;Precision;Recall;Rules;Examples"
            sequence 
              [ do putStrLn $ "\n>>> TESTING: depth " ++ show treeDepth ++ "; size " ++ show maxSubtreeSize ++ "; maxMerged " ++ show maxMergedPerTree ++ "; ofun " ++ oname ++ "; lang " ++ lname ++ "; reshufflings " ++ show reshufflingCount ++ "; examples " ++ show minExampleCount ++ ".." ++ show maxExampleCount ++ "\n"
                   results <- recreateGrammar (Grammar lpgf_r []) (fromJust $ readLanguage lname) (Grammar lpgf_0 []) treeDepth maxSubtreeSize maxMergedPerTree reshufflingCount ofun
                   hPutStr handle $ unlines
                     [(lname ++ ";" ++ show shuffleNo ++ ";" ++ show exampleCount ++ ";" ++ show treeDepth ++ ";" ++
                       show maxSubtreeSize ++ ";" ++ show maxMergedPerTree ++ ";" ++ oname ++ ";" ++ show timer ++ ";" ++ show psize ++ ";" ++ show prec ++ ";" ++
                       show recall ++ ";" ++ show rules ++ ";" ++ show examples)
                     | (shuffleNo,exampleCount,timer,examples,rules,psize,prec,recall) <- results]
              | maxSubtreeSize <- subtreeSizes,
                (lname,lpgf_r,lpgf_0) <- zip3 resourceLanguages resourceGrammars exemplumGrammars,
                treeDepth <- treeDepths,
                maxMergedPerTree <- maxMergedList,
                (oname,ofun) <- objectiveFunctions
              ]
            hFlush handle -- should do nothing without buffering
      )

{-
First experiment: depth 9, numRules and all languages
Second experiment: depth 9, finnish, all objective functions
Third experiment: english, numRules and all depth 5..9
-}
      
compareTreebank :: Grammar -> Language -> [(String,Tree)] -> Int -> Int -> ObjectiveFunction [(String,[String])] -> IO (Double,Double)
compareTreebank g_r lang_r treeBank maxSubtreeSize maxMergedPerTree ofun =
  do
    let
      examples = map fst treeBank
      forests = examplesToForests g_r lang_r examples
      problem = forestsToProblem forests maxSubtreeSize maxMergedPerTree ofun
    solution <- solve problem
    g' <- generateGrammar g_r solution
--    let accuracy = (fromIntegral $ length $ filter id [t `elem` parse (pgf g') lang_r (startCat $ pgf g') e | (e,t) <- treeBank]) / (fromIntegral $ length treeBank) :: Double
    let accuracy = (fromIntegral $ sum [maximum [length (intersect (flatten t) (flatten p)) | p <-parses ] | (e,t) <- treeBank, let parses = parse (pgf g') lang_r (startCat $ pgf g') e]) / (fromIntegral $ sum [length $ flatten t | (_,t) <- treeBank ]) :: Double
    let ambiguity = (fromIntegral $ sum [ length $ parse (pgf g') lang_r (startCat $ pgf g') e | (e,t) <- treeBank]) / (fromIntegral $ length treeBank) :: Double
    return (accuracy,ambiguity)



