module Subgrammar.Experiments where

import System.Random (mkStdGen)
import Data.List
import Data.Maybe
-- import System.FilePath((</>))
-- import Control.Concurrent.ParallelIO
import Control.Concurrent.ParallelIO.Local
import System.Random.Shuffle
import PGF
import Subgrammar.Common
import Subgrammar.GFSubtree

-- | Returns the rules and the associated precision and recall
recreateFromExamples :: Grammar -> Language -> Grammar -> [Example] -> Int -> IO ([String],Double,Double)
recreateFromExamples g_r lang_r g_0 examples maxSubtreeSize =
  do
    -- putStrLn $ ">>> Working on " ++ show examples
    let forests = examplesToForests g_r lang_r examples
    -- create csp
    -- putStrLn $ ">>> Create problem"
    let problem = forestsToProblem forests maxSubtreeSize numTrees
    -- solve problem
    -- putStrLn $ ">>> Solve problem"
    solution <- solve problem
    -- get the results
    -- putStrLn $ ">>> Analyze results"
    let splitted = filter (/= "@") $ concat [split "#" r|r <- snd solution]
    let precision = (fromIntegral $ length (intersect (map showCId $ functions $ pgf g_0) splitted)) / (fromIntegral $ length splitted)
    let recall = (fromIntegral $ length (intersect (map showCId $ functions $ pgf g_0) splitted)) / (fromIntegral $ length (functions $ pgf g_0))
    return (splitted, precision,recall)

-- | Return the examples used, the rules created, precision and recall
recreateGrammar :: Grammar -> Language -> Grammar -> Int -> Int -> Int -> Int -> IO [([String],[String],Double,Double)]
recreateGrammar g_r lang_r g_0 exampleCount treeDepth maxSubtreeSize runs = do
  let gen = mkStdGen 4 -- chosen by a fair dice role
  putStrLn ">>> Generate trees"
  let trees = take exampleCount $ generateRandomDepth gen (pgf g_0) (startCat $ pgf g_0) (Just treeDepth)
  putStrLn ">>> Linearize trees"
  let sentences = [linearize (pgf g_r) lang_r t | t <- trees]
  putStrLn ">>> Randomize sentences"
  let shuffledSentences = map (\l -> shuffle' l (length l) gen) $ replicate runs sentences 
  putStrLn ">>> Start process"
  -- sequence
  withPool 4 $ \p -> parallel p [(\(r,prec,re) -> (es,r,prec,re)) <$> recreateFromExamples g_r lang_r g_0 es maxSubtreeSize | shuffled <- shuffledSentences,
                                 l <- [1..length shuffled-1], let es = (take l shuffled)]

recreateExemplum :: Int -> Int -> Int -> Int -> IO [([String],[String],Double,Double)]
recreateExemplum exampleCount treeDepth maxSubtreeSize runs =
  do
    putStrLn ">>> Load RGL"
    pgf_r <- readPGF "pgfs/LangEng.pgf"
    putStrLn ">>> Load Exemplum"
    pgf_0 <- readPGF $ "pgfs/ExemplumEng.pgf"
    recreateGrammar (Grammar pgf_r []) (fromJust $ readLanguage "LangEng") (Grammar pgf_0 []) exampleCount treeDepth maxSubtreeSize runs
    

compareTreebank :: Grammar -> [(String,[(Language,Tree)])] -> ([(String,Tree)],Double,Double)
compareTreebank = undefined
