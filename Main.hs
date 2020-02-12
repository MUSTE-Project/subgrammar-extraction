module Main where

import Subgrammar.GFSubtree
import Subgrammar.Experiments
import Test.BenchPress

main :: IO ()
main = -- putStrLn "Hello, Haskell!"
  do
--    bench 1 $ Subgrammar.GFSubtree.treeTest
    bench 1 $ Subgrammar.Experiments.recreateExemplum "experiment.csv"
