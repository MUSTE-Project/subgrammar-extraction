module Main where

--import Subgrammar.GFSubtree
import Subgrammar.Experiments
import Test.BenchPress

main :: IO ()
main = -- putStrLn "Hello, Haskell!"
  do
--    bench 10 Subgrammar.GFSubtree.treeTest
    bench 1 $ writeFile "out663.txt" =<< show <$> Subgrammar.Experiments.recreateExemplum 6 6 3
