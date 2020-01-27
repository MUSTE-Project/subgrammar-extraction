module Main where

import Subgrammar.GFSubtree
import Test.BenchPress

main :: IO ()
main = -- putStrLn "Hello, Haskell!"
  do
    bench 10 Subgrammar.GFSubtree.treeTest
