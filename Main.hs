module Main where

import Subgrammar.GFSubtree
import Subgrammar.Common

main :: IO ()
main = -- putStrLn "Hello, Haskell!"
  do
    _ <- time Subgrammar.GFSubtree.test
    return ()
