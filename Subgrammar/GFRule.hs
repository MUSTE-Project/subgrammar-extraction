module Subgrammar.GFRule where

import PGF
import Data.Maybe

type Example = String
type Forest = [Tree]
type Grammar = PGF
data Formula = Var String | Conj [Formula] | Disj [Formula] | Neg Formula | Imp Formula Formula
type Solution = [String]

exampleToForest :: Grammar -> Example -> Forest
exampleToForest grammar example =
  concatMap snd $ parseAllLang grammar (startCat grammar) example

forestToFormula :: Forest -> Formula
forestToFormula forest =
  Disj [Conj (map Var $ treeToRules t) | t <- forest ]
  where
    treeToRules :: Tree -> [String]
    treeToRules tree = maybe [] (\(f,ts) -> showCId f:(concatMap treeToRules ts)) $ unApp tree

combineFormulas :: [Formula] -> Formula
combineFormulas = Conj

solve :: Formula -> Solution
solve = undefined

generateGrammar :: Solution -> Grammar
generateGrammar = undefined
