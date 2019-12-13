module Subgrammar.GFSubtree where

import PGF
import Data.List
import Debug.Trace
{-
      f
    /  \
   g    h
   |
   i
[
  [[f],[g],[h],[i]]
  [[f],[g,i],[h]]
  [[f,g],[h],[i]]
  [[f,h],[g],[i]]
  [[f,h],[g,i]]
  [[f,g,h],[i]]
  [[f,g,i],[h]]

([],[],[(f (g i) h)])
([],[f],[(g i),h])
-}
testTree =
  mkApp (mkCId "f") [mkApp (mkCId "g") [mkApp (mkCId "i") []],mkApp (mkCId "h") []]

t2 = mkApp (mkCId "f") [mkApp (mkCId "g") [],mkApp (mkCId "h") []]
type Subtree = [String]
type Subtrees = [Subtree]

treeToSubtrees :: Tree -> Int -> [Subtrees]
treeToSubtrees tree depth = undefined
--  subsequences $ treeToList tree
  -- nub $ map sort $ treeToSubtrees' depth [root] children
  -- where
  --   (root,children) = decompose tree
-- treeToSubtrees' :: Int -> Subtree -> [Tree] -> [Subtrees]
-- treeToSubtrees' depth subtree [] = [[subtree]]
-- treeToSubtrees' depth subtree trees
-- --  | length subtree == depth = concatMap (map (subtree:)) [ treeToSubtrees' depth [c] (updateTrees trees t nts)| t <- trees, let (c,nts) = decompose t]
--   | otherwise =
--   concat [ treeToSubtrees' depth (c:subtree) uts -- ++ map (subtree:) (treeToSubtrees' depth [c] uts)
--          | t <- trees, let (c,nts) = decompose t, let uts = (updateTrees trees t nts)]
  -- ++
  -- concatMap (map (subtree:)) [ treeToSubtrees' depth [c] (updateTrees trees t nts)| t <- trees, let (c,nts) = decompose t]
-- treeToSubtrees' depth subtree trees
--   | null trees  = [[reverse subtree]]
--   | length subtree == depth =
--     treeToSubtrees'' depth subtree trees
--   | otherwise =
--     let sts = [(c:subtree,updateTrees trees t nts)| t <- trees, let (c,nts) = decompose t] in
--       treeToSubtrees'' depth subtree trees ++
--       concatMap (\(c,ts) -> treeToSubtrees' 3 c ts) sts              
--           -- concat [ treeToSubtrees' depth (c:subtree) (updateTrees trees t nts)| t <- trees, let (c,nts) = decompose t]
--   where
--     treeToSubtrees'' depth subtree trees =
--       concatMap (map ((reverse subtree):))
--       [treeToSubtrees' depth [c] (updateTrees trees t nts)| t <- trees, let (c,nts) = decompose t]
updateTrees (t:ts) t' nts
  | t == t' = nts ++ ts
  | otherwise = t:(updateTrees ts t' nts)
updateTrees [] _ _ = []

destruct :: Tree -> (String,[Tree])
destruct = maybe ("_",[]) (\(c,ts) -> (showCId c,ts)) . unApp   
                     
dfs :: Tree -> [String]
dfs t = c:(concat [dfs t' | t' <- ts])
  where
    (c,ts) = destruct t

bfs :: Tree -> [String]
bfs t = 
  [c] ++ map fst dcs ++ (concatMap (concatMap bfs . snd) dcs)
    where
    (c,ts) = destruct t
    dcs = [(c',ts') | t' <- ts, let (c',ts') = destruct t']

subtrees :: ([[String]],[String],[Tree]) -> [[[String]]]
subtrees p = -- trace (show p) $
  subtrees' p
subtrees' ([],[],[]) = []
subtrees' (sts,[],[]) = [sts]
subtrees' (sts,st,[]) = subtrees (sts ++ [st],[],[])
subtrees' ([], [],(t:ts)) =
  let (c,ts') = destruct t in
    subtrees ([],[c],ts') ++ subtrees ([],[],ts)
subtrees' (sts,[],(t:ts)) =
  subtrees (sts,[c],(ts'++ts))
  where
    (c,ts') = destruct t
subtrees' (sts,st,(t:ts)) =
  subtrees (sts ++ [st],[],(t:ts)) ++
  subtrees (sts,st ++ [c],ts'++ts)
  where (c,ts') = destruct t
    
f :: Tree -> [[[String]]]

f' :: [Tree] ->
f' (t:ts) = [(t':ts´) | 
