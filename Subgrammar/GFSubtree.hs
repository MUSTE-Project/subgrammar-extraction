module Subgrammar.GFSubtree
  ( forestsToProblem
  , numRules
  , numRulesTrees
  , weightedRules
  ) where

{-
Subgrammar extraction based on subtree optimization
-}
import PGF
import Data.List
import Data.Maybe

import Subgrammar.Common

import Control.Monad.LPMonad
import Data.LinearProgram (linCombination,ObjectiveFunc,Direction(..),writeLP,VarKind(BinVar))
import qualified Data.Map.Lazy as M

import Control.Monad (guard,when)

import System.IO.Unsafe (unsafePerformIO)
import Text.Printf (printf)

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


testTree :: Tree
testTree =
  mkApp (mkCId "f") [mkApp (mkCId "g") [mkApp (mkCId "i") []],mkApp (mkCId "h") []]

type Subtree = [String]
type Subtrees = [Subtree]

-- | Cuts a tree into root and subtrees
destruct :: Tree -> (String,[Tree])
destruct = maybe ("_",[]) (\(c,ts) -> (showCId c,ts)) . unApp   

-- | Simple tree type
data SimpleTree = Empty | Node String [SimpleTree] deriving Eq

instance Show SimpleTree where
  show Empty = "()"
  show (Node n []) = n
  show (Node n ts) = "(" ++ n ++ concatMap show ts ++ ")"

-- | Converts a GF tree into a SimpleTree
treeToSimpleTree :: Tree -> SimpleTree
treeToSimpleTree t =
  let (n,ts) = destruct t
  in
    Node n (map treeToSimpleTree ts)

-- | Gets the root of a simple tree
getSimpleRoot :: SimpleTree -> String
getSimpleRoot Empty = hole
getSimpleRoot (Node n _) = n

-- | Gets the subtrees of a simple tree
getSimpleSubtrees :: SimpleTree -> [SimpleTree]
getSimpleSubtrees Empty = []
getSimpleSubtrees (Node _ ts) = ts

-- | Breadth-first enumeration of all nodes
simpleBfs :: SimpleTree -> [String]
simpleBfs Empty = []
simpleBfs (Node n ts) =
  filter (not . null) $ n:(map getSimpleRoot ts) ++ (concatMap simpleBfs $ concatMap getSimpleSubtrees ts)

-- | Depth-first enumeration of all nodes
simpleDfs :: SimpleTree -> [String]
simpleDfs Empty = []
simpleDfs (Node n ts) =
  filter (not . null) $ n:(concatMap simpleDfs ts)

-- | Path in a tree
type Path = [Int]

-- | Gets all the pathes in a simple tree
getAllPathes :: SimpleTree -> [Path]
getAllPathes t =
  let
    pathes Empty = []
    pathes (Node _ []) = []
    pathes (Node _ ts) =
      let zips = zip [0..] ts in
      [[c]|(c,_) <- zips] ++ concatMap (\(p,c) -> map (p:) $ pathes c) zips
  in
    pathes t

-- | Removes a branch at a given path and returns both the removed subtree and the new tree
deleteBranch :: SimpleTree -> Path -> (SimpleTree,SimpleTree)
-- with empty tree do nothing
deleteBranch Empty _ = (Empty,Empty)
-- walk down the path
-- End of the path
deleteBranch oldTree@(Node n trees) [pos]
  | pos >= 0 && pos < length trees =  -- subtree must exist
    let
      subTree = trees !! pos
    in
      (subTree,Node n (trees !!= (pos,Empty)))
  | otherwise = (Empty,oldTree) -- if branch does not exist just do nothing
deleteBranch oldTree@(Node n trees) (pos:ps)
  | pos >= 0 && pos < length trees =  -- subtree must exist
    let
      subTree = trees !! pos
      (branch,newTree) = deleteBranch subTree ps
    in
      (branch,Node n (trees !!= (pos,newTree)))
  | otherwise = (Empty,oldTree) -- if branch does not exist just do nothing
deleteBranch oldTree [] =
  (Empty,oldTree) -- at empty path do nothing

-- | Replaces a list item at a certain index
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (pos,el) =
  let 
  (pre,post) = splitAt pos l
  in
    pre ++ el:(tail post)

-- | Computes all subtrees of a simple tree
allSubtrees :: SimpleTree -> [Subtrees]
allSubtrees tree =
  let
    pathes = getAllPathes tree
    -- get all subsets and sort by longest path first
    combinations = map (sortBy (\a b -> compare (length b) (length a))) $ subsequences pathes
  in
    map (map simpleDfs) $ map (subtrees' tree) combinations
  where
    subtrees' :: SimpleTree -> [Path] -> [SimpleTree]
    subtrees' tree' [] = [tree']
    subtrees' tree' (p:ps) =
      let
        (branch,newTree) = deleteBranch tree' p
      in
        branch:subtrees' newTree ps

-- | Only collects subtrees up to a certain size
sizedSubtrees :: SimpleTree -> Int -> [Subtrees]
sizedSubtrees tree size =
  let
    pathes = getAllPathes tree
    -- get all subsets and sort by longest path first
    combinations = map (sortBy (\a b -> compare (length b) (length a))) $ subsequences pathes
  in
    map (map simpleDfs) $ catMaybes $ map (subtrees' tree)  combinations
  where
    subtrees' :: SimpleTree -> [Path] -> Maybe [SimpleTree]
    subtrees' tree' []
      | simpleSize tree' <= size = Just [tree']
      | otherwise = Nothing
    subtrees' tree' (p:ps) =
      let
        (branch,newTree) = deleteBranch tree' p
      in
        if simpleSize branch <= size then fmap (branch:) (subtrees' newTree ps) else Nothing

-- | Size of a SimpleTree
simpleSize :: SimpleTree -> Int
simpleSize t =
  let l = simpleDfs t
  in
    length l - (length $ filter (== hole) l)

-- | Filters all possible subtrees by maximum size
maxSizeSubtrees :: SimpleTree -> Int -> [Subtrees]
maxSizeSubtrees tree size =
  let
    allTrees = allSubtrees tree
  in
    [splitted | splitted <- allTrees, maximum (map (length . filter (/=hole)) splitted) <= size]



-- | Translate a list of forests into a constraint problem given a maximum subtree size
forestsToProblem :: Int -> (Maybe Int) -> [Forest] -> [Forest] -> ObjectiveFunction [(String, [String])] -> Problem
forestsToProblem size mergedPerTree positive_forests negative_forests (OF f dir) =
  let
    -- helper to add consequtive numbers
    numbered :: [a] -> [(Int,a)]
    numbered = zip [1..]
    -- Hierarchy of tags for sentences, trees and rules
    positive_tags = [(s_tag, [(t_tag, [(p_tag, map (join "#") rs) |
                              (pn,rs) <- numbered (sizedSubtreesByChopping size mergedPerTree (treeToSimpleTree t)),
                              let p_tag = t_tag ++ "p" ++ show pn])
                    | (tn,t) <- numbered ts,
                      let t_tag = s_tag ++ "t" ++ show tn])
           | (sn,ts) <- numbered positive_forests,
             let s_tag = "s" ++ show sn]
           :: [(String,[(String,[(String,[String])])])]
    -- List of all sentence variables
    positive_sentences = map fst positive_tags 
    -- List of all tree variables
    positive_trees = [t | (_,ts) <- positive_tags, (t,_) <- ts]
    negative_trees = [map (join "#") rs | ts <- negative_forests,t <- ts, rs <- sizedSubtreesByChopping size mergedPerTree (treeToSimpleTree t)] :: [[String]]
    -- List of all partition variables
    positive_partitions = [p | (_,ts) <- positive_tags, (_,ps) <- ts, (p,_) <- ps]
    -- List of all rule names
    positive_rules = [r | (_,ts) <- positive_tags, (_,ps) <- ts, (_,rs) <- ps, r <- rs]
    negative_rules = [rs | ps <- negative_trees, rs <- ps]
  in
   let
    problem :: Problem
    problem = 
     execLPM $ do
      setDirection dir
      setObjective (f positive_tags)
      geqTo (linCombination [(1,s) | s <- positive_sentences]) $ length positive_sentences
      sequence_ [geqTo (linCombination ((-1,s):[(1,t) | (t,_) <- ts])) 0 | (s,ts) <- positive_tags]
      sequence_ [geqTo (linCombination ((-1,t):[(1,p) | (p,_) <- ps])) 0 | (_,ts) <- positive_tags,(t,ps) <- ts]
      sequence_ [geqTo (linCombination ((-(length rs),p):[(1,r) | r <- rs])) 0 | (_,ts) <- positive_tags,(_,ps) <- ts,(p,rs) <- ps]
      sequence_ [leqTo (linCombination ([(1,r) | r <- rs])) (length rs - 1) | rs <- negative_trees ]
      sequence_ $
        [setVarKind s BinVar | s <- positive_sentences] ++
        [setVarKind t BinVar | t <- positive_trees]  ++
        [setVarKind p BinVar | p <- positive_partitions] ++
        [setVarKind r BinVar | r <- nub (positive_rules ++ negative_rules)]
    printstat :: IO ()
    printstat =
      do let ntake = 10
         putStrLn $ "--->"
         let ss' = [(s, length ts) | (s, ts) <- positive_tags]
         let ts' = [(t, length ps) | (_, ts) <- positive_tags, (t, ps) <- ts]
         let ps' = [(p, length rs) | (_, ts) <- positive_tags, (_, ps) <- ts, (p, rs) <- ps]
         printf "Sents:  %4d   Trees/sent: %s...\n" (length ss') (show (take ntake ss'))
         printf "Trees:  %4d   Parts/tree: %s...\n" (length ts') (show (take ntake ts'))
         printf "Parts:  %4d   Rules/part: %s...\n" (length ps') (show (take ntake ps'))
         let groupedRules = sort [(length g, r) | g@(r:_) <- group (sort positive_rules)]
         let mergedRules = [nr | nr@(_,r) <- groupedRules, '#' `elem` r]
         printf "Rules:  %4d\n" (length groupedRules)
         printf "Merged: %4d\n" (length mergedRules)
         putStrLn $ "Occurrences/merged rule:"
         let grules = if length mergedRules <= 2*ntake then mergedRules
                      else take ntake mergedRules ++ [(0, "...")] ++ reverse (take ntake (reverse mergedRules))
         mapM_ (\(n,r) -> printf "    %5d  %s\n" n r) grules
         putStrLn $ "<---"
   in unsafePerformIO (do { when debug printstat ; return problem } )

-- | Objective function to minimize the number of rules
numRules :: ObjectiveFunction [(String,[String])]
numRules = OF numRulesOF Min
  where
    numRulesOF :: [(String,[(String,[(String,[String])])])] -> ObjectiveFunc String Int
    numRulesOF tags = linCombination $ nub [(1,r) | (_,ts) <- tags,(_,sts) <- ts,(_,rs) <- sts, r <- rs]

-- | Objective function to minimize the sum of rules and trees
numRulesTrees :: ObjectiveFunction [(String,[String])]
numRulesTrees = OF numRulesOF Min
  where
    numRulesOF :: [(String,[(String,[(String,[String])])])] -> ObjectiveFunc String Int
    numRulesOF tags = linCombination $ nub [(1,r) | (_,ts) <- tags,(_,sts) <- ts,(_,rs) <- sts, r <- rs] ++ nub [(1,t) | (_,ts) <- tags,(t,_) <- ts]

-- | Objective function to minimize the sum of all rules weighted by number of occurences
weightedRules :: ObjectiveFunction [(String,[String])]
weightedRules = OF numRulesOF Min
  where
    numRulesOF :: [(String,[(String,[(String,[String])])])] -> ObjectiveFunc String Int
    numRulesOF tags =
      let
        ruleVars = [r | (_,ts) <- tags,(_,sts) <- ts,(_,rs) <- sts, r <- rs]
        ruleFreq = Prelude.foldl (\m k -> M.alter (maybe (Just 1) (\n -> Just (n + 1))) k m) M.empty $ ruleVars
        ruleCount = length $ nub ruleVars
      in
        linCombination $ nub [(round ((fromIntegral (ruleFreq M.! r) / fromIntegral ruleCount) * 100) ,r) | r <- ruleVars]

-- | Computes all subtrees (up to a given size), optimized version
sizedSubtreesByChopping :: Int -> Maybe Int -> SimpleTree -> [Subtrees]
sizedSubtreesByChopping sizeLimit maxMergedPerTree tree 
  = map (map simpleDfs) $ chopTreeIntoBitsAndPieces sizeLimit maxMergedPerTree tree


chopTreeIntoBitsAndPieces :: Int -> Maybe Int -> SimpleTree -> [[SimpleTree]]
chopTreeIntoBitsAndPieces sizeLimit maxMerged tree = [subtrees | (_, subtrees) <- chopTree tree]
  where 
    chopTree :: SimpleTree -> [(Int, [SimpleTree])]
    chopTree tree = 
      do (subtree, children, size) <- getPrunedTrees tree
         guard (subtree /= Empty)
         (merged, subtrees) <- chopChildren children
         let merged' = if size == 1 then merged else merged + 1
         guard (isNothing maxMerged || Just merged' <= maxMerged)
         return (merged', subtree : subtrees)

    chopChildren :: [SimpleTree] -> [(Int, [SimpleTree])]
    chopChildren [] = return (0, [])
    chopChildren (tree : trees) = 
      do (merged, subtrees) <- chopTree tree
         (merged', subtrees') <- chopChildren trees
         let merged'' = merged + merged'
         guard (isNothing maxMerged || Just merged'' <= maxMerged)
         return (merged'', subtrees ++ subtrees')

    getPrunedTrees :: SimpleTree -> [(SimpleTree, [SimpleTree], Int)]
    getPrunedTrees tree = pruneTs tree [] 0

    pruneTs :: SimpleTree -> [SimpleTree] -> Int -> [(SimpleTree, [SimpleTree], Int)]
    pruneTs tree@(Node root children) branches size 
      = (Empty, tree:branches, size) :
        do guard $ size < sizeLimit
           (children', branches', size') <- pruneCs children branches (size+1) 
           return (Node root children', branches', size')
    pruneTs tree branches size 
      = [(tree, branches, size)]

    pruneCs :: [SimpleTree] -> [SimpleTree] -> Int -> [([SimpleTree], [SimpleTree], Int)]
    pruneCs [] branches size = return ([], branches, size)
    pruneCs (tree:trees) branches size 
      = do (tree', branches', size') <- pruneTs tree branches size 
           (trees', branches'', size'') <- pruneCs trees branches' size' 
           return (tree':trees', branches'', size'')

