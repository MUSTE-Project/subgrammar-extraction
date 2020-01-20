module Subgrammar.GFSubtree where

import PGF
import Data.List
import Data.Maybe

import Subgrammar.Common

import Control.Monad.LPMonad
import Data.LinearProgram
import System.FilePath((</>),(<.>))

-- import Control.Monad (guard)

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

-- | Cuts a tree into root and subtrees
destruct :: Tree -> (String,[Tree])
destruct = maybe ("_",[]) (\(c,ts) -> (showCId c,ts)) . unApp   

-- | Simple tree type
data SimpleTree = Empty | Node String [SimpleTree]

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
getSimpleRoot Empty = ""
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
    map (map simpleBfs) $ map (subtrees' tree) combinations
  where
    subtrees' :: SimpleTree -> [Path] -> [SimpleTree]
    subtrees' tree [] = [tree]
    subtrees' tree (p:ps) =
      let
        (branch,newTree) = deleteBranch tree p
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
    map (map simpleBfs) $ catMaybes $ map (subtrees' tree size)  combinations
  where
    subtrees' :: SimpleTree -> Int -> [Path] -> Maybe [SimpleTree]
    subtrees' tree size []
      | simpleSize tree <= size = Just [tree]
      | otherwise = Nothing
    subtrees' tree msize (p:ps) =
      let
        (branch,newTree) = deleteBranch tree p
      in
        if simpleSize branch <= size then fmap (branch:) (subtrees' newTree size ps) else Nothing

-- | Size of a SimpleTree
simpleSize :: SimpleTree -> Int
simpleSize = length . simpleBfs

-- | Filters all possible subtrees by maximum size
maxSizeSubtrees :: SimpleTree -> Int -> [Subtrees]
maxSizeSubtrees tree size =
  let
    all = allSubtrees tree
  in
    [split | split <- all, maximum (map length split) <= size]
    

{-
Code to just look at all possible subtrees, not just valid segmentations

data PruneOpts = PruneOpts
  { pruneDepth :: Maybe Int
  , pruneSize  :: Maybe Int
  } deriving Show

emptyPruneOpts :: PruneOpts
emptyPruneOpts = PruneOpts Nothing Nothing


splitAndPrune :: PruneOpts -> SimpleTree -> [(SimpleTree, Path, SimpleTree, [SimpleTree])]
splitAndPrune opts base_tree =
    do (adj_path, split_tree) <- splitBaseTree base_tree
       (adj_tree, pruned_children) <- getPrunedTrees opts split_tree
       return (base_tree, adj_path, adj_tree, pruned_children)

splitBaseTree :: SimpleTree -> [(Path, SimpleTree)]
splitBaseTree tree@(Node _ children)
    = ([], tree) : [ (n:path, tree') |
                     (n, child) <- zip [0..] children,
                     (path, tree') <- splitBaseTree child ]
splitBaseTree _ = error "Muste.Prune.splitBaseTree: Non-exhaustive pattern match"


getPrunedTrees :: PruneOpts -> SimpleTree -> [(SimpleTree, [SimpleTree])]
getPrunedTrees (PruneOpts depthLimit sizeLimit) tree 
    = [ (tree, branches) | (tree, branches, _) <- pruneTs tree [] 0 0 ]
    where pruneTs :: SimpleTree -> [SimpleTree] -> Int -> Int -> [(SimpleTree, [SimpleTree], Int)]
          pruneTs tree@(Node fun children) branches depth size 
              = (Empty, tree:branches, size) :
                do guard $ depth `less` depthLimit && size `less` sizeLimit
                   (children', branches', size') <- pruneCs children branches (depth+1) (size+1) 
                   return (Node fun children', branches', size')
          pruneTs tree branches _depth size 
              = [(tree, branches, size)]

          pruneCs :: [SimpleTree] -> [SimpleTree] -> Int -> Int -> [([SimpleTree], [SimpleTree], Int)]
          pruneCs [] branches _depth size = return ([], branches, size)
          pruneCs (tree:trees) branches depth size 
              = do (tree', branches', size') <- pruneTs tree branches depth size 
                   (trees', branches'', size'') <- pruneCs trees branches' depth size' 
                   return (tree':trees', branches'', size'')

          value `less` Just limit = value < limit
          _     `less` Nothing    = True

  
-}
  
-- | Translate a list of forests into a constraint problem given a maximum subtree size
forestsToProblem :: [Forest] -> Int -> ObjectiveFunction [(String, [String])] -> Problem
forestsToProblem forests size (OF fun direction) =
  let
    -- helper to add consequtive numbers
    numbered = zip [1..]
    -- Hierarchy of tags for sentences, trees and rules
    tags =   [(s_tag, [(t_tag,
                        [(p_tag,map (join "#") rs) | (pn,rs) <- numbered (sizedSubtrees (treeToSimpleTree t) size), let p_tag = t_tag ++ "p" ++ show pn]
                       )
                      | (tn,t) <- numbered ts,let t_tag = s_tag ++ "t" ++ show tn]
              )
             | (sn,ts) <- numbered forests, let s_tag = "s" ++ show sn] :: [(String,[(String,[(String,[String])])])]
      -- List of all sentence variables
    sentences = map fst tags 
    -- List of all tree variables
    trees = [t | (s,ts) <- tags, (t,_) <- ts]
    -- List of all partition variables
    partitions = [p | (s,ts) <- tags, (t,ps) <- ts, (p,_) <- ps]
    -- List of all rule names
    rules = [r | (s,ts) <- tags, (t,ps) <- ts, (p,rs) <- ps, r <- rs]
  in
    execLPM $ do
      setDirection direction
      setObjective (fun tags)
      geqTo (linCombination [(1,s) | s <- sentences]) $ length sentences
      sequence_ [geqTo (linCombination ((-1,s):[(1,t) | (t,_) <- ts])) 0 | (s,ts) <- tags]
      sequence_ [geqTo (linCombination ((-1,t):[(1,p) | (p,_) <- ps])) 0 | (s,ts) <- tags,(t,ps) <- ts]
      sequence_ [geqTo (linCombination ((-(length rs),p):[(1,r) | r <- rs])) 0 | (s,ts) <- tags,(t,ps) <- ts,(p,rs) <- ps]
      sequence_ $
        [setVarKind s BinVar | s <- sentences] ++
        [setVarKind t BinVar | t <- trees]  ++
        [setVarKind p BinVar | p <- partitions] ++
        [setVarKind r BinVar | r <- rules]

-- | Test function
test :: IO ()
test = do
  -- load grammar
  putStrLn ">>> Load grammar"
  p <- readPGF $ path_to_exemplum</>"Exemplum.pgf"
  let grammar = Grammar p [path_to_exemplum</>"ExemplumEng.gf"]
  putStrLn $ ">>> Loaded " ++ (show $ length $ functions p) ++ " Rules"
  -- convert examples
  putStrLn ">>> Convert examples to forests"
  let forests = examplesToForests grammar (fromJust $ readLanguage "ExemplumEng") examples
  -- create csp
  putStrLn ">>> Convert forests to CSP"
  let problem = forestsToProblem forests 2 numTrees
  putStrLn $ ">>> Got problem:\n" ++ show problem
  writeLP "/tmp/problem.lp" problem
  -- solve problem
  putStrLn ">>> Solve the CSP"
  solution <- solve problem
  putStrLn $ ">>> Got " ++ (show $ length $ snd solution) ++ " rules with a score of " ++ (show $ fst solution) ++ ": \n" ++ show (snd solution)
  -- create new grammar
  putStrLn ">>> Create New Grammar"
  -- Converting solution
  let splitted = (fst solution,concat [split "#" r|r <- snd solution])
  putStrLn $ ">>> Splitted rules " ++ show splitted
  grammar' <- generateGrammar grammar splitted
  putStrLn $ ">>> Loaded " ++ (show $ length $ functions $ pgf grammar') ++ " Rules"
  -- check result
  let test = testExamples grammar' (fromJust $ readLanguage "ExemplumSubEng") examples
  if (and $ map snd test)  then
    putStrLn ">>> Success!!!"
  else
    putStrLn $ ">>> Failed covering:\n" ++ (unlines $ map fst $ filter (not . snd) test)
  where
    examples = [
      "few bad fathers become big"
      "now John and Paris aren't good now",
      "many cold books come today",
      "now Paris and he today don't read few cold mothers",
      "it is blue",
      "they don't love every mother",
      "now it doesn't become blue in John",
      "John becomes cold",
      "it doesn't come",
      "on Paris now Paris comes",
      "now the bad cold fathers are big",
      "today she doesn't read Paris now now",
      "every computer doesn't break many mothers now",
      "Paris doesn't switch on it now today now",
      "today to it they become good now",
      "many fathers today on Paris don't hit many mothers",
      "to Paris on it today they don't close her now",
      "Paris isn't good today today",
      "it becomes bad already",
      "they don't break her today already today"
      ]
    testExamples :: Grammar -> Language -> [Example] -> [(String,Bool)]
    testExamples g l es = 
      zip es $ map (not.null) $ examplesToForests g l es


test' =
  do
    p <- readPGF "/tmp/Exemplum/Exemplum.pgf"
    let t = head $ parse p (fromJust $ readLanguage "ExemplumEng") (startCat p) "few bad fathers become big"
    return $ treeToSimpleTree t


  
