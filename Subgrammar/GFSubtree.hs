module Subgrammar.GFSubtree where

import PGF
import Data.List
import Data.Maybe

import Subgrammar.Common

import Control.Monad.LPMonad
import Data.LinearProgram


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

destruct :: Tree -> (String,[Tree])
destruct = maybe ("_",[]) (\(c,ts) -> (showCId c,ts)) . unApp   
                     
data SimpleTree = Empty | Node String [SimpleTree]

instance Show SimpleTree where
  show Empty = "()"
  show (Node n []) = n
  show (Node n ts) = "(" ++ n ++ concatMap show ts ++ ")"

treeToSimpleTree :: Tree -> SimpleTree
treeToSimpleTree t =
  let (n,ts) = destruct t
  in
    Node n (map treeToSimpleTree ts)

-- | Gets the root of a simple tree
getSimpleRoot :: SimpleTree -> String
getSimpleRoot Empty = ""
getSimpleRoot (Node n _) = n


getSimpleSubtrees :: SimpleTree -> [SimpleTree]
getSimpleSubtrees Empty = []
getSimpleSubtrees (Node _ ts) = ts

simpleBfs :: SimpleTree -> [String]
simpleBfs Empty = []
simpleBfs (Node n ts) =
  filter (not . null) $ n:(map getSimpleRoot ts) ++ (concatMap simpleBfs $ concatMap getSimpleSubtrees ts)

-- | Path in a tree
type Path = [Int]

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
forestsToProblem :: [Forest] -> Int -> Problem
forestsToProblem forests size = 
  let
    -- helper to add consequtive numbers
    numbered = zip [1..]
    -- add sentence number to forests
    nForests =  numbered forests
    -- list of all sentences with all their trees
    sentenceTrees = [("s" ++ show sn, ["s" ++ show sn ++ "t" ++ show tn | (tn, t) <- numbered ts])| (sn,ts) <- nForests]
    -- list of all trees with all their rules
    treeRules = concat [[("s" ++ show sn ++ "t" ++ show tn,sizedSubtrees st size
                           -- maxSizeSubtrees st size
                         ) | (tn, t) <- numbered ts, let st = treeToSimpleTree t]| (sn,ts) <- nForests]
    -- List of all sentence variables
    sentences = map fst sentenceTrees
    -- List of all tree variables
    trees = concatMap snd sentenceTrees
    -- List of all rule names
    rules = nub $ map concat $ concat $ concat $ map snd treeRules 
  in
    Problem sentenceTrees rules $
    do
      geqTo (linCombination [(1,s) | s <- sentences]) $ length sentences
      sequence_ [geqTo (linCombination ((-1,s):[(1,t) | t <- ts])) 0 | (s,ts) <- sentenceTrees]
--      sequence_ [geqTo (linCombination ((-(length rs),t):[(1,r) | r <- rs])) 0 | (t,rs) <- treeRules]
      sequence_ $
        [setVarKind s BinVar | s <- sentences] ++
        [setVarKind t BinVar | t <- trees]  ++
        [setVarKind r BinVar | r <- rules]

-- | Test function
test :: IO ()
test = do
  -- load grammar
  putStrLn ">>> Load grammar"
  p <- readPGF "/tmp/Exemplum/Exemplum.pgf"
  let grammar = Grammar p ["/tmp/Exemplum/ExemplumEng.gf"]
  putStrLn $ ">>> Loaded " ++ (show $ length $ functions p) ++ " Rules"
  -- convert examples
  putStrLn ">>> Convert examples to forests"
  let forests = examplesToForests grammar (fromJust $ readLanguage "ExemplumEng") examples
  -- create csp
  putStrLn ">>> Convert forests to CSP"
  let problem = forestsToProblem forests 3
  putStrLn $ ">>> Got problem:\n" -- ++ show problem
  -- solve problem
  putStrLn ">>> Solve the CSP"
  solution <- solve problem numTrees
  putStrLn $ ">>> Got " ++ (show $ length $ snd solution) ++ " rules with a score of " ++ (show $ fst solution) ++ ": \n" ++ show (snd solution)
  -- -- create new grammar
  -- putStrLn ">>> Create New Grammar"
  -- grammar' <- generateGrammar grammar solution
  -- putStrLn $ ">>> Loaded " ++ (show $ length $ functions $ pgf grammar') ++ " Rules"
  -- -- check result
  -- let test = testExamples grammar' (fromJust $ readLanguage "ExemplumSubEng") examples
  -- if (and $ map snd test)  then
  --   putStrLn ">>> Success!!!"
  -- else
  --   putStrLn $ ">>> Failed covering:\n" ++ (unlines $ map fst $ filter (not . snd) test)
  where
    examples = [
      -- "few bad fathers become big",
      -- "now John and Paris aren't good now",
      -- "many cold books come today",
      -- "now Paris and he today don't read few cold mothers",
      -- "it is blue",
      -- "they don't love every mother",
      -- "now it doesn't become blue in John",
      -- "John becomes cold",
      -- "it doesn't come",
      -- "on Paris now Paris comes",
      -- "now the bad cold fathers are big",
      -- "today she doesn't read Paris now now",
      -- "every computer doesn't break many mothers now",
      -- "Paris doesn't switch on it now today now",
      -- "today to it they become good now",
      -- "many fathers today on Paris don't hit many mothers",
      -- "to Paris on it today they don't close her now",
      -- "Paris isn't good today today",
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


  
