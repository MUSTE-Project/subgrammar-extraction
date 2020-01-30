module Subgrammar.GFSubtree where

import PGF
import Data.List
import Data.Maybe

import Subgrammar.Common

import Control.Monad.LPMonad
import Data.LinearProgram hiding ((-))
import System.FilePath((</>))

-- import Control.Monad (guard)

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

{- | Taken from MissingH:Data.String.Utils:
Given a delimiter and a list of items (or strings), join the items
by using the delimiter.

Example:

> join "|" ["foo", "bar", "baz"] -> "foo|bar|baz"
-}
join :: [a] -> [[a]] -> [a]
join delim l = concat (intersperse delim l)

-- | Splits a list at a delimiter element
split :: Eq a => [a] -> [a] -> [[a]]
split delim l =
  split' l []
  where
    split' [] [] = []
    split' [] acc = [reverse acc]
    split' l'@(hd:tl) acc
      | isPrefixOf delim l' = (reverse acc):(split' (drop (length delim) l') [])
      | otherwise = split' tl (hd:acc)

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
getSimpleRoot Empty = "@"
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
    map (map simpleBfs) $ catMaybes $ map (subtrees' tree)  combinations
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
  let l = simpleBfs t
  in
    length l - (length $ filter (=="@") l)

-- | Filters all possible subtrees by maximum size
maxSizeSubtrees :: SimpleTree -> Int -> [Subtrees]
maxSizeSubtrees tree size =
  let
    allTrees = allSubtrees tree
  in
    [splitted | splitted <- allTrees, maximum (map (length . filter (/="@")) splitted) <= size]
    

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
forestsToProblem forests size (OF f dir) =
  let
    -- helper to add consequtive numbers
    numbered :: [a] -> [(Int,a)]
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
    trees = [t | (_,ts) <- tags, (t,_) <- ts]
    -- List of all partition variables
    partitions = [p | (_,ts) <- tags, (_,ps) <- ts, (p,_) <- ps]
    -- List of all rule names
    rules = [r | (_,ts) <- tags, (_,ps) <- ts, (_,rs) <- ps, r <- rs]
  in
    execLPM $ do
      setDirection dir
      setObjective (f tags)
      geqTo (linCombination [(1,s) | s <- sentences]) $ length sentences
      sequence_ [geqTo (linCombination ((-1,s):[(1,t) | (t,_) <- ts])) 0 | (s,ts) <- tags]
      sequence_ [geqTo (linCombination ((-1,t):[(1,p) | (p,_) <- ps])) 0 | (_,ts) <- tags,(t,ps) <- ts]
      sequence_ [geqTo (linCombination ((-(length rs),p):[(1,r) | r <- rs])) 0 | (_,ts) <- tags,(_,ps) <- ts,(p,rs) <- ps]
      sequence_ $
        [setVarKind s BinVar | s <- sentences] ++
        [setVarKind t BinVar | t <- trees]  ++
        [setVarKind p BinVar | p <- partitions] ++
        [setVarKind r BinVar | r <- rules]

-- | Test function
test :: IO ()
test = do
  --sequence_ [(\i -> putStrLn $ "!!!> " ++ show [e]    ++ " " ++ show s ++ " " ++ show i) =<< show <$> time (test' s [e])    | e <- [0..2]++[4..9], s <- [2,3]]
  -- sequence_ [(\i -> putStrLn $ "!!!> " ++ show (take e l) ++ " " ++ show s ++ " " ++ show i) =<< show <$> time (test' s (take e l)) | let l = [0,2,4,5,6,7,8,9], e <- [1..length l], s <- [2,3]]
  putStrLn =<< (show <$> time (test' 3 [0,2,4,5,7,8,9])) -- potetially 6
  where
    test' :: Int -> [Int] -> IO ()
    test' maxSize exampleNos =
      do
        -- load grammar
        putStrLn ">>> Load grammar"
        p <- readPGF $ path_to_exemplum</>"Exemplum.pgf"
        let grammar = Grammar p [path_to_exemplum</>"ExemplumEng.gf"]
        putStrLn $ ">>> Loaded " ++ (show $ length $ functions p) ++ " Rules"
        -- convert examples
        putStrLn ">>> Convert examples to forests"
        let forests = examplesToForests grammar (fromJust $ readLanguage "ExemplumEng") (map (exampleSentences !!) exampleNos)
        -- putStrLn $ ">>> Forrest:\n" ++ (unlines $ map show $ map (map (\t -> (simpleSize $ treeToSimpleTree t,t))) forests)
        --  putStrLn $ ">>> Tree sizes:" ++ (show $ map (map (\t -> let st = treeToSimpleTree t in (simpleSize st, length $ sizedSubtrees st maxSize))) forests)
        -- create csp
        putStrLn ">>> Convert forests to CSP"
        let problem = forestsToProblem forests maxSize numTrees
        --  putStrLn $ ">>> Got problem:\n" ++ show problem
        writeLP ("/tmp/problem-tmp" ++ concatMap show exampleNos ++ "-" ++ show maxSize ++ ".lp") problem
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
        let testResults = testExamples grammar' (fromJust $ readLanguage "ExemplumSubEng") (map (exampleSentences !!) exampleNos)
        if (and $ map snd testResults)  then
          putStrLn ">>> Success!!!"
        else
          putStrLn $ ">>> Failed covering:\n" ++ (unlines $ map fst $ filter (not . snd) testResults)
    testExamples :: Grammar -> Language -> [Example] -> [(String,Bool)]
    testExamples g l es = 
      zip es $ map (not.null) $ examplesToForests g l es

treeTest :: IO ()
treeTest = do
  let maxSize = 3
  -- create csp
  putStrLn ">>> Convert forests to CSP"
  let problem = forestsToProblem (map (map (fromJust . readExpr)) exampleTrees) maxSize numTrees
  -- solve problem
  putStrLn ">>> Solve the CSP"
  solution <- solve problem
  putStrLn $ ">>> Got " ++ (show $ length $ snd solution) ++ " rules with a score of " ++ (show $ fst solution) ++ ": \n" ++ show (snd solution)
  -- check the solution
  let splittedSolution = nub $ concat [split "#" r|r <- snd solution]
  let splittedTrees = nub $ words $ filter (\c -> not $ c `elem` "()") $ join " " $ concat exampleTrees
  let missing = splittedTrees \\ splittedSolution
  if (null missing)  then
    putStrLn ">>> Success!!!"
  else
    do 
      putStrLn $ ">>> Failed covering: " ++ (unwords missing)
  
exampleSentences :: [String]
exampleSentences = [
  "few bad fathers become big",                                       -- 1
  "now John and Paris aren't good now",                               -- 2
  "many cold books come today",                                       -- 3
  "now Paris and he today don't read few cold mothers",               -- 4
  "it is blue",                                                       -- 5
  "they don't love every mother",                                     -- 6
  "now it doesn't become blue in John",                               -- 7
  "John becomes cold",                                                -- 8
  "it doesn't come",                                                  -- 9
  "on Paris now Paris comes",                                         -- 10
  "now the bad cold fathers are big",                                 -- 11
  "today she doesn't read Paris now now",                             -- 12
  "every computer doesn't break many mothers now",                    -- 13
  "Paris doesn't switch on it now today now",                         -- 14
  "today to it they become good now",                                 -- 15
  "many fathers today on Paris don't hit many mothers",               -- 16
  "to Paris on it today they don't close her now",                    -- 17
  "Paris isn't good today today",                                     -- 18
  "it becomes bad already",                                           -- 19
  "they don't break her today already today"                          -- 20
  ]


exampleTrees :: [[String]]
exampleTrees =
  [
    [ "UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN few_Det (AdjCN (PositA bad_A) (UseN2 father_N2))) (ComplVA become_VA (PositA big_A))))" ],
    [ "UttS (AdvS now_Adv (UseCl (TTAnt TPres ASimul) PNeg (PredVP (ConjNP and_Conj (BaseNP (UsePN john_PN) (UsePN paris_PN))) (AdvVP (UseComp (CompAP (PositA good_A))) now_Adv))))" ],
    [ "UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (DetCN many_Det (AdjCN (PositA cold_A) (UseN book_N))) (AdvVP (UseV come_V) today_Adv)))" ],
    [ "UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron it_Pron) (UseComp (CompAP (PositA blue_A)))))" ],
    [ "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron they_Pron) (ComplSlash (SlashV2a love_V2) (DetCN every_Det (UseN2 mother_N2)))))" ],
    [ "UttS (AdvS now_Adv (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron it_Pron) (AdvVP (ComplVA become_VA (PositA blue_A)) (PrepNP in_Prep (UsePN john_PN))))))" ],
    [ "UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePN john_PN) (ComplVA become_VA (PositA cold_A))))" ],
    [ "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron it_Pron) (UseV come_V)))" ],
    [
      "UttS (AdvS (PrepNP on_Prep (UsePN paris_PN)) (AdvS now_Adv (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePN paris_PN) (UseV come_V)))))",
      "UttS (AdvS (PrepNP on_Prep (AdvNP (UsePN paris_PN) now_Adv)) (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePN paris_PN) (UseV come_V))))"
    ]
    -- -- [
    -- --   "UttS (AdvS today_Adv (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron she_Pron) (AdvVP (AdvVP (ComplSlash (SlashV2a read_V2) (UsePN paris_PN)) now_Adv) now_Adv))))",
    -- --   "UttS (AdvS today_Adv (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron she_Pron) (AdvVP (ComplSlash (SlashV2a read_V2) (AdvNP (UsePN paris_PN) now_Adv)) now_Adv))))",
    -- --   "UttS (AdvS today_Adv (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron she_Pron) (ComplSlash (SlashV2a read_V2) (AdvNP (AdvNP (UsePN paris_PN) now_Adv) now_Adv)))))"
    -- -- ],
    -- -- [
    -- --   "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (DetCN every_Det (UseN computer_N)) (AdvVP (ComplSlash (SlashV2a break_V2) (DetCN many_Det (UseN2 mother_N2))) now_Adv)))",
    -- --   "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (DetCN every_Det (UseN computer_N)) (ComplSlash (SlashV2a break_V2) (AdvNP (DetCN many_Det (UseN2 mother_N2)) now_Adv))))"
    -- -- ],
    -- -- [
    -- --   "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePN paris_PN) (AdvVP (AdvVP (AdvVP (ComplSlash (SlashV2a switch8on_V2) (UsePron it_Pron)) now_Adv) today_Adv) now_Adv)))",
    -- --   "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePN paris_PN) (AdvVP (AdvVP (ComplSlash (SlashV2a switch8on_V2) (AdvNP (UsePron it_Pron) now_Adv)) today_Adv) now_Adv)))",
    -- --   "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePN paris_PN) (AdvVP (ComplSlash (SlashV2a switch8on_V2) (AdvNP (AdvNP (UsePron it_Pron) now_Adv) today_Adv)) now_Adv)))",
    -- --   "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePN paris_PN) (ComplSlash (SlashV2a switch8on_V2) (AdvNP (AdvNP (AdvNP (UsePron it_Pron) now_Adv) today_Adv) now_Adv))))"
    -- -- ],
    -- -- [ "UttS (AdvS today_Adv (AdvS (PrepNP to_Prep (UsePron it_Pron)) (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron they_Pron) (AdvVP (ComplVA become_VA (PositA good_A)) now_Adv)))))"],
    -- -- [ "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePN paris_PN) (AdvVP (AdvVP (UseComp (CompAP (PositA good_A))) today_Adv) today_Adv)))"],
    -- -- [ "UttS (UseCl (TTAnt TPres ASimul) PPos (PredVP (UsePron it_Pron) (AdvVP (ComplVA become_VA (PositA bad_A)) already_Adv)))"],
    -- -- [
    -- --   "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron they_Pron) (AdvVP (AdvVP (AdvVP (ComplSlash (SlashV2a break_V2) (UsePron she_Pron)) today_Adv) already_Adv) today_Adv)))",
    -- --   "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron they_Pron) (AdvVP (AdvVP (ComplSlash (SlashV2a break_V2) (AdvNP (UsePron she_Pron) today_Adv)) already_Adv) today_Adv)))",
    -- --   "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron they_Pron) (AdvVP (ComplSlash (SlashV2a break_V2) (AdvNP (AdvNP (UsePron she_Pron) today_Adv) already_Adv)) today_Adv)))",
    -- --   "UttS (UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron they_Pron) (ComplSlash (SlashV2a break_V2) (AdvNP (AdvNP (AdvNP (UsePron she_Pron) today_Adv) already_Adv) today_Adv))))"
    -- -- ]
  ]
{- Stats:
Interpreted
              | Size 2 |                            | Size 3 |
No Sentences  | Time   | Size/No Subtrees           | Time   | Size/No Subtrees
1 (1)         | 13s    | [(18,2100)]                | 18s    | [[(18,18658)]]
1 (2)         | 238s   | [(22,13476)]               | 347s   | [[(22,202183)]]
1 (3)         | 12s    | [(18,2100)]                | 19s    | [[(18,18658)]]
1 (4)         | ?      | [(28,202944),(28,201120)]] | ?      | ?
1 (5)         | 1s     | [(13,240)]                 | 1s     | [[(13,1166)]]
1 (6)         | 4s     | [(16,884)]                 | 5s     | [[(16,6016)]]
1 (7)         | 56s    | [(20,5250)]                | 79s   | [[(20,58551)]]
1 (8)         | 1s     | [(13,220)]                 | 1s     | [[(13,1066)]]
1 (9)         | 1s     | [(11,92)]                  | 1s     | [[(11,341)]]
1 (10)        | 26s    | [(18,2260),(18,2260)]      | 37s    | [[(18,17769),(18,17769)]]
1 (11)        | ?      | [25]
1 (12)        | ?      | [20,20,20]
1 (13)        | ?      | [20,20]
1 (14)        | ?      | [20,20,20,20]
1 (15)        | ?      | [22]
1 (16)        | ?      | [25]
1 (17)        | ?      | [28,28,28,28,28,28,28,28,28,28]
1 (18)        | ?      | [17]
1 (19)        | ?      | [15]
1 (20)        | ?      | [20,20,20,20]

Compiled (heap size 8GB)
1 (1)                 | "19" |"23"
1 (2)                 | "49" |"108"
1 (3)                 | "19" | "24"
1 (5)                 | "18" | "18"
1 (6)                 | "20" | "20"
1 (7)                 | "25" | "38"
1 (8)                 | "20" | "20"
1 (9)                 | "18" | "18"
1 (10)                | "21" | "30"

1 (1)                 | 18s | 25s
2 (1,3)               | 21s | 28s
3 (1,3,5)             | 22s | 29s
4 (1,3,5,6)           | 24s | 35s
5 (1,3,5,6,7)         | 34s | 64s
6 (1,3,5,6,7,8)       | 36s | 79s
7 (1,3,5,6,7,8,9)     | 33s | 77s
8 (1,3,5,6,7,8,9,10)  | 36s | 101s
7 (1,3,5,7,8,9,10)    | 27s | 48s
8 (1,3,5,6,7,8,9,10)  | 38s | 99s
-}
