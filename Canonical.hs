{- (c) Thomas Hallgren 2019 -}
{- (c) Herbert Lange 2020 -}
module Canonical where

import System.FilePath((</>),(<.>))
import System.Directory(createDirectoryIfMissing)
import GF.Support(writeUTF8File)
import GF.Text.Pretty(render80)
import qualified GF
import GF.Grammar.Canonical -- (Grammar(..),ModId(..),abstrName,concName,Abstract)
import Data.List(stripPrefix,nub,intersperse)
import Data.Maybe

type CanonicalGrammar = GF.Grammar.Canonical.Grammar

-- Original code Written by Thomas Hallgren
-- (https://github.com/MUSTE-Project/gf-canonical-transforms)

-- | Write a canonical grammar in GF source files that can be compiled with GF.
-- The 'FilePath' argument is the name of a subdirectory where the files will
-- be written.

-- | Write a grammar to file
writeGrammar :: FilePath -> CanonicalGrammar -> IO ()
writeGrammar prefix (Grammar absGram cncs) =
  do createDirectoryIfMissing False prefix
     writeUTF8File (absPath absGram) (render80 absGram)
     sequence_ [writeUTF8File (cncPath cnc) (render80 cnc) | cnc<-cncs]
  where
    absPath = gfpath . abstrName 
    cncPath = gfpath . concName

    gfpath (ModId s) = prefix</>s<.>"gf"

-- | Rename a canonical grammar, i.e. both the abstract and the concrete syntax
renameGrammar :: String -> CanonicalGrammar -> CanonicalGrammar
renameGrammar newAbs (Grammar absGram cncs) =
    Grammar (renameAbs absGram) (map renameCnc cncs)
  where
    renameAbs (Abstract _ fls cs fs) = Abstract (ModId newAbs) fls cs fs

    renameCnc (Concrete (ModId oldCnc) (ModId oldAbs) fls ps lcs ls) =
        Concrete (ModId newCnc) (ModId newAbs) fls ps lcs ls
      where
        newCnc = maybe oldCnc (newAbs++) (stripPrefix oldAbs oldCnc)

-- Extensions by Herbert Lange

-- | Get the abstract name of a canonical grammar
getAbsName :: CanonicalGrammar -> String
getAbsName (Grammar (Abstract (ModId name) _ _ _) _) = name

-- | Get all the concrete names of a canonical grammar
getConcNames :: CanonicalGrammar -> [String]
getConcNames (Grammar _ concs) =
  [name | (Concrete (ModId name) _ _ _ _ _) <- concs]

-- | Function to filter out rules that are not in a list of allowed ones
filterGrammar :: [String] -> [String] -> CanonicalGrammar -> CanonicalGrammar
filterGrammar includedFuns excludedFuns (Grammar absGram concs) =
  Grammar (filterAbstract absGram)
    $ map filterConcrete concs
  where
    filterAbstract (Abstract absId flags cats absfuns) =
      Abstract absId flags cats [f | f <- absfuns, let (FunDef (FunId fname) _) = f, fname `elem` includedFuns, fname `notElem` excludedFuns]
    filterConcrete  (Concrete concId absId flags params lincat lindef) =
      Concrete concId absId flags params lincat [f | f <- lindef, let (LinDef (FunId fname) _ _) = f, fname `elem` includedFuns, fname `notElem` excludedFuns]

-- | Get all abstract funcrions from a canonical grammar
allAbsFuns :: CanonicalGrammar -> [String]
allAbsFuns (Grammar (Abstract _ _ _ funs) _) =
  [funId | (FunDef (FunId funId) _) <- funs]

-- | Looks up an abstract type from a canonical grammar
lookupAbstractType :: String -> CanonicalGrammar -> Maybe Type
lookupAbstractType funId (Grammar absGram _) =
  let
    (Abstract _ _ _ absFuns) = absGram
  in
    listToMaybe [funType | (FunDef (FunId funId') funType) <- absFuns, funId' == funId]

-- | Looks up a linearization from a canonical grammar
lookupConcreteLin :: String -> CanonicalGrammar -> [(String, Maybe ([String],LinValue))]
lookupConcreteLin funId (Grammar _ concGrams) =
  map (\c@(Concrete (ModId concId) _ _ _ _ _) -> (concId,lookupConcreteLin' c)) concGrams
  where
    lookupConcreteLin' (Concrete (ModId concId) absId flags params lincat lindef) = listToMaybe [([id' | (VarId id') <- vars],value) |(LinDef (FunId funId') vars value) <- lindef, funId' == funId]

-- | Merges rules
mergeRules :: [[String]] -> CanonicalGrammar -> CanonicalGrammar
mergeRules rules g@(Grammar absGram concs) =
  let
    -- find the rules to be merged
    singleRules = nub $ concat rules
    -- merge rules
    mergeableRules = [(combineAbstract g r,combineConcrete g r) | r <- rules] 
    -- remove the single rules
    allFuns = allAbsFuns g
    g'@(Grammar absGram' concs') = filterGrammar allFuns singleRules g
    -- add merged rules
    newAbs = map fst mergeableRules
    newConc = map snd mergeableRules
  in
    Grammar (mergeAbstract absGram' newAbs) (mergeConcrete concs' newConc)
  where
    mergeAbstract :: Abstract -> [FunDef] -> Abstract
    mergeAbstract (Abstract absId flags cats funs) newFuns=
      Abstract absId flags cats (funs ++ newFuns)
    combineAbstract :: CanonicalGrammar -> [String] -> FunDef
    combineAbstract grammar funs@(f:fs) =
      let
        funId = concat $ intersperse "_" funs
        funtype = foldl (\fullType nextPart -> updateType fullType (lookupAbstractType nextPart grammar)) (fromJust $ lookupAbstractType f grammar) fs
      in
        FunDef (FunId funId) funtype
      where
        -- | Combines two types to a new type
        updateType :: Type -> Maybe Type -> Type
        -- case of not a function
        updateType t@(Type [] _) _ = t
        -- case of a hole
        updateType t Nothing = t
        -- general case
        updateType (Type (b:bs) app) newType@(Just (Type bindings' app'))
        -- matching category
          | getBindingCat b == getAppCat app' = Type (bindings' ++ bs) app
        -- mismatch
        -- not sure what to do here, atm skip and try next type
          | otherwise = let (Type bindings'' app'') = updateType (Type bs app) newType in Type (b:bindings'') app''
        -- Get the main category from a type binding
        getBindingCat :: TypeBinding -> String
        getBindingCat (TypeBinding _ (Type _ app)) = getAppCat app
        -- Get the main category from a type application
        getAppCat :: TypeApp -> String
        getAppCat (TypeApp (CatId c) _) = c        
    mergeConcrete :: [Concrete] -> [[(String,LinDef)]] -> [Concrete]
    mergeConcrete concs newLins =
      [ (Concrete (ModId concId) absId flags params lincat (lindef ++ [lin | lins <- newLins, (concId',lin) <- lins,concId' == concId])) | (Concrete (ModId concId) absId flags params lincat lindef) <- concs]
    combineConcrete :: CanonicalGrammar -> [String] -> [(String,LinDef)]
    combineConcrete (Grammar abs concs) funs =      
      [(concId,combineConcrete' c funs) | c@(Concrete (ModId concId) _ _ _ _ _ ) <- concs]
      where
        -- In one concrete syntax, create a new LinDef from a list of rule names
        combineConcrete' :: Concrete -> [String] -> LinDef
        combineConcrete' conc funs@(f:fs) =
          let
            linId = concat $ intersperse "_" funs
            -- First function in list, pattern matching like this includes potentially dangerous assumptions         
            [(_,Just (vars1,lin1))] = lookupConcreteLin f (Grammar abs [conc])
            -- the second list of variables should be empty
            ((vars,_),linValue) =
              foldl (\((v,v'),l) l' -> let ((w,w'),n) = substituteVar (v',l) l' in ((v++w,w'),n)) (([],vars1),lin1) [l | fun <- fs, let [(_,l)] = lookupConcreteLin fun (Grammar abs [conc])]              
          in
            LinDef (FunId linId) (map VarId vars) linValue
          -- Replaces a variable with a linvalue and returns the new lin as well as the variables split into touched and untouched
        substituteVar :: ([String],LinValue) -> Maybe ([String],LinValue) -> (([String],[String]),LinValue)
        substituteVar ([],l) _ = (([],[]),l)
        substituteVar ((v:vs),lin) Nothing = (([v],vs),lin)
        substituteVar ((v:vs),lin) (Just (vs',lin')) = ((vs',vs), mapLinValue (substitute v lin') lin)
        -- Does the real substitution of the VarValue
        substitute :: String -> LinValue -> LinValue -> LinValue
        substitute v l (VarValue (VarValueId (Unqual vid))) 
          | v == vid = l
        substitute _ _ l = l
        -- | Rename all vars in a lin by prefixing them with a function id
        renameVars :: String -> ([String],LinValue) -> ([String],LinValue)
        renameVars funId (vars,lin)=
          (map ((funId ++"_") ++) vars,mapLinValue (rename funId) lin)
        -- Does the real renaming in the VarValue
        rename funId (VarValue (VarValueId (Unqual vid))) = (VarValue (VarValueId (Unqual $ funId ++ "_" ++ vid)))
        rename _ l = l
        -- Generic meta-function to apply a function to each linvalue contained in a linvalue
        mapLinValue f (ConcatValue l1 l2) = f $ ConcatValue (mapLinValue f l1) (mapLinValue f l2)
        mapLinValue f (TupleValue ls) = f $ TupleValue (map (mapLinValue f) ls) 
        mapLinValue f (VariantValue ls) = f $ VariantValue (map (mapLinValue f) ls)
        mapLinValue f (PreValue pres l) = f $ PreValue [(ss,mapLinValue f lv) | (ss,lv) <- pres] (mapLinValue f l)
        mapLinValue f (Projection l lid) = f $ Projection (mapLinValue f l) lid
        mapLinValue f (Selection l1 l2) = f $ Selection (mapLinValue f l1) (mapLinValue f l2)
        mapLinValue f (CommentedValue s l) = f $ CommentedValue s (mapLinValue f l)
        mapLinValue f (RecordValue rrvs) = f $ RecordValue [RecordRow i (mapLinValue f l) | RecordRow i l <- rrvs]
        mapLinValue f (TableValue t trvs) = f $ TableValue t [TableRow p (mapLinValue f l) | TableRow p l <- trvs]
        mapLinValue f (ParamConstant (Param pid ls)) = f $ ParamConstant (Param pid (map (mapLinValue f) ls))
        mapLinValue f l = f l

  
-- | Loads a canonical grammar from a list of concrete GF files
loadCanonicalGrammar :: [FilePath] -> [FilePath] -> IO CanonicalGrammar
loadCanonicalGrammar libPath concs =
  do
    let options = modifyFlags (\f -> f { optLibraryPath = libPath })
    (_,(concname,gfgram)) <- GF.batchCompile options concs
    let absname = GF.srcAbsName gfgram concname
    return $ GF.grammar2canonical noOptions absname gfgram
    
