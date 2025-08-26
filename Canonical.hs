{- (c) Thomas Hallgren 2019 -}
{- (c) Herbert Lange 2020/2025 -}
module Canonical where

import System.FilePath((</>),(<.>))
import System.Directory(createDirectoryIfMissing)
import GF.Support(writeUTF8File,optLibraryPath,modifyFlags,noOptions)
import GF.Text.Pretty(render80)
import qualified GF
import GF.Grammar.Canonical -- (Grammar(..),ModId(..),abstrName,concName,Abstract)
import Data.List(stripPrefix,nub,intersperse,isPrefixOf)
import Data.Maybe

type CanonicalGrammar = GF.Grammar.Canonical.Grammar

str2rawid :: String -> GF.RawIdent
str2rawid = GF.ident2raw . GF.identS

rawid2str :: GF.RawIdent -> String
rawid2str = GF.showRawIdent

-- | String used to concatenate function names when merging
mergeStr :: String
mergeStr = "_M_"

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
     sequence_ [writeUTF8File (cncPath cnc) (fix_predef $ render80 cnc) | cnc<-cncs]
  where
    absPath = gfpath . abstrName 
    cncPath = gfpath . concName

    gfpath (ModId s) = prefix</> (rawid2str (s :: GF.RawIdent)) <.>"gf"
    fix_predef :: String -> String
    fix_predef [] = []
    fix_predef str
     | isPrefixOf "Predef_" str = "Predef." ++ (fix_predef $ drop (length "Predef_") str)
     | otherwise = take 1 str ++ (fix_predef $ drop 1 str)

-- | Rename a canonical grammar, i.e. both the abstract and the concrete syntax
renameGrammar :: String -> CanonicalGrammar -> CanonicalGrammar
renameGrammar newAbs (Grammar absGram cncs) =
    Grammar (renameAbs absGram) (map renameCnc cncs)
  where
    renameAbs (Abstract _ fls cs fs) = Abstract (ModId (str2rawid newAbs)) fls cs fs

    renameCnc (Concrete (ModId oldCnc) (ModId oldAbs) fls ps lcs ls) =
        Concrete (ModId (str2rawid newCnc)) (ModId (str2rawid newAbs)) fls ps lcs ls
      where
        newCnc = maybe (rawid2str oldCnc) (newAbs++) (stripPrefix (rawid2str oldAbs) (rawid2str oldCnc))

-- Extensions by Herbert Lange

-- | Get the abstract name of a canonical grammar
getAbsName :: CanonicalGrammar -> String
getAbsName (Grammar (Abstract (ModId name) _ _ _) _) = rawid2str name

-- | Get all the concrete names of a canonical grammar
getConcNames :: CanonicalGrammar -> [String]
getConcNames (Grammar _ concs) =
  [rawid2str name | (Concrete (ModId name) _ _ _ _ _) <- concs]

-- | Function to filter out rules that are not in a list of allowed ones
filterGrammar :: [String] -> [String] -> CanonicalGrammar -> CanonicalGrammar
filterGrammar includedFuns excludedFuns (Grammar absGram concs) =
  Grammar (filterAbstract absGram)
    $ map filterConcrete concs
  where
    filterAbstract (Abstract absId flags cats absfuns) =
      Abstract absId flags cats [f | f <- absfuns, let (FunDef (FunId fname) _) = f, (rawid2str fname) `elem` includedFuns, (rawid2str fname) `notElem` excludedFuns]
    filterConcrete  (Concrete concId absId flags params lincat lindef) =
      Concrete concId absId flags params lincat [f | f <- lindef, let (LinDef (FunId fname) _ _) = f, (rawid2str fname) `elem` includedFuns, (rawid2str fname) `notElem` excludedFuns]

-- | Get all abstract funcrions from a canonical grammar
allAbsFuns :: CanonicalGrammar -> [String]
allAbsFuns (Grammar (Abstract _ _ _ funs) _) =
  [rawid2str funId | (FunDef (FunId funId) _) <- funs]

-- | Looks up an abstract type from a canonical grammar
lookupAbstractType :: String -> CanonicalGrammar -> Maybe Type
lookupAbstractType funId (Grammar absGram _) =
  let
    (Abstract _ _ _ absFuns) = absGram
  in
    listToMaybe [funType | (FunDef (FunId funId') funType) <- absFuns, funId' == str2rawid funId]

-- | Looks up a linearization from a canonical grammar
lookupConcreteLin :: String -> CanonicalGrammar -> [(String, Maybe ([String],LinValue))]
lookupConcreteLin funId (Grammar _ concGrams) =
  map (\c@(Concrete (ModId concId) _ _ _ _ _) -> (rawid2str concId,lookupConcreteLin' c)) concGrams
  where
    lookupConcreteLin' (Concrete _ _ _ _ _ lindef) = listToMaybe [([rawid2str id' | (VarId id') <- vars],value) |(LinDef (FunId funId') vars value) <- lindef, funId' == str2rawid funId]

-- | Merges rules
mergeRules :: [[String]] -> CanonicalGrammar -> CanonicalGrammar
mergeRules rules g@(Grammar absGram concs) =
  let
    -- find the rules to be merged
    singleRules = nub $ concat rules
    -- merge rules
    mergeableRules = [(combineAbstract g rs,combineConcrete rs) | rs <- rules] 
    -- remove the single rules
    allFuns = allAbsFuns g
    -- Remove all the single rules that are part of merged rules
    (Grammar absGram' concs') = filterGrammar allFuns singleRules g
    -- prepare merged rules
    newAbs = map fst mergeableRules
    newConc = map snd mergeableRules
  in
    -- create a new grammar where the rules are merged
    Grammar (mergeAbstract absGram' newAbs) (mergeConcrete concs' newConc)
  where
    -- Merges the existing and the merged rules to a new abstract syntax
    mergeAbstract :: Abstract -> [FunDef] -> Abstract
    mergeAbstract (Abstract absId flags cats funs) newFuns=
      Abstract absId flags cats (funs ++ newFuns)
    -- Combines a list of rules to a new abstract rule
    combineAbstract :: CanonicalGrammar -> [String] -> FunDef
    combineAbstract _ [] =
      error "Empty function list"
    combineAbstract grammar funs@(f:fs) =
      let
        -- Create new function name by putting the mergeStr between the function names
        funId = concat $ intersperse mergeStr funs
        -- Create new function type from the types of the single functions by stepwise updating the type
        funtype = foldl (\fullType nextPart -> updateType fullType (lookupAbstractType nextPart grammar)) (fromJust $ lookupAbstractType f grammar) fs
      in
        FunDef (FunId (str2rawid funId)) funtype
      where
        -- | Combines two types to a new type
        -- It looks for any parameter in the first type that has the same category as the result type of the second type
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
        getAppCat (TypeApp (CatId c) _) = rawid2str c
    -- Merges a concrete grammar with a list of newly  combined functions
    -- Updates the lindefs for each concrete grammar where the id matches. The first component of the second parameter is the id of the concrete syntax to be updated
    mergeConcrete :: [Concrete] -> [[(String,LinDef)]] -> [Concrete]
    mergeConcrete cs newLins =
      [ (Concrete (ModId concId) absId flags params lincat (lindef ++ [lin | lins <- newLins, (concId',lin) <- lins,concId' == rawid2str concId])) | (Concrete (ModId concId) absId flags params lincat lindef) <- cs]
    combineConcrete :: [String] -> [(String,LinDef)]
    combineConcrete funs =
      -- -- New Version
      -- [(concId,combineConcrete' c) | c@(Concrete (ModId concId) _ _ _ _ _ ) <- concs]
      -- where        
      --   -- In one concrete syntax, create a new LinDef from a list of rule names
      --   combineConcrete' :: Concrete -> LinDef
      --   combineConcrete' conc =
      --     let
      --       -- split funs into head and tail
      --       (f:fs) = funs
      --       -- create new linId
      --       linId = concat $ intersperse mergeStr funs
      --       -- First function in list, pattern matching like this includes potentially dangerous assumptions, i.e. that function must exist
      --       [(_,Just (vars1,lin1))] = lookupConcreteLin f (Grammar absGram [conc])
      --       -- the second list of variables should be empty
      --       vars =
      --         foldl (\(LinDef 
      --           vars1 -- the first

      --           [lindef | fun <- fs, let [(_,lindef)] = lookupConcreteLin fun (Grammar absGram [conc])]
      --       linValue = LiteralValue (StrConstant "empty")
      --     in
      --       LinDef (FunId linId) (map VarId vars) linValue
      -- Old version:
      [(rawid2str concId,combineConcrete' c) | c@(Concrete (ModId concId) _ _ _ _ _ ) <- concs]
      where
        -- In one concrete syntax, create a new LinDef from a list of rule names
        combineConcrete' :: Concrete -> LinDef
        combineConcrete' conc =
          let
            -- split funs into head and tail
            (f:fs) = funs
            -- create new linId
            linId = concat $ intersperse mergeStr funs
            -- First function in list, pattern matching like this includes potentially dangerous assumptions, i.e. that function must exist
            [(_,Just (vars1,lin1))] = lookupConcreteLin f (Grammar absGram [conc])
            -- the second list of variables should be empty
            ((vars,_),linValue) =
              foldl (\((touchedVars,untouchedVars),lindef) mergedLindef -> let ((touchedVars',untouchedVars'),newLindef) = substituteVar linId (untouchedVars,lindef) mergedLindef in ((touchedVars++touchedVars',untouchedVars'),newLindef))
                (([],vars1),lin1) -- the first lindef, no touched vars and the (untouched) vars of the first lindef
                -- look up the lindefs of all functions
                [lindef | fun <- fs, let [(_,lindef)] = lookupConcreteLin fun (Grammar absGram [conc])]              
          in
            LinDef (FunId (str2rawid linId)) (map VarId (map str2rawid vars)) linValue
        -- Replaces a variable with a linvalue and returns the new lin as well as the variables split into touched and untouched
        substituteVar :: String -> ([String],LinValue) -> Maybe ([String],LinValue) -> (([String],[String]),LinValue)
        -- 
        substituteVar _ ([],l) _ = (([],[]),l)
        substituteVar _ ((v:vs),lin) Nothing = (([v],vs),lin)
        substituteVar funId ((v:vs),lin) (Just l') = let (vs',lin') = renameVars funId l' in ((vs',vs), mapLinValue (substitute v lin') lin)
        -- Does the real substitution of the VarValue
        substitute :: String -> LinValue -> LinValue -> LinValue
        substitute v newLin (VarValue (VarValueId (Unqual vid))) 
          | v == (GF.showRawIdent vid) = newLin
        substitute _ _ oldLin = oldLin
        -- | Rename all vars in a lin by prefixing them with a function id
        renameVars :: String -> ([String],LinValue) -> ([String],LinValue)
        renameVars funId (vars,lin)=
          (map ((funId ++ mergeStr) ++) vars,mapLinValue (rename funId) lin)
        -- Does the real renaming in the VarValue
        rename funId (VarValue (VarValueId (Unqual vid))) = (VarValue (VarValueId (Unqual $ str2rawid $ funId ++ mergeStr ++ (GF.showRawIdent vid))))
        rename _ l = l
        
-- Generic higher-order function to apply a function to each linvalue contained in a linvalue
mapLinValue :: (LinValue -> LinValue) -> LinValue -> LinValue
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
mapLinValue f l@(VarValue _) = f l
mapLinValue f l = error $ "no idea how to handle " ++ show l --f l

  
-- | Loads a canonical grammar from a list of concrete GF files
loadCanonicalGrammar :: [FilePath] -> [FilePath] -> IO CanonicalGrammar
loadCanonicalGrammar libPath concs =
  do
    let options = modifyFlags (\f -> f { optLibraryPath = libPath })
    (_,(concname,gfgram)) <- GF.batchCompile options concs
    let absname = GF.srcAbsName gfgram concname
    return $ GF.grammar2canonical noOptions absname gfgram
    
