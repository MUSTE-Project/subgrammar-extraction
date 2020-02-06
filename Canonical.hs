{- (c) Thomas Hallgren 2019 -}
{- (c) Herbert Lange 2020 -}
module Canonical where

import System.FilePath((</>),(<.>))
import System.Directory(createDirectoryIfMissing)
import GF.Support(writeUTF8File)
import GF.Text.Pretty(render80)
import GF.Grammar.Canonical -- (Grammar(..),ModId(..),abstrName,concName,Abstract)
import Data.List(stripPrefix)

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


mergeRules :: [String] -> CanonicalGrammar -> CanonicalGrammar
mergeRules = undefined -- rules = undefined
  -- find the rules to be merged
  -- remove the single rules
  -- merge rules
  
-- | Loads a canonical grammar from a list of concrete GF files
loadCanonicalGrammar :: [FilePath] -> [FilePath] -> IO CanonicalGrammar
loadCanonicalGrammar libPath concs =
  do
    let options = modifyFlags (\f -> f { optLibraryPath = libPath })
    (_,(concname,gfgram)) <- GF.batchCompile options concs
    let absname = GF.srcAbsName gfgram concname
    return $ GF.grammar2canonical noOptions absname gfgram
    
