Warning
=======
This is unsupported legacy code

# Readme

This project has a cabal and stack file. The easiest probably is to use stack.

The `glpk` library is required for building this project.

Run `stack ghci` to load all necessary files into a GHCi session.

## Running it

In `ghci` use:

```
import PGF
import Data.Maybe
import qualified Subgrammar.GFRule as GFRule

positive_examples = ["I am here now"]
negative_examples = ["now I am here"]

rgl <- readPGF "/home/herb/src/gf/gf-rgl/src/english/Lang.pgf"
grammar = Grammar rgl ["/home/herb/src/gf/gf-rgl/src/english/LangEng.gf"]
positive_trees = examplesToForests grammar (fromJust $ readLanguage "LangEng") positive_examples
negative_trees = examplesToForests grammar (fromJust $ readLanguage "LangEng") negative_examples
problem = GFRule.forestsToProblem positive_trees negative_trees GFRule.numRules
solution <- solve problem
generateGrammar grammar (Just "/home/herb/src/gf/gf-rgl/src/") ["abstract","english","common","prelude","api"] solution True
```

## Testing 

There currently are two test functions `Subgrammar.GFRule.test` and `Subgrammar.GFSubtree`.

The test functions require the [Exemplum grammar](https://github.com/MUSTE-Project/mulle-grammars) (both the
PGF and the ExemplumEng.gf file) and the [GF RGL](https://github.com/GrammaticalFramework/gf-rgl)

The relevant pathes can be adjusted in the `Subgrammar/Common.hs` file.

The tests load a grammar and use a set of example sentences to generate a new grammar. Afterwards it is tested
that the examples are really covered.


## Profiling

To profile the code you can build the code with profiling:

```stack build --executable-profiling --ghc-options "-threaded"``` 

or 

```stack install --executable-profiling --ghc-options "-threaded -rtsopts" --pedantic```

Afterwards you can run it with profiling enabled:

```stack exec --rts-options -p subgrammar-extraction```

This results in a file called `subgrammar-extraction.prof` that can be
analysed further.

By also adding `-h` you can also get the heap analysis as a `.hp` file that
can be converted by using `hp2ps`.
