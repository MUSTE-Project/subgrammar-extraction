# Readme

This project has a cabal and stack file. The easiest probably is to use stack.

Run `stack ghci` to load all necessary files into a GHCi session.

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