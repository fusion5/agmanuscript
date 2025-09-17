# Ancient greek interpreter helper

Usage:

First, generate the dictionary in serialised format:

```
cabal run mkdictionary -- \
    --input-path $dict_path/CTS_XML_TEI/perseus/pdllex/grc/lsj/ \
    --output-file dictionary.bin
```

## Development guidelines

### Formatting: 

```
fourmolu --mode inplace .
```

## Changelog

2025-02-08

2025-03-15

Use conduit and xml-conduit to parse the dictionary files
    OK done, with folder traversal.

2025-03-22

Refactor modules, add fourmolu formatter.

2025-05-23

Define a simple parser model and some tests for it.

2025-06-20 

Fix the parser model and make the tests pass. Write more tests.

2025-06-25

Try to normalise dictionary terms from betacode to Roman alphabet, this means that
  normalised terms will have more meanings...
Keep only the roman characters and strip out anything else which would generate accents.

2025-09-15

Create dictionary deserialiser

2025-09-15

Fix the dictionary deserialiser. Possibly modify the serialiser to match. Using binary-conduit 
    seems to work for now.

Create text parser with text input and dictionary input. Output debugging result to stdout

Getting some results, but a phrase from the original text is not
present. Add some more dictionary commands to be able to decipher meaning
of individual words. There could be some discrepancy between the alphabet
used and the betacode normalised to latin chars. Find out what that is.

## TODO:

Simplify parser: remove HashMap input and use Set input
Add http server to query the dictionary and parse input more efficiently
Make the parser efficient
Benchmark serialisation to ensure that it runs in constant memory
Consider simple-pipe also, there is xml-pipe
