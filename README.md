# Ancient greek interpreter helper

## Problem description

Ancient greek text comes without spaces between words. Given a dictionary of words, split ancient
greek text into possible sequences of words from the dictionary.

## Program usage 

Enter a nix shell:

```bash
nix-shell
```

First, generate the dictionary in serialised format:

```
cabal run ag-mkdictionary -- \
    --input-path $dict_path/CTS_XML_TEI/perseus/pdllex/grc/lsj/ \
    --output-file dictionary.bin
```

Then, parse an input using the dictionary:

```
cabal run ag-parser -- \
    --input-dictionary-file ./dictionary.bin \
    --input-text-file ./data/manuscript.in
```

Here, `dictionary.bin` is the dictionary generated using `ag-mkdictionary`, and `manuscript.in` is 
a text file containing unseparated words. The output format is still a work in progress for now.

## Development guidelines

### Run the test suite

```
cabal test
```

### Formatting

```
fourmolu --mode inplace .
```

## Dictionary format

The dictionary entries in XML format are in betacode format. But we wish to simplify our input by
providing it in a form mapped to latin characters. Below is a mapping from betacode to unicode:

```
BETACODE_MAP = {
  "a*(/|": "ᾍ",
  "*(/|a": "ᾍ",
  "a*(/": "Ἅ",
  "*(/a": "Ἅ",
  "a*(\\|": "ᾋ",
  "*(\\|a": "ᾋ",
  "a*(\\": "Ἃ",
  "*(\\a": "Ἃ",
  "a*(=|": "ᾏ",
  "*(=|a": "ᾏ",
  "a*(=": "Ἇ",
  "*(=a": "Ἇ",
  "a*(|": "ᾉ",
  "*(|a": "ᾉ",
  "a*(": "Ἁ",
  "*(a": "Ἁ",
  "a*)/|": "ᾌ",
  "*)/|a": "ᾌ",
  "a*)/": "Ἄ",
  "*)/a": "Ἄ",
  "a*)\\|": "ᾊ",
  "*)\\|a": "ᾊ",
  "a*)\\": "Ἂ",
  "*)\\a": "Ἂ",
  "a*)=|": "ᾎ",
  "*)=|a": "ᾎ",
  "a*)=": "Ἆ",
  "*)=a": "Ἆ",
  "a*)|": "ᾈ",
  "*)|a": "ᾈ",
  "a*)": "Ἀ",
  "*)a": "Ἀ",
  "a*/": "Ά",
  "*/a": "Ά",
  "a*\\": "Ὰ",
  "*\\a": "Ὰ",
  "a*|": "ᾼ",
  "*|a": "ᾼ",
  "a*_": "Ᾱ",
  "*_a": "Ᾱ",
  "a*^": "Ᾰ",
  "*^a": "Ᾰ",
  "a*": "Α",
  "*a": "Α",
  "a(/|": "ᾅ",
  "a(/": "ἅ",
  "a(\\|": "ᾃ",
  "a(\\": "ἃ",
  "a(=|": "ᾇ",
  "a(=": "ἇ",
  "a(|": "ᾁ",
  "a(": "ἁ",
  "a)/|": "ᾄ",
  "a)/": "ἄ",
  "a)\\|": "ᾂ",
  "a)\\": "ἂ",
  "a)=|": "ᾆ",
  "a)=": "ἆ",
  "a)|": "ᾀ",
  "a)": "ἀ",
  "a/|": "ᾴ",
  "a/": "ά",
  "a\\|": "ᾲ",
  "a\\": "ὰ",
  "a=|": "ᾷ",
  "a=": "ᾶ",
  "a|": "ᾳ",
  "a_": "ᾱ",
  "a^": "ᾰ",
  "a": "α",
  "b*": "Β",
  "*b": "Β",
  "b": "β",
  "c*": "Ξ",
  "*c": "Ξ",
  "c": "ξ",
  "d*": "Δ",
  "*d": "Δ",
  "d": "δ",
  "e*(/": "Ἕ",
  "*(/e": "Ἕ",
  "e*(\\": "Ἓ",
  "*(\\e": "Ἓ",
  "e*(": "Ἑ",
  "*(e": "Ἑ",
  "e*)/": "Ἔ",
  "*)/e": "Ἔ",
  "e*)\\": "Ἒ",
  "*)\\e": "Ἒ",
  "e*)": "Ἐ",
  "*)e": "Ἐ",
  "e*/": "Έ",
  "*/e": "Έ",
  "e*\\": "Ὲ",
  "*\\e": "Ὲ",
  "e*": "Ε",
  "*e": "Ε",
  "e(/": "ἕ",
  "e(\\": "ἓ",
  "e(": "ἑ",
  "e)/": "ἔ",
  "e)\\": "ἒ",
  "e)": "ἐ",
  "e/": "έ",
  "e\\": "ὲ",
  "e": "ε",
  "f*": "Φ",
  "*f": "Φ",
  "f": "φ",
  "g*": "Γ",
  "*g": "Γ",
  "g": "γ",
  "h*(/|": "ᾝ",
  "*(/|h": "ᾝ",
  "h*(/": "Ἥ",
  "*(/h": "Ἥ",
  "h*(\\|": "ᾛ",
  "*(\\|h": "ᾛ",
  "h*(\\": "Ἣ",
  "*(\\h": "Ἣ",
  "h*(=|": "ᾟ",
  "*(=|h": "ᾟ",
  "h*(=": "Ἧ",
  "*(=h": "Ἧ",
  "h*(|": "ᾙ",
  "*(|h": "ᾙ",
  "h*(": "Ἡ",
  "*(h": "Ἡ",
  "h*)/|": "ᾜ",
  "*)/|h": "ᾜ",
  "h*)/": "Ἤ",
  "*)/h": "Ἤ",
  "h*)\\|": "ᾚ",
  "*)\\|h": "ᾚ",
  "h*)\\": "Ἢ",
  "*)\\h": "Ἢ",
  "h*)=|": "ᾞ",
  "*)=|h": "ᾞ",
  "h*)=": "Ἦ",
  "*)=h": "Ἦ",
  "h*)|": "ᾘ",
  "*)|h": "ᾘ",
  "h*)": "Ἠ",
  "*)h": "Ἠ",
  "h*/": "Ή",
  "*/h": "Ή",
  "h*\\": "Ὴ",
  "*\\h": "Ὴ",
  "h*|": "ῌ",
  "*|h": "ῌ",
  "h*": "Η",
  "*h": "Η",
  "h(/|": "ᾕ",
  "h(/": "ἥ",
  "h(\\|": "ᾓ",
  "h(\\": "ἣ",
  "h(=|": "ᾗ",
  "h(=": "ἧ",
  "h(|": "ᾑ",
  "h(": "ἡ",
  "h)/|": "ᾔ",
  "h)/": "ἤ",
  "h)\\|": "ᾒ",
  "h)\\": "ἢ",
  "h)=|": "ᾖ",
  "h)=": "ἦ",
  "h)|": "ᾐ",
  "h)": "ἠ",
  "h/|": "ῄ",
  "h/": "ή",
  "h\\|": "ῂ",
  "h\\": "ὴ",
  "h=|": "ῇ",
  "h=": "ῆ",
  "h|": "ῃ",
  "h": "η",
  "i*(/": "Ἵ",
  "*(/i": "Ἵ",
  "i*(\\": "Ἳ",
  "*(\\i": "Ἳ",
  "i*(=": "Ἷ",
  "*(=i": "Ἷ",
  "i*(": "Ἱ",
  "*(i": "Ἱ",
  "i*)/": "Ἴ",
  "*)/i": "Ἴ",
  "i*)\\": "Ἲ",
  "*)\\i": "Ἲ",
  "i*)=": "Ἶ",
  "*)=i": "Ἶ",
  "i*)": "Ἰ",
  "*)i": "Ἰ",
  "i*+": "Ϊ",
  "*+i": "Ϊ",
  "i*/": "Ί",
  "*/i": "Ί",
  "i*\\": "Ὶ",
  "*\\i": "Ὶ",
  "i*_": "Ῑ",
  "*_i": "Ῑ",
  "i*^": "Ῐ",
  "*^i": "Ῐ",
  "i*": "Ι",
  "*i": "Ι",
  "i(/": "ἵ",
  "i(\\": "ἳ",
  "i(=": "ἷ",
  "i(": "ἱ",
  "i)/": "ἴ",
  "i)\\": "ἲ",
  "i)=": "ἶ",
  "i)": "ἰ",
  "i+/": "ΐ",
  "i+\\": "ῒ",
  "i+=": "ῗ",
  "i+": "ϊ",
  "i/": "ί",
  "i\\": "ὶ",
  "i=": "ῖ",
  "i_": "ῑ",
  "i^": "ῐ",
  "i": "ι",
  "k*": "Κ",
  "*k": "Κ",
  "k": "κ",
  "l*": "Λ",
  "*l": "Λ",
  "l": "λ",
  "m*": "Μ",
  "*m": "Μ",
  "m": "μ",
  "n*": "Ν",
  "*n": "Ν",
  "n": "ν",
  "o*(/": "Ὅ",
  "*(/o": "Ὅ",
  "o*(\\": "Ὃ",
  "*(\\o": "Ὃ",
  "o*(": "Ὁ",
  "*(o": "Ὁ",
  "o*)/": "Ὄ",
  "*)/o": "Ὄ",
  "o*)\\": "Ὂ",
  "*)\\o": "Ὂ",
  "o*)": "Ὀ",
  "*)o": "Ὀ",
  "o*/": "Ό",
  "*/o": "Ό",
  "o*\\": "Ὸ",
  "*\\o": "Ὸ",
  "o*": "Ο",
  "*o": "Ο",
  "o(/": "ὅ",
  "o(\\": "ὃ",
  "o(": "ὁ",
  "o)/": "ὄ",
  "o)\\": "ὂ",
  "o)": "ὀ",
  "o/": "ό",
  "o\\": "ὸ",
  "o": "ο",
  "p*": "Π",
  "*p": "Π",
  "p": "π",
  "q*": "Θ",
  "*q": "Θ",
  "q": "θ",
  "r*(": "Ῥ",
  "*(r": "Ῥ",
  "r*)": "᾿Ρ",
  "*)r": "᾿Ρ",
  "r*": "Ρ",
  "*r": "Ρ",
  "r(": "ῥ",
  "r)": "ῤ",
  "r": "ρ",
  "s*": "Σ",
  "*s": "Σ",
  "s": "σ",
  "t*": "Τ",
  "*t": "Τ",
  "t": "τ",
  "u*(/": "Ὕ",
  "*(/u": "Ὕ",
  "u*(\\": "Ὓ",
  "*(\\u": "Ὓ",
  "u*(=": "Ὗ",
  "*(=u": "Ὗ",
  "u*(": "Ὑ",
  "*(u": "Ὑ",
  "u*+": "Ϋ",
  "*+u": "Ϋ",
  "u*/": "Ύ",
  "*/u": "Ύ",
  "u*\\": "Ὺ",
  "*\\u": "Ὺ",
  "u*_": "Ῡ",
  "*_u": "Ῡ",
  "u*^": "Ῠ",
  "*^u": "Ῠ",
  "u*": "Υ",
  "*u": "Υ",
  "u(/": "ὕ",
  "u(\\": "ὓ",
  "u(=": "ὗ",
  "u(": "ὑ",
  "u)/": "ὔ",
  "u)\\": "ὒ",
  "u)=": "ὖ",
  "u)": "ὐ",
  "u+/": "ΰ",
  "u+\\": "ῢ",
  "u+=": "ῧ",
  "u+": "ϋ",
  "u/": "ύ",
  "u\\": "ὺ",
  "u=": "ῦ",
  "u_": "ῡ",
  "u^": "ῠ",
  "u": "υ",
  "v*": "Ϝ",
  "*v": "Ϝ",
  "v": "ϝ",
  "w*(/|": "ᾭ",
  "*(/|w": "ᾭ",
  "w*(/": "Ὥ",
  "*(/w": "Ὥ",
  "w*(\\|": "ᾫ",
  "*(\\|w": "ᾫ",
  "w*(\\": "Ὣ",
  "*(\\w": "Ὣ",
  "w*(=|": "ᾯ",
  "*(=|w": "ᾯ",
  "w*(=": "Ὧ",
  "*(=w": "Ὧ",
  "w*(|": "ᾩ",
  "*(|w": "ᾩ",
  "w*(": "Ὡ",
  "*(w": "Ὡ",
  "w*)/|": "ᾬ",
  "*)/|w": "ᾬ",
  "w*)/": "Ὤ",
  "*)/w": "Ὤ",
  "w*)\\|": "ᾪ",
  "*)\\|w": "ᾪ",
  "w*)\\": "Ὢ",
  "*)\\w": "Ὢ",
  "w*)=|": "ᾮ",
  "*)=|w": "ᾮ",
  "w*)=": "Ὦ",
  "*)=w": "Ὦ",
  "w*)|": "ᾨ",
  "*)|w": "ᾨ",
  "w*)": "Ὠ",
  "*)w": "Ὠ",
  "w*/": "Ώ",
  "*/w": "Ώ",
  "w*\\": "Ὼ",
  "*\\w": "Ὼ",
  "w*|": "ῼ",
  "*|w": "ῼ",
  "w*": "Ω",
  "*w": "Ω",
  "w(/|": "ᾥ",
  "w(/": "ὥ",
  "w(\\|": "ᾣ",
  "w(\\": "ὣ",
  "w(=|": "ᾧ",
  "w(=": "ὧ",
  "w(|": "ᾡ",
  "w(": "ὡ",
  "w)/|": "ᾤ",
  "w)/": "ὤ",
  "w)\\|": "ᾢ",
  "w)\\": "ὢ",
  "w)=|": "ᾦ",
  "w)=": "ὦ",
  "w)|": "ᾠ",
  "w)": "ὠ",
  "w/|": "ῴ",
  "w/": "ώ",
  "w\\|": "ῲ",
  "w\\": "ὼ",
  "w=|": "ῷ",
  "w=": "ῶ",
  "w|": "ῳ",
  "w": "ω",
  "x*": "Χ",
  "*x": "Χ",
  "x": "χ",
  "y*": "Ψ",
  "*y": "Ψ",
  "y": "ψ",
  "z*": "Ζ",
  "*z": "Ζ",
  "z": "ζ",
  "(/|": "῞ͅ",
  "(/": "῞",
  "(\\|": "῝ͅ",
  "(\\": "῝",
  "(=|": "῟ͅ",
  "(=": "῟",
  "(|": "ʽͅ",
  "(": "ʽ",
  ")/|": "῎ͅ",
  ")/": "῎",
  ")\\|": "῍ͅ",
  ")\\": "῍",
  ")=|": "῏ͅ",
  ")=": "῏",
  ")|": "ʼͅ",
  ")": "ʼ",
  "+/": "΅",
  "+\\": "῭",
  "+=": "῁",
  "+": "¨",
  "/|": "´ͅ",
  "/": "´",
  "\\|": "`ͅ",
  "\\": "`",
  "=|": "῀ͅ",
  "=": "῀",
  "|": "ι",
  "_": "¯",
  "^": "˘",
  "'": "᾽",
  "s1": "σ",
  "s2": "ς"
}
```

Here are some notes, useful links about this mapping:

[1]: https://github.com/PerseusDL/lexica/tree/master/CTS_XML_TEI/perseus/pdllex/grc/lsj
[2]: https://github.com/matgrioni/betacode
[3]: http://www.tlg.uci.edu/encoding/BCM.pdf
[4]: https://github.com/PerseusDL/tei-conversion-tools/wiki/Greek-Betacode-to-Unicode-Transformations


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

2025-09-16

Fix the dictionary deserialiser. Possibly modify the serialiser to match. Using binary-conduit 
    seems to work for now.

Create text parser with text input and dictionary input. Output debugging result to stdout

Getting some results, but a phrase from the original text is not
present. Add some more dictionary commands to be able to decipher meaning
of individual words. There could be some discrepancy between the alphabet
used and the betacode normalised to latin chars. Find out what that is.

2025-09-17

Refactor modules and write some documentation

## TODO:

- Simplify parser: remove HashMap input and use Set input
- Add http server to query the dictionary and parse input more efficiently
- Make the parser efficient
- Benchmark serialisation to ensure that it runs in constant memory
- Consider simple-pipe also, there is xml-pipe
