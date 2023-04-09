# Resolre (Very Much In Progress)
## A Brainfuck compiler using Solfege/Solresol syllables

Resolre is a Brainfuck compiler adorned with the thin veneer of [Solresol][https://en.wikipedia.org/wiki/Solresol], an esoteric constructed language using the syllables of solfege. Resolre means "Error" in Solresol, in addition to being the inverse of "Solresol" itself. (I also strongly considered Solsolredo, "headache" in Solresol, which would have been closer to its brainfuck lineage).

To make things more frustrating (or interesting?) I have named the filenames according to loose Solresol translations:

Solresol | Translation
-------- | ----------
laresolre | package
larelasi | list/catalog (the brainfuck array)
solfamifa-soldofa | solfege-type
solrela-sisolla | lexical-analysis
mirefami | main

### Current State of the Project:

So far, I've managed to compile one test file, dumb-hello-world.ssrd. It spits out Hello, and regrettably doesn't use loops, as they're still being debugged. You can try it with `(run-file (asdf:system-relative-pathname "resolre" "dumb-hello-world.rsr"))`

I've been putting this implementation together with this as my primary reference: https://esolangs.org/wiki/Brainfuck



(For more Solresol translations, I found this dictionary both useful and amusing:
https://www.geocachingtoolbox.com/pages/codeTables/solresolDictionary.pdf )
