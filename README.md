# Resolre
## A Brainfuck compiler using Solfege/Solresol syllables

Resolre is a Brainfuck compiler adorned with the thin veneer of [Solresol](https://en.wikipedia.org/wiki/Solresol), an esoteric constructed language using the syllables of solfege. 

Resolre means "Error" in Solresol, in addition to being the inverse of "Solresol" itself. (I also strongly considered Solsolredo, "headache" in Solresol, which honestly bears more resemblance to my experience writing this project).

To make things more frustrating (or interesting?) I have named the filenames according to Solresol translations:

Solresol | Translation
-------- | ----------
laresolre | package
larelasi | list/catalog (the brainfuck array)
solfamifa-soldofa | solfege-type
solrela-sisolla | lexical-analysis
mirefami | main

### Current State of the Project:

I've compiled one test file, dumb-hello-world.ssrd. It makes use of all commands except for loops and character input. It spits out `Hello`, which you can try using `(run-dumb-hello-world)` 

Loops are on their way, post haste.

### Resources/citations

I've been putting this implementation together with these as my primary references: 

- https://esolangs.org/wiki/Brainfuck 
- https://en.wikipedia.org/wiki/Brainfuck
- https://www.geocachingtoolbox.com/pages/codeTables/solresolDictionary.pdf
