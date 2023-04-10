# Resolre
## A Brainfuck compiler using Solfege syllables

Resolre is a Brainfuck compiler adorned with a thin veneer of [Solresol](https://en.wikipedia.org/wiki/Solresol), an invented human language using the syllables of solfege. 

Resolre means "Error" in Solresol, in addition to being the inverse of "Solresol" itself. (I also strongly considered Solsolredo, "headache" in Solresol, which honestly bears more resemblance to my experience writing this project).

### Syntax

Resolre/brainfuck is a language that exists within an array of cells, which are navigated, altered, and accessed with only eight commands.

Like Solresol, Resolre uses solfege syllables: do re mi fa so la si ut. Here are the eight Resolre commands, along with their Brainfuck counterparts and their respective functions:

Resolre | Brainfuck | Function
--- | --- | ---
do  | >   | move one cell to the right
re  | <   | move one cell to the left
mi  | +   | increment the current cell
fa  | -   | decrement the current cell
so  | .   | print the corresponding ascii character to the value of the current cell
la  | ,   | take one byte of input, store its value in the current cell
si  | [   | Start loop, if the current cell's value is 0, end loop.
ut  | ]   | If cell is non-zero, go back to the start of the loop, else continue past.

### Navigating the Project:

To make things more frustratingly eccentric, and hopefully interesting, I have named the source files according to Solresol translations:

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
