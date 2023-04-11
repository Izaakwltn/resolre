# Resolre
## A Brainfuck compiler using Solfege syllables

Resolre is a Brainfuck compiler adorned with a thin veneer of [Solresol](https://en.wikipedia.org/wiki/Solresol), an invented human language using the syllables of solfege. 

Resolre means "Error" in Solresol, in addition to being the inverse of "Solresol" itself. (I also strongly considered Solsolredo, "headache" in Solresol, which honestly bears more resemblance to my experience writing this project).

### Simple Example (Prints "7")

```
print-7.rsr

mimimimi mimimi      ; c0 = 7
                    
                     ; add 48 to 7 for ascii:
                     
do mimimimi mimimimi ; c1 = 8 (loop counter)
la                   ; start loop
  re mimimimi mimi   ; incr c0 x6
  do fa              ; decr c1
si                   ; end loop

re so                ; print c0 => 55 ("7")
```

### Syntax

Resolre/brainfuck is a language that exists within an array of cells, which are navigated, altered, and accessed with only eight commands.

Like Solresol, Resolre uses solfege syllables: do re mi fa so la si. Here are the eight Resolre commands, along with their Brainfuck counterparts and their respective functions:

Resolre | Brainfuck | Function
--- | --- | ---
do  | >   | Move one cell to the right
re  | <   | Move one cell to the left
mi  | +   | Increment the current cell
fa  | -   | Decrement the current cell
so  | .   | Print the corresponding ascii character to the value of the current cell
la  | [   | Start loop, if the current cell's value is 0, end loop.
si  | ]   | If cell is non-zero, go back to the start of the loop, else continue past.

### Navigating the Project:

To make things more frustratingly eccentric, and hopefully interesting, I have named the source files according to Solresol translations:

Solresol | Translation
-------- | ----------
laresolre | package
larelasi | list/catalog (the array)
resolremi | commands
solrela-sisolla | lexical-analysis
mirefami | main

### Current State of the Project:

Three .rsr files have been tested and are up and running, if you'd like to try them out:
`(run-hello-world)`
`(run-dumb-hello-world)`
`(run-print-7)`
`(run-summoning)`

I'll be adding more examples soon. 

### Resources/citations

I've been putting this implementation together with these as my primary references: 

- https://esolangs.org/wiki/Brainfuck 
- https://en.wikipedia.org/wiki/Brainfuck
- https://www.geocachingtoolbox.com/pages/codeTables/solresolDictionary.pdf
