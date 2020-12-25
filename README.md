# WHILE language parser and evaluator

This small program parses and evaluates a given program written in the WHILE 
language taught under the Models of Computation course at Imperial College
London. The program evaluation is printed as a LaTeX file containing a
big-step operational semantics proof of the program.

# Running the program

To run the program, simply compile using stack

```
stack build
```

and then run the program, supplying an input text file containing your program
and an output file name

```
stack run input.txt output.tex
```

# The WHILE language

The WHILE language is defined by the following grammar

```
Expr ::= x ||
         n ||
         Expr + Expr ||
         Expr * Expr

Boolean ::= true ||
            false ||
            Expr = Expr ||
            Expr < Expr ||
            Expr > Expr ||
            Boolean & Boolean ||
            Boolean | Boolean ||
            ~ Boolean

Command ::= x := Expr ||
            if Boolean then Command else Command ||
            Command ; Command ||
            skip ||
            while Boolean then Command 
```

where `x` denotes variable identifiers (alphanumeric strings where the first
character is alphabetical) and `n` denotes natural numbers. When encountered,
the skip command terminates the computation early.
