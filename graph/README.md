This directory hold the code for the pretty-printing functionality of 
Coccinelle. Pretty-printing converts the AST into a graph by describing it with
DOT.

Methods that are called in response to the `--graph-cocci-ast` and 
`--graph-c-ast` option are declared in `actions.ml[i]`