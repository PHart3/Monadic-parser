This is a purely functional, monadic recursive descent parser for arithmetic expressions produced by the following grammar.

expr ::= expr addop term | term
term ::= term mulop strg | strg
strg ::= factor expop strg | factor
factor ::= numb | ( expr )
numb ::=  0 | 1 | 2 | ...
addop ::= * | -
mulop ::= * | /
expop ::= ^

The parser actually includes a lexer and evaluator for such expressions. To run it, simply use the file "parser_IO.ml" in OCaml's toplevel system and then apply the function doParse to any string. It outputs the value of the expression if it's well-formed or else the default value 0 along with the unparsed suffix. The implementation was inspired by the paper "Monadic parsing in Haskell" by Hutton and Meijer.
