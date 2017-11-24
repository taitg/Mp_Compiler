

CPSC 411 Assignment 5 Winter 2017 (Geordie Tait)

Compiler for M+
=================================

This is a Haskell-based compiler (lexer/parser/semantic analyzer/code generator)
for the M+ language using Alex/Happy and utilizing the BNF Converter. It reads
in M+ code from a text file in order to produce a syntax tree, and then uses this
syntax tree (along with a generated symbol table) to produce an intermediate
representation of the M+ code. From this is generated AM stack machine code.

Arrays and user-defined data types were not implemented.

How to run and compile
=================================

Usage:          ./CompileMplus <inputfile> <outputfile>

Compilation:    make

A note on tests
=================================

Included is a folder of test cases (M+ code). This includes all the given test
programs and 6 of my own (mytest1.m+ through mytest6.m+). Note that mytest5.m+
is an example that will not compile due to invalid type.

Tests 1 and 4 of the given test cases have been modified so that they compile.
The assumption was made that these were not supposed to compile in their original
form (test1.m+ attempts to add to a boolean value, test4.m+ attempts to assign a
value to an undeclared variable f, which also has the same name as a declared function).
