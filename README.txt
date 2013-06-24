Alpacca - Another Lua Parser and Compiler Compiler Attempt
==========================================================

Alpcca allows you to specify a grammar using near EBNF syntax.  The
specification you write is (by the magic of lua's metatable/operator
overloading mechanism) translated into a set of parser combinators
which efficiently parse an input string, and indicates via a returned
boolean whether the string conforms to the grammar or not.

In addition, the user can configure the grammar to create a parse tree
on sucessful parsing.  

Alpacca delivers this power in a very small amount of code (>1k lines)
making it lightweight enough to add to almost any project.  

