# Nim implementation of Lox interpreter.

"examples" directory contains examples of Lox programs. Some of them might be used
for testing the interpreter later in the future.

Work-in-progress as I read the book.

Completed optional exercises or stuff which is not in the original language:
- Multiline comments with /* ... */
- Ternary operator `true? "a" : "b"`
- Break statement (detection of incorrect usage is done in resolver pass)
- Automatic conversion to string when at least one of arguments is string (addition op)
- Catching division by zero 
- Native "echo" function which acts the same as the "print" statement
- Comments can also be made by using `#` like in Nim
- Anonymous functions (== functions as expressions) (Chapter 10 ex. 2)
- Unused variables are detected by the resolver (Chapter 11 ex. 3, although this interpreter produces a warning instead of an error).