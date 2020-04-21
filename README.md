# Nim implementation of Lox interpreter.
For more info about Lox see http://www.craftinginterpreters.com/

"examples" directory contains examples of Lox programs. Some of them might be used
for testing the interpreter later in the future.

This interpreter is based on the C implementation of the interpreter in the book.
(There's also an interpreter without classes and inheritance based on Java implementation in the branch ``old``)

The main difference from the C implementation is that this implementation
does not manually allocate/deallocate memory (at least for now), since Nim has automatic garbage collection. It'll probably be needed in the future

Completed optional exercises or stuff which is not in the original language:
- String indexing by [] like ``"hello"[0]`` or ``"yes"[5+5*5-30+1]`` (done via an OP_INDEX_GET opcode)