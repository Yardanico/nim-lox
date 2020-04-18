import strformat

import types, tokens


type  
  RuntimeError* = ref object of ValueError
    tok*: Token
  # TODO: Rename these to something else?
  BreakError* = ref object of RuntimeError
  ReturnError* = ref object of RuntimeError
    value*: LoxValue

var hadSyntaxError* = false
var hadRuntimeError* = false

proc report*(line: int, where, msg: string) = 
  echo fmt"[line {line}] Error{where}: {msg}"
  hadSyntaxError = true

proc error*(tok: Token, msg: string) = 
  if tok.kind == Eof:
    report(tok.line, " at end", msg)
  else:
    report(tok.line, " at '" & tok.lexeme & "'", msg)

proc error*(line: int, msg: string) = 
  report(line, "", msg)

proc runtimeError*(error: RuntimeError) = 
  echo &"{error.msg}\n[line {error.tok.line}]"
  hadRuntimeError = true