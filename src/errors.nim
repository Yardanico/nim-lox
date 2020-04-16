import strformat

import tokens


type  
  RuntimeError* = ref object of ValueError
    tok*: Token

var hadSyntaxError* = false
var hadRuntimeError* = false

proc report*(line: int, where, msg: string) = 
  echo fmt"[line {line}] Error {where}: {msg}"
  hadSyntaxError = true

proc error*(tok: Token, msg: string) = 
  if tok.kind == Eof:
    report(tok.line, " at end", msg)
  else:
    report(tok.line, "at '" & tok.lexeme & "'", msg)

proc error*(line: int, msg: string) = 
  report(line, "", msg)

proc runtimeError*(error: RuntimeError) = 
  echo &"{error.msg}\n[line {error.tok.line}]"
  hadRuntimeError = true