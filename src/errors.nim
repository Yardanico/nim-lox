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

proc report*(line: int, where, msg: string, isWarn: bool) = 
  if not isWarn:
    echo fmt"[line {line}] Error{where}: {msg}"
    hadSyntaxError = true
  else:
    echo fmt"[line {line}] Warning{where}: {msg}"

proc error*(tok: Token, msg: string, isWarn = false) = 
  if tok.kind == Eof:
    report(tok.line, " at end", msg, isWarn)
  else:
    report(tok.line, " at '" & tok.lexeme & "'", msg, isWarn)

proc warning*(tok: Token, msg: string) = 
  error(tok, msg, true)

proc error*(line: int, msg: string, isWarn = false) = 
  report(line, "", msg, isWarn)

proc runtimeError*(error: RuntimeError) = 
  echo &"{error.msg}\n[line {error.tok.line}]"
  hadRuntimeError = true