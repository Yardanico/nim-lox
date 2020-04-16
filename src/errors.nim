import strformat

import token
var hadError* = false

proc report*(line: int, where, msg: string) = 
  echo fmt"[line {line}] Error {where}: {msg}"
  hadError = true

proc error*(tok: Token, msg: string) = 
  if tok.kind == Eof:
    report(tok.line, " at end", msg)
  else:
    report(tok.line, "at '" & tok.lexeme & "'", msg)

proc error*(line: int, msg: string) = 
  report(line, "", msg)