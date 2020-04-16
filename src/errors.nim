import strformat

var hadError* = false

proc report*(line: int, where, msg: string) = 
  echo fmt"[line {line}] Error{where}: {msg}"
  hadError = true

proc error*(line: int, msg: string) = 
  report(line, "", msg)