import os

import vm

proc repl = 
  while true:
    stdout.write("> ")

    var line: string
    if not stdin.readLine(line):
      stdout.write("\n")
      break
    
    echo interpret(line)

proc runFile(path: string) = 
  var src = readFile(path)
  let result = interpret(src)

  if (result == InterpretCompileError): quit(65)
  elif (result == InterpretRuntimeError): quit(70)

proc main = 
  initVM()
  if (paramCount() == 0):
    repl()
  elif paramCount() == 1:
    runFile(paramStr(2))
  else:
    stderr.write("Usage: nlox [path]\n");

main()