import std / os
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
  let res = interpret(src)

  if res == InterpretCompileError: 
    quit "Compilation failed!"
  elif res == InterpretRuntimeError: 
    quit "Runtime error!"

proc main = 
  initVM()
  case paramCount()
  of 0:
    repl()
  of 1:
    runFile(paramStr(1))
  else:
    stderr.write("Usage: nlox [path]\n")
  repl()

main()