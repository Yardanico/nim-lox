import os

import errors, scanner, parser, interpreter

proc run(source: string) = 
  let scanner = newScanner(source)
  let tokens = scanner.scanTokens()

  let parser = newParser(tokens)
  let expr = parser.parse()

  let interpreter = newInterpreter()

  if (hadSyntaxError): return

  interpreter.interpret(expr)

proc runPrompt = 
  while true:
    stdout.write("> ")
    run(stdin.readLine())
    hadSyntaxError = false

proc runFile(path: string) = 
  let data = readFile(path)
  run(data)

  if (hadSyntaxError): quit 65
  if (hadRuntimeError): quit 70

proc main = 
  if paramCount() > 1:
    echo "Usage: nlox [script]"
    quit 64
  elif paramCount() == 1:
    runFile(paramStr(1))
  else:
    runPrompt()

main()