import os, strformat

import errors, scanner

proc run(source: string) = 
  let scanner = newScanner(source)
  let tokens = scanner.scanTokens()

  for token in tokens:
    echo token

proc runPrompt = 
  while true:
    stdout.write("> ")
    run(stdin.readLine())
    hadError = false

proc runFile(path: string) = 
  let data = readFile(path)
  run(data)

  if (hadError): quit 65


proc main = 
  if paramCount() > 1:
    echo "Usage: nlox [script]"
    quit 64
  elif paramCount() == 1:
    runFile(paramStr(1))
  else:
    runPrompt()

main()