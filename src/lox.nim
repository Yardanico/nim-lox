import os

import errors, scanner, parser, interpreter, env, resolver

proc run(source: string, env = newEnvironment()) = 
  let scanner = newScanner(source)
  let tokens = scanner.scanTokens()
  let parser = newParser(tokens)
  let stmts = parser.parse()
  # If parsing failed
  if (hadSyntaxError): return

  let interpreter = newInterpreter(env)

  let res = newResolver(interpreter)
  res.resolve(stmts)
  # If resolver pass failed
  if (hadSyntaxError): return

  interpreter.interpret(stmts)

proc runPrompt = 
  let env = newEnvironment()
  while true:
    stdout.write("> ")
    stdout.flushFile()
    run(stdin.readLine(), env)
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