import std / [strformat, strutils]

import scanner, chunk, value, debug

type
  Parser = object
    src: string
    cur, prev: Token
    hadError, panicMode: bool
  
  Precedence = enum
    PrecNone,
    PrecAssignment, # =
    PrecOr, # or
    PrecAnd, # and 
    PrecEquality, # == !=
    PrecComparison, # < > <= >=
    PrecTerm, # + -
    PrecFactor, # * /
    PrecUnary, # ! -
    PrecCall,  # . ()
    PrecPrimary
  
  ParseFn = proc(comp: Compiler): void

  ParseRule = ref object
    prefix, infix: ParseFn
    precedence: Precedence
  
  Compiler = ref object
    parser: Parser
    scanner: Scanner
    compilingChunk: Chunk

using
  comp: Compiler

proc newCompiler(src: string): Compiler = 
  Compiler(
    parser: Parser(src: src),
    scanner: newScanner(src)
  )

template makeRule(pref, inf: ParseFn, prec: Precedence): ParseRule = 
  ParseRule(prefix: pref, infix: inf, precedence: prec)

proc errorAt(comp; tok: Token, msg: string) = 
  if (comp.parser.panicMode): return
  comp.parser.panicMode = true
  stderr.write(fmt"[line {tok.line}] Error")

  if tok.kind == Eof:
    stderr.write(" at end")
  elif tok.kind == Error:
    discard
  else:
    stderr.write(" at '" & comp.parser.src[tok.start..tok.start+tok.len-1] & '\'')
  
  stderr.write(&": {msg}\n")
  comp.parser.hadError = true

proc error(comp; msg: string) = 
  comp.errorAt(comp.parser.prev, msg)

proc errorAtCurrent(comp; msg: string) = 
  comp.errorAt(comp.parser.cur, msg)

proc advance(comp) = 
  comp.parser.prev = comp.parser.cur

  while true:
    comp.parser.cur = comp.scanner.scanToken()
    if comp.parser.cur.kind != Error: break

    #errorAtCurrent(parser.cur.start)
    comp.errorAtCurrent("error here")

proc consume(comp; kind: TokenKind, msg: string) = 
  if comp.parser.cur.kind == kind:
    comp.advance()
  else:
    #errorAtCurrent(msg)
    comp.errorAtCurrent(msg)

proc check(comp; kind: TokenKind): bool = 
  comp.parser.cur.kind == kind

proc match(comp; kind: TokenKind): bool = 
  result = if not comp.check(kind): 
    false
  else: 
    comp.advance()
    true

proc emitByte(comp; byt: uint8 | Opcode) = 
  writeChunk(comp.compilingChunk, uint8(byt), comp.parser.prev.line)

proc emitBytes(comp; byt1: uint8 | Opcode, byt2: uint8 | Opcode) = 
  comp.emitByte(uint8(byt1))
  comp.emitByte(uint8(byt2))

proc emitReturn(comp) = 
  comp.emitByte(byte(OpReturn))

proc makeConstant(comp; val: Value): uint8 = 
  result = uint8 comp.compilingChunk.addConstant(val)

  if result > uint8.high:
    comp.error("Too many constants in one chunk.")
    result = 0

proc emitConstant(comp; val: Value) = 
  ## Emits an OpConstant or OpConstantLong instruction and writes
  ## the index of the constant in the next byte (or next 3 bytes)
  if comp.compilingChunk.consts.values.len > 255:
    comp.emitByte(OpConstantLong)
    let idx = comp.compilingChunk.addConstant(val)
    # TODO: maybe there's a better portable way of doing this?
    # convert a small number to 3 bytes
    var bytes = cast[array[3, uint8]](idx)
    comp.emitBytes(bytes[0], bytes[1])
    comp.emitByte(bytes[2])
  else:
    comp.emitBytes(OpConstant, comp.makeConstant(val))


proc endCompiler(comp) = 
  comp.emitReturn()
  when defined(loxDebug):
    comp.compilingChunk.disassemble("code")

proc substr(comp; a, b: int): string = 
  comp.parser.src[comp.parser.prev.start + a .. comp.parser.prev.start + b]

proc expression(comp)
proc getRule(kind: TokenKind): ParseRule
proc parsePrecedence(comp; prec: Precedence)


proc binary(comp) = 
  let opType = comp.parser.prev.kind
  var rule = getRule(opType)
  comp.parsePrecedence(Precedence(int(rule.precedence) + 1))

  case opType
  of BangEqual: comp.emitBytes(OpEqual, OpNot)
  of EqualEqual: comp.emitByte(OpEqual)
  # Can be implemented as OpNot (OpLess or OpGreater)
  of Greater: comp.emitByte(OpGreater)
  of GreaterEqual: comp.emitBytes(OpLess, OpNot)
  of Less: comp.emitByte(OpLess)
  of LessEqual: comp.emitBytes(OpGreater, OpNot)
  of Plus: comp.emitByte(OpAdd)
  # Can be implemented as OpAdd with a negative operand
  of Minus: comp.emitByte(OpSubtract)
  of Star: comp.emitByte(OpMultiply)
  of Slash: comp.emitByte(OpDivide)
  else: discard

proc literal(comp) = 
  case comp.parser.prev.kind
  of False: comp.emitByte(OpFalse)
  of Nil: comp.emitByte(OpNil)
  of True: comp.emitByte(OpTrue)
  else: assert false

proc grouping(comp) = 
  comp.expression()
  comp.consume(RightParen, "Expect ')' after expression.")

proc number(comp) = 
  let val = parseFloat(comp.substr(0, comp.parser.prev.len - 1))
  comp.emitConstant(numVal(val))

proc strval(comp) = 
  # Trim leading and trailing quotation marks
  # TODO: If need to translate \n and other escape sequences,
  # do it here.
  comp.emitConstant(
    objVal(
      stringObj(
        comp.substr(1, comp.parser.prev.len - 2)
      )
    )
  )

proc unary(comp) = 
  let opType = comp.parser.prev.kind

  comp.parsePrecedence(PrecUnary)
  case opType
  of Bang: comp.emitByte(OpNot)
  of Minus: comp.emitByte(OpNegate)
  else: discard

proc bracket(comp) = 
  comp.parsePrecedence(PrecTerm)
  comp.emitByte(OpIndexGet)
  comp.consume(RightBracket, "Expect closing ']' for indexing access.")

var rules: array[TokenKind, ParseRule] = [
  makeRule(grouping, nil, PrecNone), # LeftParen
  makeRule(nil, nil, PrecNone), # RightParen
  makeRule(nil, nil, PrecNone), # LeftBrace
  makeRule(nil, nil, PrecNone), # RightBrace
  makeRule(nil, bracket, PrecCall), # LeftBracket
  makeRule(nil, nil, PrecNone), # RightBracket
  makeRule(nil, nil, PrecNone), # Comma
  makeRule(nil, nil, PrecNone), # Dot
  makeRule(unary, binary, PrecTerm), # Minus
  makeRule(nil, binary, PrecTerm), # Plus
  makeRule(nil, nil, PrecNone), # Semicolon
  makeRule(nil, binary, PrecFactor), # Slash
  makeRule(nil, binary, PrecFactor), # Star
  makeRule(unary, nil, PrecNone), # Bang
  makeRule(nil, binary, PrecEquality), # BangEqual
  makeRule(nil, nil, PrecNone), # Equal
  makeRule(nil, binary, PrecComparison), # EqualEqual
  makeRule(nil, binary, PrecComparison), # Greater
  makeRule(nil, binary, PrecComparison), # GreaterEqual
  makeRule(nil, binary, PrecComparison), # Less
  makeRule(nil, binary, PrecComparison), # LessEqual
  makeRule(nil, nil, PrecNone), # Identifier
  makeRule(strval, nil, PrecNone), # String
  makeRule(number, nil, PrecNone), # Number
  makeRule(nil, nil, PrecNone), # And
  makeRule(nil, nil, PrecNone), # Class
  makeRule(nil, nil, PrecNone), # Else
  makeRule(literal, nil, PrecNone), # False
  makeRule(nil, nil, PrecNone), # For
  makeRule(nil, nil, PrecNone), # Fun
  makeRule(nil, nil, PrecNone), # If
  makeRule(literal, nil, PrecNone), # Nil
  makeRule(nil, nil, PrecNone), # Or
  makeRule(nil, nil, PrecNone), # Print
  makeRule(nil, nil, PrecNone), # Return
  makeRule(nil, nil, PrecNone), # Super
  makeRule(nil, nil, PrecNone), # This
  makeRule(literal, nil, PrecNone), # True
  makeRule(nil, nil, PrecNone), # Var
  makeRule(nil, nil, PrecNone), # While
  makeRule(nil, nil, PrecNone), # Error
  makeRule(nil, nil, PrecNone), # Eof
]

proc getRule(kind: TokenKind): ParseRule = 
  rules[kind]

proc parsePrecedence(comp; prec: Precedence) = 
  comp.advance()
  let prefixRule = getRule(comp.parser.prev.kind).prefix

  if prefixRule.isNil():
    comp.error("Expect expression")
    return
  comp.prefixRule()
  while (prec <= getRule(comp.parser.cur.kind).precedence):
    comp.advance()
    let infixRule = getRule(comp.parser.prev.kind).infix
    comp.infixRule()

proc expression(comp) = 
  comp.parsePrecedence(PrecAssignment)

proc printStatement(comp) = 
  comp.expression()
  comp.consume(Semicolon, "Expect ';' after value.")
  comp.emitByte(OpPrint)

proc statement(comp) = 
  if comp.match(Print):
    comp.printStatement()

proc declaration(comp) = 
  comp.statement()

proc compile*(src: string, chunk: Chunk): bool = 
  var c = newCompiler(src)
  c.compilingChunk = chunk
  c.advance()

  while not c.match(Eof):
    c.declaration()
  #c.expression()
  #c.consume(Eof, "Expect end of expression.")
  c.endCompiler()
  result = not c.parser.hadError