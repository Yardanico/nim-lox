import strformat, strutils

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
  
  ParseFn = proc(): void

  ParseRule = ref object
    prefix, infix: ParseFn
    precedence: Precedence

template makeRule(pref, inf: ParseFn, prec: Precedence): ParseRule = 
  ParseRule(prefix: pref, infix: inf, precedence: prec)

var parser* = Parser()
var compilingChunk*: Chunk

proc currentChunk(): Chunk = 
  compilingChunk

proc errorAt(tok: Token, msg: string) = 
  if (parser.panicMode): return
  parser.panicMode = true
  stderr.write(fmt"[line {tok.line}] Error")

  if tok.kind == Eof:
    stderr.write(" at end")
  elif tok.kind == Error:
    discard
  else:
    stderr.write(" at '" & parser.src[tok.start..tok.start+tok.len-1] & '\'')
  
  stderr.write(&": {msg}\n")
  parser.hadError = true

proc error(msg: string) = 
  errorAt(parser.prev, msg)

proc errorAtCurrent(msg: string) = 
  errorAt(parser.cur, msg)

proc advance = 
  parser.prev = parser.cur

  while true:
    parser.cur = scanToken()
    if parser.cur.kind != Error: break

    #errorAtCurrent(parser.cur.start)
    errorAtCurrent("error here")

proc consume(kind: TokenKind, msg: string) = 
  if parser.cur.kind == kind:
    advance()
  else:
    #errorAtCurrent(msg)
    errorAtCurrent(msg)

proc emitByte(byt: uint8 | Opcode) = 
  writeChunk(currentChunk(), uint8(byt), parser.prev.line)

proc emitBytes(byt1: uint8 | Opcode, byt2: uint8 | Opcode) = 
  emitByte(uint8(byt1))
  emitByte(uint8(byt2))

proc emitReturn() = 
  emitByte(byte(OpReturn))

proc makeConstant(val: Value): uint8 = 
  result = uint8(addConstant(currentChunk(), val))

  if result > uint8.high:
    error("Too many constants in one chunk.")
    return 0

proc emitConstant(val: Value) = 
  emitBytes(OpConstant, makeConstant(val))

proc endCompiler() = 
  emitReturn()
  disassembleChunk(currentChunk(), "code")

proc substr(a, b: int): string = 
  parser.src[parser.prev.start + a .. parser.prev.start + b]

proc expression
proc getRule(kind: TokenKind): ParseRule
proc parsePrecedence(prec: Precedence)


proc binary = 
  let opType = parser.prev.kind
  var rule = getRule(opType)
  parsePrecedence(Precedence(int(rule.precedence) + 1))

  case opType
  of BangEqual: emitBytes(OpEqual, OpNot)
  of EqualEqual: emitByte(OpEqual)
  # Can be implemented as OpNot (OpLess or OpGreater)
  of Greater: emitByte(OpGreater)
  of GreaterEqual: emitBytes(OpLess, OpNot)
  of Less: emitByte(OpLess)
  of LessEqual: emitBytes(OpGreater, OpNot)
  of Plus: emitByte(OpAdd)
  # Can be implemented as OpAdd with a negative operand
  of Minus: emitByte(OpSubtract)
  of Star: emitByte(OpMultiply)
  of Slash: emitByte(OpDivide)
  else: return

proc literal = 
  case parser.prev.kind
  of False: emitByte(OpFalse)
  of Nil: emitByte(OpNil)
  of True: emitByte(OpTrue)
  else: discard # Unreachable

proc grouping = 
  expression()
  consume(RightParen, "Expect ')' after expression.")

proc number = 
  let val = parseFloat(substr(0, parser.prev.len - 1))
  emitConstant(numVal(val))

proc strval = 
  # Trim leading and trailing quotation marks
  # TODO: If need to translate \n and other escape sequences,
  # do it here.
  emitConstant(objVal(stringObj(substr(1, parser.prev.len - 2))))

proc unary = 
  let opType = parser.prev.kind

  parsePrecedence(PrecUnary)
  case opType
  of Bang: emitByte(OpNot)
  of Minus: emitByte(OpNegate)
  else: discard

proc bracket = 
  parsePrecedence(PrecTerm)
  emitByte(OpIndexGet)
  consume(RightBracket, "Expect closing ']' for indexing access.")

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

proc parsePrecedence(prec: Precedence) = 
  advance()
  let prefixRule = getRule(parser.prev.kind).prefix

  if prefixRule.isNil():
    error("Expect expression")
    return
  prefixRule()
  while (prec <= getRule(parser.cur.kind).precedence):
    advance()
    let infixRule = getRule(parser.prev.kind).infix
    infixRule()

proc getRule(kind: TokenKind): ParseRule = 
  rules[kind]

proc expression = 
  parsePrecedence(PrecAssignment)

proc compile*(src: string, chunk: Chunk): bool = 
  initScanner(src)
  compilingChunk = chunk
  parser.hadError = false
  parser.panicMode = false
  parser.src = src
  advance()
  expression()
  consume(Eof, "Expect end of expression.")
  endCompiler()
  return not parser.hadError