import strutils, parseutils

type
  TokenKind* = enum
    # Single-character tokens
    LeftParen, RightParen, 
    LeftBrace, RightBrace,
    LeftBracket, RightBracket,
    Comma, 
    Dot, Minus, Plus, 
    Semicolon, Slash, Star

    # One or two character tokens
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    # Literals
    Identifier, String, Number,

    # Keywords
    And, Class, Else, False, For, Fun, If, Nil, Or,
    Print, Return, Super, This, True, Var, While

    Error
    Eof
  
  Token* = object
    kind*: TokenKind
    start*, len*: int
    line*: int
  
  Scanner = object
    src: string
    start, current: int
    line: int


var scanner = Scanner()

proc initScanner*(src: string) = 
  scanner.start = 0
  scanner.src = src
  scanner.current = 0
  scanner.line = 1

proc isAtEnd(): bool = 
  scanner.current > scanner.src.len-1

proc advance: char {.discardable.} = 
  inc scanner.current
  if scanner.current - 1 > scanner.src.len-1: '\0'
  else: scanner.src[scanner.current - 1]

proc peek: char = 
  if scanner.current > scanner.src.len-1: '\0'
  else: scanner.src[scanner.current]

proc peekNext: char = 
  if isAtEnd(): '\0'
  else: scanner.src[scanner.current+1]

proc match(expected: char): bool = 
  if isAtEnd(): false
  elif scanner.src[scanner.current] != expected:
    false
  else:
    inc scanner.current
    true
  
proc initToken(kind: TokenKind): Token = 
  Token(
    kind: kind, 
    start: scanner.start, 
    len: scanner.current - scanner.start,
    line: scanner.line
  )

proc errorToken(msg: string): Token = 
  Token(
    kind: Error,
    # TODO: token error message
    start: 0,
    len: 10,
    line: scanner.line
  )

proc skipWhitespace = 
  while true:
    let c = peek()
    case c
    of ' ', '\r', '\t': advance()
    of '\n':
      inc scanner.line
      advance()
    of '/':
      if peekNext() == '/':
        # A comment goes until the EOL
        while peek() != '\n' and not isAtEnd():
          advance()
      else:
        return
    else: return

proc checkKeyword(start: int, rest: static[string], kind: TokenKind): TokenKind = 
  # TODO: Figure a better way to do this.
  let add = if start == 1: 0 else: 1 
  if scanner.src[scanner.start+start..scanner.start + rest.len + add] == rest:
    kind
  else: Identifier 

proc identifierType(): TokenKind = 
  case scanner.src[scanner.start]
  of 'a': checkKeyword(1, "nd", And)
  of 'c': checkKeyword(1, "lass", Class)
  of 'e': checkKeyword(1, "lse", Else)
  of 'i': checkKeyword(1, "f", If)
  of 'n': checkKeyword(1, "il", Nil)
  of 'o': checkKeyword(1, "r", Or)
  of 'p': checkKeyword(1, "rint", Print)
  of 'r': checkKeyword(1, "eturn", Return)
  of 's': checkKeyword(1, "uper", Super)
  of 't':
    if (scanner.current - scanner.start) > 1:
      case scanner.src[scanner.start+1]
      of 'h': checkKeyword(2, "is", This)
      of 'r': checkKeyword(2, "ue", True)
      else: Identifier
    else: Identifier
  of 'v': checkKeyword(1, "ar", Var)
  of 'w': checkKeyword(1, "hile", While)
  of 'f':
    if (scanner.current - scanner.start) > 1:
      case scanner.src[scanner.start+1]
      of 'a': checkKeyword(2, "lse", False)
      of 'o': checkKeyword(2, "r", For)
      of 'u': checkKeyword(2, "n", Fun)
      else: Identifier
    else: Identifier
  
  else: Identifier

proc identifier: Token = 
  while isAlphaAscii(peek()) or isDigit(peek()):
    advance()
  
  initToken(identifierType())

proc number: Token = 
  while isDigit(peek()):
    advance()
  
  if peek() == '.' and isDigit(peekNext()):
    advance()
    while (isDigit(peek())): advance()
  
  initToken(Number)

proc string: Token = 
  while not isAtEnd() and peek() != '"':
    if peek() == '\n':
      inc scanner.line
    advance()
  
  if isAtEnd():
    return errorToken("Unterminated string.")

  advance()
  initToken(String)

proc scanToken*: Token = 
  skipWhitespace()
  scanner.start = scanner.current
  let c = advance()
  if isAlphaAscii(c): return identifier()
  elif isDigit(c):
    return number()

  case c
  of '(': return initToken(LeftParen)
  of ')': return initToken(RightParen)
  of '{': return initToken(LeftBrace)
  of '}': return initToken(RightBrace)
  of '[': return initToken(LeftBracket)
  of ']': return initToken(RightBracket)
  of ';': return initToken(Semicolon)
  of ',': return initToken(Comma)
  of '.': return initToken(Dot)
  of '-': return initToken(Minus)
  of '+': return initToken(Plus)
  of '/': return initToken(Slash)
  of '*': return initToken(Star)
  of '!': return initToken(if match('='): BangEqual else: Bang)
  of '=': return initToken(if match('='): EqualEqual else: EqualEqual)
  of '<': return initToken(if match('='): LessEqual else: Less)
  of '>': return initToken(if match('='): GreaterEqual else: Greater)
  of '"': return string()
  else: discard

  if isAtEnd():
    return initToken(Eof)

  result = errorToken("Unexpected character.")