import std / strutils

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
  
  Scanner* = ref object
    src: string
    start, current: int
    line: int

using
  s: Scanner

proc newScanner*(src: string): Scanner =
  Scanner(
    start: 0,
    src: src,
    current: 0,
    line: 1
  )

proc getSlice(s): string = 
  s.src[s.start ..< s.current]

proc isAtEnd(s): bool = 
  s.current > s.src.len-1

proc advance(s): char {.discardable.} = 
  inc s.current
  if s.current - 1 > s.src.len-1: '\0'
  else: s.src[s.current - 1]

proc peek(s): char = 
  if s.current > s.src.len-1: '\0'
  else: s.src[s.current]

proc peekNext(s): char = 
  if s.isAtEnd(): '\0'
  else: s.src[s.current+1]

proc match(s; expected: char): bool = 
  if s.isAtEnd(): false
  elif s.src[s.current] != expected:
    false
  else:
    inc s.current
    true
  
proc initToken(s; kind: TokenKind): Token = 
  Token(
    kind: kind, 
    start: s.start, 
    len: s.current - s.start,
    line: s.line
  )

proc errorToken(s; msg: string): Token = 
  Token(
    kind: Error,
    # TODO: token error message
    start: 0,
    len: 10,
    line: s.line
  )

proc skipWhitespace(s) = 
  while true:
    let c = s.peek()
    case c
    of ' ', '\r', '\t': s.advance()
    of '\n':
      inc s.line
      s.advance()
    of '/':
      if s.peekNext() == '/':
        # A comment goes until the EOL
        while s.peek() != '\n' and not s.isAtEnd():
          s.advance()
      else:
        break
    else: 
      break

proc identifierType(s): TokenKind = 
  let ident = s.getSlice()
  case ident
  of "and": And
  of "class": Class
  of "else": Else
  of "if": If
  of "nil": Nil
  of "or": Or
  of "print": Print
  of "return": Return
  of "super": Super
  of "this": This
  of "true": True
  of "var": Var
  of "while": While
  of "false": False
  of "for": For
  of "fun": Fun  
  else: Identifier

proc identifier(s): Token = 
  while s.peek().isAlphaAscii() or s.peek().isDigit():
    s.advance()
  
  s.initToken(s.identifierType())

proc number(s): Token = 
  while s.peek().isDigit():
    s.advance()
  
  if s.peek() == '.' and s.peekNext().isDigit():
    s.advance()
    while s.peek().isDigit(): s.advance()
  
  s.initToken(Number)

proc strlit(s): Token = 
  while not s.isAtEnd() and s.peek() != '"':
    if s.peek() == '\n':
      inc s.line
    s.advance()
  
  result = if s.isAtEnd():
    s.errorToken("Unterminated string.")
  else:
    s.advance()
    s.initToken(String)

proc scanToken*(s): Token = 
  s.skipWhitespace()
  s.start = s.current
  let c = s.advance()
  let typ = case c
  of Letters: Identifier
  of Digits: Number
  of '(': LeftParen
  of ')': RightParen
  of '{': LeftBrace
  of '}': RightBrace
  of '[': LeftBracket
  of ']': RightBracket
  of ';': Semicolon
  of ',': Comma
  of '.': Dot
  of '-': Minus
  of '+': Plus
  of '/': Slash
  of '*': Star
  of '!': 
    if s.match('='): BangEqual else: Bang
  of '=': 
    if s.match('='): EqualEqual else: Equal
  of '<': 
    if s.match('='): LessEqual else: Less
  of '>':
    if s.match('='): GreaterEqual else: Greater
  of '"': String
  else: 
    if s.isAtEnd(): Eof
    else: Error
  
  case typ
  of Identifier: s.identifier()
  of Number: s.number()
  of String: s.strlit()
  of Error: s.errorToken("Unexpected character.") 
  else: s.initToken(typ)