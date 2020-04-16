import tables, strutils

import errors, tokens


const 
  # TODO: Use something else, using a table seems kinda dirty
  Keywords = {
    "and": And,
    "class": Class,
    "else": Else,
    "false": False,
    "for": For,
    "fun": Fun,
    "if": If,
    "nil": Nil,
    "or": Or,
    "print": Print,
    "return": Return,
    "super": Super,
    "this": This,
    "true": True,
    "var": Var,
    "while": While
  }.toTable()

type
  Scanner* = ref object
    source: string
    tokens: seq[Token]
    start, current, line: int


proc newScanner*(src: string): Scanner = 
  Scanner(source: src, line: 1)


# TODO: Use stdlib functions instead of these
proc isAtEnd(s: Scanner): bool = 
  s.current >= s.source.len

proc isDigit(c: char): bool = 
  c in {'0'..'9'}

proc isAlnum(c: char): bool = 
  c in Letters + Digits + {'_'}

proc advance(s: Scanner): char {.discardable.} = 
  inc s.current
  return s.source[s.current-1]

proc getText(s: Scanner): string = 
  s.source[s.start..s.current-1]

proc addToken(s: Scanner, kind: TokenKind) = 
  let text = s.getText()
  s.tokens.add initTok(kind, text, s.line)

proc addToken(s: Scanner, kind: TokenKind, lit: string | float) = 
  # TODO: Do we really need to get text?
  let text = s.getText()
  when lit is string:
    s.tokens.add initStrTok(lit, s.line)
  else:
    s.tokens.add initNumTok(lit, s.line, text)


proc match(s: Scanner, expected: char): bool = 
  result = false
  if s.isAtEnd(): return
  if s.source[s.current] != expected: return

  inc s.current
  result = true

proc peek(s: Scanner): char = 
  if s.isAtEnd(): '\0'
  else: s.source[s.current]

proc peekNext(s: Scanner): char = 
  if s.current + 1 >= s.source.len: '\0'
  else: s.source[s.current+1]


proc string(s: Scanner) = 
  while s.peek() != '"' and not s.isAtEnd():
    if s.peek == '\n': inc s.line
    s.advance()
  # Unterminated string.
  if s.isAtEnd():
    error(s.line, "Unterminated string.")
  
  # The closing ".
  s.advance()

  # Trim the surrounding quotes.
  let val = s.source[s.start+1 .. s.current-2]
  s.addToken(String, val)

proc number(s: Scanner) = 
  while s.peek().isDigit(): 
    s.advance()

  # Detect decimals
  if s.peek() == '.' and s.peekNext().isDigit():
    s.advance()

    while s.peek().isDigit(): 
      s.advance()
  
  s.addToken(Number, parseFloat(s.getText()))

proc ident(s: Scanner) = 
  while s.peek().isAlnum():
    s.advance()
  
  let text = s.getText()
  let tokType = Keywords.getOrDefault(text, Eof)
  
  s.addToken(if tokType == Eof: Identifier else: tokType)


proc scanToken(s: Scanner) = 
  let c = s.advance()
  case c
  of '(': s.addToken(LeftParen)
  of ')': s.addToken(RightParen)
  of '{': s.addToken(LeftBrace)
  of '}': s.addToken(RightBrace)
  of ',': s.addToken(Comma)
  of '.': s.addToken(Dot)
  of '-': s.addToken(Minus)
  of '+': s.addToken(Plus)
  of ';': s.addToken(Semicolon)
  of '*': s.addToken(Star)
  of '!': s.addToken(if s.match('='): BangEqual else: Bang)
  of '=': s.addToken(if s.match('='): EqualEqual else: Equal)
  of '<': s.addToken(if s.match('='): LessEqual else: Less)
  of '>': s.addToken(if s.match('='): GreaterEqual else: Greater)
  of '?': s.addToken(QuestionMark)
  of ':': s.addToken(Colon)
  # Either a comment or a slash
  of '/': 
    if s.match('/'):
      while s.peek() != '\n' and not s.isAtEnd():
        s.advance()
    # Multinline comment
    elif s.match('*'):
      while s.peek() != '*' and s.peekNext() != '/' and not s.isAtEnd():
        s.advance()
        if s.source[s.current] == '\n': inc s.line
      # Skip */
      s.current += 2
    else:
      s.addToken(Slash)
  # Ignore whitespace
  of ' ', '\r', '\t': discard
  # Start of the string
  of '"': s.string()
  # Newline
  of '\n': inc s.line
  # Number literal
  of '0'..'9': s.number()
  # Identifier
  of Letters, '_': s.ident()
  else:
    error(s.line, "Unexpected character.")

proc scanTokens*(s: Scanner): seq[Token] = 
  while not s.isAtEnd():
    s.start = s.current
    s.scanToken()
  
  s.tokens.add initTok(Eof, line=s.line)
  return s.tokens