import hashes

type
  TokenKind* = enum
    # Single-character tokens
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    QuestionMark, Colon

    # One or two character tokens
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    # Literals
    Identifier, String, Number,

    # Keywords
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While, Break

    Eof
  
  Token* = object
    case kind*: TokenKind
    # Different literals
    #of Identifier: ident: string
    of String: str*: string
    of Number: num*: float
    else: discard
    lexeme*: string
    line*: int

proc hash*(t: Token): Hash =
  var h: Hash = 0
  case t.kind
  of String: h = h !& hash(t.str)
  of Number: h = h !& hash(t.num)
  else: discard
  h = h !& hash(t.lexeme)
  h = h !& hash(t.line)
  result = !$h

proc `$`*(t: Token): string = 
  $t.kind & " " & t.lexeme

proc initTok*(kind: TokenKind, lexeme = "", line = 0): Token = 
  Token(kind: kind, lexeme: lexeme, line: line)

proc initStrTok*(val: string, line = 0): Token = 
  result = initTok(String, val, line)
  result.str = val

proc initNumTok*(val: float, line = 0, lexeme = $val): Token = 
  result = initTok(Number, lexeme, line)
  result.num = val

#proc initIdentTok*(val: string, line = 0): Token = 
#  Token(kind: Identifier, ident: val, lexeme: val)