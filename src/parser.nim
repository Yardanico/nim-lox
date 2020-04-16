import errors, ast, tokens

type 
  Parser = ref object
    tokens: seq[Token]
    current: int
  
  ParseError* = ref object of ValueError

proc peek(p: Parser): Token = 
  p.tokens[p.current]

proc previous(p: Parser): Token = 
  p.tokens[p.current-1]

proc isAtEnd(p: Parser): bool = 
  p.peek().kind == Eof


proc advance(p: Parser): Token {.discardable.} = 
  if not p.isAtEnd():
    inc p.current
  return p.previous()

proc check(p: Parser, kind: TokenKind): bool = 
  if p.isAtEnd(): return
  p.peek().kind == kind

proc match(p: Parser, kinds: varargs[TokenKind]): bool = 
  for kind in kinds:
    if p.check(kind):
      p.advance()
      return true

proc error(tok: Token, msg: string): ParseError = 
  errors.error(tok, msg)

  return ParseError()

proc synchronize(p: Parser) = 
  p.advance()

  while not p.isAtEnd():
    if p.previous().kind == Semicolon: return

    case p.peek().kind
    # TODO: Make this line shorter
    of TokenKind.Class, TokenKind.Fun, TokenKind.Var, TokenKind.For, TokenKind.If, TokenKind.While, TokenKind.Print, TokenKind.Return: return
    else: discard
  
  p.advance()


proc consume(p: Parser, kind: TokenKind, msg: string): Token {.discardable.} = 
  if p.check(kind): p.advance()
  else: raise error(p.peek(), msg)

# Forward declarations
proc expression(p: Parser): Expr
proc equality(p: Parser): Expr
proc comparison(p: Parser): Expr
proc addition(p: Parser): Expr
proc multiplication(p: Parser): Expr

proc primary(p: Parser): Expr = 
  # TODO: Use case statement here
  if p.match(False, True):
    Expr(
      kind: Literal, litKind: LitBool, 
      litBool: if p.previous().kind == False: false else: true
    )
  elif p.match(Nil):
    Expr(kind: Literal, litKind: LitNil)

  elif p.match(Number):
    Expr(kind: Literal, litKind: LitNum, litNum: p.previous().num)
  elif p.match(String):
    Expr(kind: Literal, litKind: LitStr, litStr: p.previous().str)
  elif p.match(LeftParen):
    let expr = p.expression()
    p.consume(RightParen, "Expect ')' after expression.")
    Expr(kind: Grouping, grpExpr: expr)
  elif p.match(BangEqual, EqualEqual):
    errors.error(p.previous(), "Missing left-hand operand.")
    discard p.equality()
    Expr()
  elif p.match(Greater, GreaterEqual, Less, LessEqual):
    errors.error(p.previous(), "Missing left-hand operand.")
    discard p.comparison()
    Expr()
  elif p.match(Plus, EqualEqual):
    errors.error(p.previous(), "Missing left-hand operand.")
    discard p.addition()
    Expr()
  elif p.match(Slash, Star):
    errors.error(p.previous(), "Missing left-hand operand.")
    discard p.multiplication()
    Expr()
  else:
    raise error(p.peek(), "Expect expression.")

proc unary(p: Parser): Expr = 
  if p.match(Bang, Minus):
    let op = p.previous()
    let right = p.unary()
    return Expr(kind: Unary, unRight: right, unOp: op)
  
  return p.primary()

# TODO: Unify these 4 (via a template I guess)
proc multiplication(p: Parser): Expr = 
  result = p.unary()

  while p.match(Slash, Star):
    let op = p.previous()
    let right = p.unary()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right)

proc addition(p: Parser): Expr = 
  result = p.multiplication()

  while p.match(Minus, Plus):
    let op = p.previous()
    let right = p.multiplication()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right)

proc comparison(p: Parser): Expr = 
  result = p.addition()

  while p.match(Greater, GreaterEqual, Less, LessEqual):
    let op = p.previous()
    let right = p.addition()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right)

proc equality(p: Parser): Expr = 
  result = p.comparison()

  while p.match(BangEqual, EqualEqual):
    let op = p.previous()
    let right = p.comparison()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right) 

# ex.2 ch. 6
proc ternary(p: Parser): Expr = 
  result = p.equality()
  while p.match(QuestionMark):
    let trueBranch = p.ternary()
    p.consume(Colon, "Expected a colon for ternary operator")
    let falseBranch = p.ternary()
    result = Expr(
      kind: Ternary,
      ternExpr: result,
      ternTrue: trueBranch,
      ternFalse: falseBranch
    )

# ex.1 ch. 6
proc comma(p: Parser): Expr = 
  result = p.ternary()

  while p.match(Comma):
    let op = p.previous()
    let right = p.equality()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right)

proc expression(p: Parser): Expr = 
  p.comma()

proc parse*(p: Parser): Expr = 
  try:
    return p.expression()
  except ParseError:
    return Expr()

proc newParser*(tokens: seq[Token]): Parser = 
  Parser(tokens: tokens)

