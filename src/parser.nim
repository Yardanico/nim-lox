import errors, ast, tokens

type 
  Parser = ref object
    tokens: seq[Token]
    current: int

  ParseError* = ref object of ValueError

using
  p: Parser

proc peek(p): Token = 
  p.tokens[p.current]

proc previous(p): Token = 
  p.tokens[p.current-1]

proc isAtEnd(p): bool = 
  p.peek().kind == Eof


proc advance(p): Token {.discardable.} = 
  if not p.isAtEnd():
    inc p.current
  return p.previous()

proc check(p; kind: TokenKind): bool = 
  if p.isAtEnd(): return
  p.peek().kind == kind

proc match(p; kinds: varargs[TokenKind]): bool = 
  for kind in kinds:
    if p.check(kind):
      p.advance()
      return true

proc error(tok: Token, msg: string): ParseError = 
  errors.error(tok, msg)

  return ParseError()

proc synchronize(p) = 
  p.advance()

  while not p.isAtEnd():
    if p.previous().kind == Semicolon: return

    case p.peek().kind
    # TODO: Make this line shorter
    of TokenKind.Class, TokenKind.Fun, TokenKind.Var, TokenKind.For, TokenKind.If, TokenKind.While, TokenKind.Print, TokenKind.Return: return
    else: discard
  
  p.advance()


proc consume(p; kind: TokenKind, msg: string): Token {.discardable.} = 
  if p.check(kind): p.advance()
  else: raise error(p.peek(), msg)

# Forward declarations
proc expression(p): Expr
proc equality(p): Expr
proc comparison(p): Expr
proc addition(p): Expr
proc multiplication(p): Expr

proc primary(p): Expr = 
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
  
  elif p.match(Identifier):
    Expr(kind: Variable, varName: p.previous())
  
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

proc unary(p): Expr = 
  if p.match(Bang, Minus):
    let op = p.previous()
    let right = p.unary()
    return Expr(kind: Unary, unRight: right, unOp: op)
  
  return p.primary()

# TODO: Unify these 4 (via a template I guess)
proc multiplication(p): Expr = 
  result = p.unary()

  while p.match(Slash, Star):
    let op = p.previous()
    let right = p.unary()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right)

proc addition(p): Expr = 
  result = p.multiplication()

  while p.match(Minus, Plus):
    let op = p.previous()
    let right = p.multiplication()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right)

proc comparison(p): Expr = 
  result = p.addition()

  while p.match(Greater, GreaterEqual, Less, LessEqual):
    let op = p.previous()
    let right = p.addition()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right)

proc equality(p): Expr = 
  result = p.comparison()

  while p.match(BangEqual, EqualEqual):
    let op = p.previous()
    let right = p.comparison()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right) 

# ex.2 ch. 6
proc ternary(p): Expr = 
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

proc assignment(p): Expr = 
  result = p.ternary()
  if p.match(Equal):
    let equals = p.previous()
    let val = p.assignment()

    if result.kind == Variable:
      let name = result.varName
      return Expr(kind: Assign, asgnName: name, asgnVal: val)
    
    errors.error(equals, "Invalid assignment target.")
  
# ex.1 ch. 6
proc comma(p): Expr = 
  result = p.assignment()

  while p.match(Comma):
    let op = p.previous()
    let right = p.assignment()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right)

proc expression(p): Expr = 
  p.comma()

proc printStmt(p): Stmt = 
  let val = p.expression()
  p.consume(Semicolon, "Expect ';' after value.")
  return Stmt(kind: PrintStmt, prExpr: val)

proc expressionStmt(p): Stmt = 
  let val = p.expression()
  p.consume(Semicolon, "Expect ';' after expression.")
  return Stmt(kind: ExprStmt, expr: val)

proc varDeclaration(p): Stmt = 
  let name = p.consume(Identifier, "Expect variable name")

  var initializer: Expr
  if p.match(Equal):
    initializer = p.expression()
  
  p.consume(Semicolon, "Expect ';' after variable declaration.")
  Stmt(kind: VarStmt, varName: name, varINit: initializer)

# Forward declaration
proc blockStmts(p): seq[Stmt]

proc statement(p): Stmt = 
  if p.match(Print):
    return p.printStmt()
  elif p.match(LeftBrace):
    return Stmt(kind: BlockStmt, blockStmts: p.blockStmts())
  return p.expressionStmt()

proc declaration(p): Stmt = 
  try:
    if p.match(Var):
      return p.varDeclaration()
    return p.statement()
  except ParseError as error:
    p.synchronize()

proc blockStmts(p): seq[Stmt] = 
  while not p.check(RightBrace) and not p.isAtEnd():
    result.add p.declaration()
  
  discard p.consume(RightBrace, "Expected '}' after block.")


proc parse*(p): seq[Stmt] = 
  while not p.isAtEnd():
    result.add p.declaration()

proc newParser*(tokens: seq[Token]): Parser = 
  Parser(tokens: tokens)

