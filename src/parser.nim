import errors, ast, tokens

type 
  Parser = ref object
    tokens: seq[Token]
    current: int
    #loopDepth: int

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
  result = if p.isAtEnd(): false
  else: p.peek().kind == kind

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
proc functionBody(p; kind: string): Expr

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
  
  # Functions as expressions (anonymous functions)
  elif p.match(Fun):
    p.functionBody("function")

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

proc finishCall(p; e: Expr): Expr = 
  result = Expr(kind: Call, cCallee: e)
  # IF we have arguments
  if not p.check(RightParen):
    result.cArgs.add(p.expression())
    # There might be no commas if there only was 1 argument
    while p.match(Comma):
      # Max argument count
      if result.cArgs.len >= 255:
        errors.error(p.peek(), "Cannot have more than 255 arguments.")
      result.cArgs.add(p.expression())
  
  let paren = p.consume(RightParen, "Expect ')' after arguments.")

proc call(p): Expr = 
  result = p.primary()

  # fn(1)(2)(3)
  while true:
    if p.match(LeftParen):
      result = p.finishCall(result)
    else:
      break

proc unary(p): Expr = 
  # -a, !a
  result = if p.match(Bang, Minus):
    let op = p.previous()
    let right = p.unary()
    Expr(kind: Unary, unRight: right, unOp: op)
  else:
    p.call()

# TODO: Unify these 4 (via a template I guess)
proc multiplication(p): Expr = 
  # a * b, a / b
  result = p.unary()

  while p.match(Slash, Star):
    let op = p.previous()
    let right = p.unary()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right)

proc addition(p): Expr =
  # a + b, a - b
  result = p.multiplication()

  while p.match(Minus, Plus):
    let op = p.previous()
    let right = p.multiplication()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right)

proc comparison(p): Expr = 
  # a > b, b > c 
  result = p.addition()

  while p.match(Greater, GreaterEqual, Less, LessEqual):
    let op = p.previous()
    let right = p.addition()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right)

proc equality(p): Expr = 
  # true == false
  result = p.comparison()

  while p.match(BangEqual, EqualEqual):
    let op = p.previous()
    let right = p.comparison()
    result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right) 

proc andExpr(p): Expr = 
  # true and false;
  result = p.equality()

  while p.match(And):
    let op = p.previous()
    let right = p.equality()
    result = Expr(kind: Logical, logLeft: result, logOp: op, logRight: right)

proc orExpr(p): Expr = 
  # true or false;
  result = p.andExpr()

  while p.match(Or):
    let op = p.previous()
    let right = p.andExpr()
    result = Expr(kind: Logical, logLeft: result, logOp: op, logRight: right)

# ex.2 ch. 6
proc ternary(p): Expr = 
  # true? "yes": "no"
  result = p.orExpr()
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
  # a = 5
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
  # a = 1, b = 2, c = 3
  result = p.assignment()

  # TODO: Fix comma op?
  #while p.match(Comma):
  #  let op = p.previous()
  #  let right = p.assignment()
  #  result = Expr(kind: Binary, binLeft: result, binOp: op, binRight: right)

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

proc breakStmt(p): Stmt = 
  # break;
  p.consume(Semicolon, "Expect ';' after break statement.")
  #if p.loopDepth == 0:
  #  errors.error(p.previous(), "'break' can only used in loop context.")
  return Stmt(kind: BreakStmt, breakKwd: p.previous())

proc varDeclaration(p): Stmt = 
  let name = p.consume(Identifier, "Expect variable name")

  var initializer: Expr
  if p.match(Equal):
    initializer = p.expression()
  
  p.consume(Semicolon, "Expect ';' after variable declaration.")
  Stmt(kind: VarStmt, varName: name, varINit: initializer)

# Forward declarations
proc statement(p): Stmt
proc blockStmts(p): seq[Stmt]

proc functionBody(p; kind: string): Expr = 
  p.consume(LeftParen, "Expect '(' after " & kind & " name.")
  var params = newSeq[Token]()
  # If the function has parameters
  if not p.check(RightParen):
    # TODO: Make a "do while" template?
    params.add(p.consume(Identifier, "Expect parameter name."))
    while p.match(Comma):
      if params.len >= 255:
        errors.error(p.peek(), "Cannot have more than 255 parameters.")
      params.add(p.consume(Identifier, "Expect parameter name."))
  p.consume(RightParen, "Expect ')' after parameters.")

  p.consume(LeftBrace, "Expect '{' before " & kind & " body.")
  # Parse fun body
  let body = p.blockStmts()
  result = Expr(kind: Func, funParams: params, funStmts: body)

proc function(p; kind: string): Stmt = 
  let name = p.consume(Identifier, "Expect " & kind & " name.")
  Stmt(kind: FuncStmt, funName: name, funBody: p.functionBody("function"))

proc ifStmt(p): Stmt = 
  p.consume(LeftParen, "Expect '(' after 'if'.")
  let cond = p.expression()
  p.consume(RightParen, "Expect ')' after if condition.")

  let thenBranch = p.statement()
  var elseBranch: Stmt
  if p.match(Else):
    elseBranch = p.statement()

  Stmt(kind: IfStmt, ifCond: cond, ifThen: thenBranch, ifElse: elseBranch)

proc whileStmt(p): Stmt = 
  p.consume(LeftParen, "Expect '(' after 'while'.")
  let cond = p.expression()
  p.consume(RightParen, "Expect '(' after condition.")
  let body = p.statement()

  Stmt(kind: WhileStmt, whileCond: cond, whileBody: body)

proc forStmt(p): Stmt = 
  # For statements are sugar over while loops
  p.consume(LeftParen, "Expect '(' after 'for'.")

  # Optional initialization
  let init = if p.match(Semicolon):
    nil
  elif p.match(Var):
    p.varDeclaration()
  else:
    p.expressionStmt()
  
  # Optional condition
  var cond = if not p.check(Semicolon): p.expression() else: nil

  p.consume(Semicolon, "Expect ';' after loop condition.")

  # Optional increment
  let incr = if not p.check(Semicolon): p.expression() else: nil
  p.consume(RightParen, "Expect ')' after for clauses.")

  # For loop body
  result = p.statement()

  if not incr.isNil():
    # Add increment expression at the end of the loop body
    result = Stmt(
      kind: BlockStmt, 
      blockStmts: @[result, Stmt(kind: ExprStmt, expr: incr)]
    )
  
  if cond.isNil():
    # No condition -> always true
    cond = Expr(kind: Literal, litKind: LitBool, litBool: true)
  # Create a while statement
  result = Stmt(kind: WhileStmt, whileCond: cond, whileBody: result)

  if not init.isNil():
    # Add var initializer in the block before the loop body
    result = Stmt(kind: BlockStmt, blockStmts: @[init, result])

proc returnStmt(p): Stmt = 
  # returnStmt → "return" expression? ";" ;
  result = Stmt(kind: ReturnStmt, retKwd: p.previous())
  # If we have a return value
  if not p.check(Semicolon):
    result.retVal = p.expression()
  
  p.consume(Semicolon, "Expect ';' after return expression;")

proc statement(p): Stmt = 
  #try:
    #inc p.loopDepth
  result = if p.match(For): p.forStmt()
  elif p.match(While): p.whileStmt()
  #finally:
    #dec p.loopDepth
  elif p.match(If): p.ifStmt()
  elif p.match(Print): p.printStmt()
  elif p.match(Break): p.breakStmt()
  elif p.match(Return): p.returnStmt()
  # New block {}
  elif p.match(LeftBrace):
    Stmt(kind: BlockStmt, blockStmts: p.blockStmts())
  else:
    p.expressionStmt()

proc declaration(p): Stmt = 
  #[
    declaration → funDecl
            | varDecl
            | statement ;
  ]#
  try:
    if p.match(Var):
      result = p.varDeclaration()
    elif p.match(Fun):
      result = p.function("function")
    else:
      result = p.statement()
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

