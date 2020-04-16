import token

type
  ExprKind* = enum
    Assign, Binary, Call, Get,
    Grouping, Literal, Logical, Set,
    Super, This, Unary, Variable
  
  LiteralKind* = enum
    Number, String, Boolean, Nil

  Expr* = ref object
    case kind*: ExprKind
    of Assign:
      asgnName*: Token
      asgnVal*: Expr
    of Binary:
      binLeft*, binRight*: Expr
      binOp*: Token
    of Call:
      cCallee*: Expr
      cParen*: Token
      cArgs*: seq[Expr]
    of Get:
      getObj*: Expr
      getName*: Token
    of Grouping:
      grpExpr*: Expr
    of Literal:
      case litKind*: LiteralKind
      of Number: numLit*: float
      of String: strLit*: string
      of Boolean: boolLit*: bool
      of Nil: discard
    of Logical:
      logLeft*, logRight*: Expr
      logOp*: Token
    of Set:
      setObj*, setVal*: Expr
      setName*: Token
    of Super:
      supKeyword*, supMethod*: Token
    of This:
      thisKeyword*: Token
    of Unary:
      unOp*: Token
      unRight*: Expr
    of Variable:
      varName*: Token
  
  StmtKind* = enum
    Block, Class, Expression, Function,
    If, Print, Return, Var, While
  
  Stmt* = ref object
    case kind*: StmtKind
    of Block:
      blockStmts*: seq[Stmt]
    of Class:
      clsName*: Token
      clsSuper*: Expr
      clsMethods*: seq[Stmt]
    of Expression:
      expr*: Expr
    of Function:
      funName*: Token
      funParams*: seq[Token]
      funBody*: seq[Stmt]
    of If:
      ifCond*: Expr
      ifThen*, ifElse*: Stmt
    of Print:
      prExpr*: Expr
    of Return:
      retKwd*: Token
      retVal*: Expr
    of Var:
      varName*: Token
      varInit*: Expr
    of While:
      whileCond*: Expr
      whileBody*: Stmt



when isMainModule:
  # Example from the end of chapter 5 for lisp-printing the AST
  proc visit(e: Expr): string

  proc parenthesize(name: string, exprs: openarray[Expr]): string = 
    result = "(" & name

    for expr in exprs:
      result &= " "
      result &= expr.visit()
    result &= ")"

  proc visitBinary(e: Expr): string = 
    parenthesize(e.binOp.lexeme, [e.binLeft, e.binRight])

  proc visitGrouping(e: Expr): string = 
    parenthesize("group", [e.grpExpr])

  proc visitLiteral(e: Expr): string = 
    case e.litKind
    of String: e.strLit
    of Number: $e.numLit
    else: ""

  proc visitUnary(e: Expr): string = 
    parenthesize(e.unOp.lexeme, [e.unRight])




  proc visit(e: Expr): string = 
    case e.kind
    of Binary:
      e.visitBinary()
    of Grouping:
      e.visitGrouping()
    of Literal:
      e.visitLiteral()
    of Unary:
      e.visitUnary()
    else: ""

  proc `$`(e: Expr): string = 
    e.visit()


  let expr = Expr(
    kind: Binary, binLeft: Expr(
      kind: Unary, unOp: Token(kind: Minus, lexeme: "-", line: 1), 
      unRight: Expr(kind: Literal, litKind: Number, numLit: 123)
    ),
    binOp: Token(kind: Star, lexeme: "*", line: 1),
    binRight: Expr(
      kind: Grouping, grpExpr: Expr(
        kind: Literal, litKind: Number, numLit: 45.67
      )
    )
  )

  echo expr