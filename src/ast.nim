import tokens

type
  ExprKind* = enum
    Assign, Binary, Call, Get,
    Grouping, Literal, Ternary, Logical, Set,
    Super, This, Unary, Variable
  
  LiteralKind* = enum
    LitNum, LitStr, LitBool, LitNil

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
      of LitNum: litNum*: float
      of LitStr: litStr*: string
      of LitBool: litBool*: bool
      of LitNil: discard
    of Logical:
      logLeft*, logRight*: Expr
      logOp*: Token
    of Ternary:
      ternExpr*, ternTrue*, ternFalse*: Expr
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




# Example from the end of chapter 5 for lisp-printing the AST
when isMainModule:
  proc visit(e: Expr): string

  import sequtils, strutils

  proc parenthesize(e: Expr, name: string, exprs: openarray[Expr]): string = 
    result = $e.kind & "(" & name

    result &= exprs.mapIt(visit(it)).join(" ")
    result &= ")"

  proc visitBinary(e: Expr): string = 
    parenthesize(e, e.binOp.lexeme, [e.binLeft, e.binRight])

  proc visitGrouping(e: Expr): string = 
    parenthesize(e, "group", [e.grpExpr])

  proc visitTernary(e: Expr): string = 
    parenthesize(e, "", [e.ternExpr, e.ternTrue, e.ternFalse])

  proc visitLiteral(e: Expr): string = 
    case e.litKind
    of LitStr: e.litStr
    of LitNum: $e.litNum
    of LitBool: $e.litBool
    of LitNil: "nil"

  proc visitUnary(e: Expr): string = 
    parenthesize(e, e.unOp.lexeme, [e.unRight])




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
    of Ternary:
      e.visitTernary()
    else: ""

  proc `$`*(e: Expr): string = 
    e.visit()


  let expr = Expr(
    kind: Binary, binLeft: Expr(
      kind: Unary, unOp: Token(kind: Minus, lexeme: "-", line: 1), 
      unRight: Expr(kind: Literal, litKind: LitNum, litNum: 123)
    ),
    binOp: Token(kind: Star, lexeme: "*", line: 1),
    binRight: Expr(
      kind: Grouping, grpExpr: Expr(
        kind: Literal, litKind: LitNum, litNum: 45.67
      )
    )
  )

  echo expr