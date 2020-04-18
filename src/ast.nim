import tokens

type
  ExprKind* = enum
    Assign, Binary, Call, Func, Get,
    Grouping, Literal, Ternary, Logical, Set,
    Super, This, Unary, Variable
  
  LiteralKind* = enum
    LitNum, LitStr, LitBool, LitNil

  Expr* {.acyclic.} = ref object
    case kind*: ExprKind
    # Assigning name to a value
    of Assign:
      asgnName*: Token
      asgnVal*: Expr
    # Binary operators
    of Binary:
      binLeft*, binRight*: Expr
      binOp*: Token
    # Call
    of Call:
      cCallee*: Expr
      cParen*: Token
      cArgs*: seq[Expr]
    of Func:
      funParams*: seq[Token]
      funStmts*: seq[Stmt]
    of Get:
      getObj*: Expr
      getName*: Token
    # (expr)
    of Grouping:
      grpExpr*: Expr
    # Some literal
    of Literal:
      case litKind*: LiteralKind
      of LitNum: litNum*: float
      of LitStr: litStr*: string
      of LitBool: litBool*: bool
      of LitNil: discard
    # Logical op
    of Logical:
      logLeft*, logRight*: Expr
      logOp*: Token
    # Ternary operator true ? "a": "b"
    of Ternary:
      ternExpr*, ternTrue*, ternFalse*: Expr
    of Set:
      setObj*, setVal*: Expr
      setName*: Token
    of Super:
      supKeyword*, supMethod*: Token
    of This:
      thisKeyword*: Token
    # Unary operators like !a -a
    of Unary:
      unOp*: Token
      unRight*: Expr
    of Variable:
      varName*: Token
  
  StmtKind* = enum
    BlockStmt, ClassStmt, ExprStmt, FuncStmt,
    IfStmt, PrintStmt, ReturnStmt, VarStmt, WhileStmt,
    BreakStmt
  
  Stmt* {.acyclic.} = ref object
    case kind*: StmtKind
    of BlockStmt:
      blockStmts*: seq[Stmt]
    of ClassStmt:
      clsName*: Token
      clsSuper*: Expr
      clsMethods*: seq[Stmt]
    of ExprStmt:
      expr*: Expr
    of FuncStmt:
      funName*: Token
      funBody*: Expr
    of IfStmt:
      ifCond*: Expr
      ifThen*, ifElse*: Stmt
    of PrintStmt:
      prExpr*: Expr
    of ReturnStmt:
      retKwd*: Token
      retVal*: Expr
    of VarStmt:
      varName*: Token
      varInit*: Expr
    of WhileStmt:
      whileCond*: Expr
      whileBody*: Stmt
    of BreakStmt: discard




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