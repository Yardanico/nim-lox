
import tables, deques

import ast, types, tokens, errors, interpreter

type
  FunctionType = enum
    None, Function

  LoopType = enum
    LoopNone, LoopWhile

  Resolver* = ref object
    interp: Interpreter
    scopes: Deque[TableRef[string, bool]]
    curFunc: FunctionType
    curLoop: LoopType

using
  r: Resolver
  i: Interpreter
  e: Expr
  s: Stmt

# addLast
# popFirst
proc newResolver*(i): Resolver = 
  result = Resolver(interp: i)
  result.scopes = initDeque[TableRef[string, bool]]()

proc visit(r, s)
proc visit(r, e)

proc beginScope(r) = 
  r.scopes.addFirst(newTable[string, bool]())

proc endScope(r) = 
  r.scopes.popFirst()

proc resolve(r, s) = 
  r.visit(s)

proc resolve(r, e) = 
  r.visit(e)

template withScope(r; body: untyped): untyped = 
  r.beginScope()
  body
  r.endScope()



proc resolve*(r; stmts: seq[Stmt]) = 
  for stmt in stmts:
    r.resolve(stmt)

proc resolveLocal(r, e; name: Token) = 
  for i in 0 ..< r.scopes.len:
    if name.lexeme in r.scopes[i]:
      r.interp.resolve(e, i)
      break
  # Assume it is global

proc define(r; name: Token) = 
  if r.scopes.len != 0:
    r.scopes[0][name.lexeme] = true

proc declare(r; name: Token) = 
  if r.scopes.len != 0:
    if name.lexeme in r.scopes.peekFirst():
      errors.error(name, "Variable with this name is already declared in this scope.")
    r.scopes[0][name.lexeme] = false

proc visitVarStmt(r, s) = 
  r.declare(s.varName)
  if not s.varInit.isNil():
    r.resolve(s.varInit)
  r.define(s.varName)

proc resolveFunction(r, s; t: FunctionType) = 
  let enclosing = r.curFunc
  r.curFunc = t

  r.withScope:
    for token in s.funBody.funParams:
      r.declare(token)
      r.define(token)
    r.resolve(s.funBody.funStmts)

  r.curFunc = enclosing

proc visitFunctionStmt(r, s) = 
  r.declare(s.funName)
  r.define(s.funName)

  r.resolveFunction(s, Function)

proc visitBlockStmt(r, s) = 
  r.withScope:
    r.resolve(s.blockStmts)

proc visitExpressionStmt(r, s) = 
  r.resolve(s.expr)

proc visitIfStmt(r, s) = 
  r.resolve(s.ifCond)
  r.resolve(s.ifThen)
  if not s.ifElse.isNil(): r.resolve(s.ifElse)

proc visitPrintStmt(r, s) = 
  r.resolve(s.prExpr)

proc visitReturnStmt(r, s) = 
  if r.curFunc == None:
    errors.error(s.retKwd, "Cannot return from top-level code.")
  if not s.retVal.isNil():
    r.resolve(s.retVal)

proc visitWhileStmt(r, s) = 
  let loop = r.curLoop
  r.curLoop = LoopWhile

  r.resolve(s.whileCond)
  r.resolve(s.whileBody)

  r.curLoop = loop

proc visitBreakStmt(r, s) = 
  if r.curLoop == LoopNone:
    errors.error(s.breakKwd, "'break' can only be used in loop context.")

proc visit(r, s) = 
  case s.kind
  of PrintStmt: r.visitPrintStmt(s)
  of ExprStmt: r.visitExpressionStmt(s)
  of VarStmt: r.visitVarStmt(s)
  of BlockStmt: r.visitBlockStmt(s)
  of IfStmt: r.visitIfStmt(s)
  of WhileStmt: r.visitWhileStmt(s)
  of BreakStmt: r.visitBreakStmt(s)
  of FuncStmt: r.visitFunctionStmt(s)
  of ReturnStmt: r.visitReturnStmt(s)
  else: discard

proc visitVariableExpr(r, e) = 
  let n = e.varName.lexeme
  if r.scopes.len != 0 and n in r.scopes[0] and r.scopes[0][n] == false:
    errors.error(e.varName, "Cannot read local variable in its own initializer.")
  r.resolveLocal(e, e.varName)

proc visitAssignExpr(r, e) = 
  r.resolve(e.asgnVal)
  r.resolveLocal(e, e.asgnName)

proc visitBinary(r, e) = 
  r.resolve(e.binLeft)
  r.resolve(e.binRight)

proc visitCallExpr(r, e) = 
  r.resolve(e.cCallee)
  for arg in e.cArgs:
    r.resolve(arg)

proc visitGrouping(r, e) = 
  r.resolve(e.grpExpr)

proc visitLiteral(r, e) = 
  discard

proc visitLogicalExpr(r, e) = 
  r.resolve(e.logLeft)
  r.resolve(e.logRight)

proc visitUnary(r, e) = 
  r.resolve(e.unRight)

proc visitTernary(r, e) = 
  r.resolve(e.ternExpr)
  r.resolve(e.ternFalse)
  r.resolve(e.ternTrue)


proc visitFuncExpr(r, e) = 
  let enclosingFunc = r.curFunc
  r.curFunc = Function

  r.withScope:
    for token in e.funParams:
      r.declare(token)
      r.define(token)
    r.resolve(e.funStmts)

  r.curFunc = enclosingFunc


proc visit(r, e) = 
  case e.kind
  of Binary: r.visitBinary(e)
  of Grouping: r.visitGrouping(e)
  of Literal: r.visitLiteral(e)
  of Unary: r.visitUnary(e)
  of Ternary: r.visitTernary(e)
  of Variable: r.visitVariableExpr(e)
  of Assign: r.visitAssignExpr(e)
  of Logical: r.visitLogicalExpr(e)
  of Func: r.visitFuncExpr(e)
  of Call: r.visitCallExpr(e)
  else: discard