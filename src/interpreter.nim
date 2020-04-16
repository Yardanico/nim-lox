import ast, errors, tokens, env, types

type
  Interpreter* = ref object
    env: Environment

using
  i: Interpreter
  op: Token
  v, l, r: LoxValue
  e: Expr
  s: Stmt

proc isTruthy(v): bool =
  # Everything except Nil is true
  if v.kind == NilVal: false
  elif v.kind == BoolVal: v.boolVal
  else: true

proc `==`(l, r): bool = 
  # Nil is only equal to nil
  if l.kind == NilVal and r.kind == NilVal: true
  # Comparing to Nil is always false otherwise
  elif l.kind == NilVal or r.kind == NilVal: false
  elif l.kind == NumVal and r.kind == NumVal: l.numVal == r.numVal
  elif l.kind == StrVal and r.kind == StrVal: l.strVal == r.strVal
  elif l.kind == BoolVal and r.kind == BoolVal: l.boolVal == r.boolVal
  # Otherwise false
  else: false

proc checkNumberOperand(op, v) = 
  if v.kind == NumVal: return
  raise RuntimeError(tok: op, msg: "Operand must be a number.")

proc checkNumberOperands(op, l, r) = 
  if l.kind == NumVal and r.kind == NumVal: return
  raise RuntimeError(tok: op, msg: "Operand must be a number.")

proc visit(i, e): LoxValue

proc visitBinary(i, e): LoxValue = 
  let left = i.visit(e.binLeft)
  let right = i.visit(e.binRight)
  result = LoxValue(kind: NumVal)
  
  case e.binOp.kind
  of Plus:
    # Overloading for numbers *AND* strings
    if left.kind == NumVal and right.kind == NumVal:
      return LoxValue(kind: NumVal, numVal: left.numVal + right.numVal)
    # Ch. 7 Ex. 2 -> "scone" + 4 == "scone4"
    # TODO: Decide if we really want that, and if yes, do a better
    # error message
    elif left.kind == StrVal or right.kind == StrVal:
      return LoxValue(kind: StrVal, strVal: $left & $right)
    else:
      raise RuntimeError(
        tok: e.binOp, msg: "Operands must be two numbers or two strings."
      )
  # Arithmetics
  of Minus: 
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: NumVal, numVal: left.numVal - right.numVal)
  of Slash: 
    checkNumberOperands(e.binOp, left, right)
    # Ch. 7 Ex. 3
    if right.numVal == 0:
      raise RuntimeError(tok: e.binOp, msg: "Division by zero.")
    return LoxValue(kind: NumVal, numVal: left.numVal / right.numVal)
  of Star: 
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: NumVal, numVal: left.numVal * right.numVal)
  # Comparison operators
  of Greater:
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: BoolVal, boolVal: left.boolVal > right.boolVal)
  of GreaterEqual:
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: BoolVal, boolVal: left.boolVal >= right.boolVal)
  of Less:
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: BoolVal, boolVal: left.boolVal > right.boolVal)
  of LessEqual:
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: BoolVal, boolVal: left.boolVal <= right.boolVal)
  # Equality
  of BangEqual:
    return LoxValue(kind: BoolVal, boolVal: not (left == right))
  of EqualEqual:
    return LoxValue(kind: BoolVal, boolVal: left == right)
  else: return

proc visitGrouping(i, e): LoxValue = 
  i.visit(e.grpExpr)

proc visitTernary(i, e): LoxValue = 
  let val = i.visit(e.ternExpr)
  # Short-circuit evaluation
  result = 
    if val.isTruthy(): i.visit(e.ternTrue)
    else: i.visit(e.ternFalse)

proc visitLiteral(i, e): LoxValue = 
  case e.litKind
  of LitStr: LoxValue(kind: StrVal, strVal: e.litStr)
  of LitNum: LoxValue(kind: NumVal, numVal: e.litNum)
  of LitBool: LoxValue(kind: BoolVal, boolVal: e.litBool)
  of LitNil: LoxValue(kind: NilVal)

proc visitUnary(i, e): LoxValue = 
  let right = i.visit(e.unRight)
  case e.unOp.kind
  of Minus:
    checkNumberOperand(e.unOp, right)
    return LoxValue(kind: NumVal, numVal: -right.numVal)
  of Bang:
    return LoxValue(kind: BoolVal, boolVal: not isTruthy(right))
  else: assert false

proc visitAssignExpr(i, e): Loxvalue = 
  result = i.visit(e.asgnVal)

  i.env.assign(e.asgnName, result)

proc visitVariableExpr(i, e): LoxValue = 
  i.env.get(e.varName)

proc visit(i, e): LoxValue = 
  case e.kind
  of Binary: i.visitBinary(e)
  of Grouping: i.visitGrouping(e)
  of Literal: i.visitLiteral(e)
  of Unary: i.visitUnary(e)
  of Ternary: i.visitTernary(e)
  of Variable: i.visitVariableExpr(e)
  of Assign: i.visitAssignExpr(e)
  else: LoxValue(kind: NilVal)

proc visitVarStmt(i, s) = 
  var value: LoxValue
  if s.varInit != nil:
    value = i.visit(s.varInit)
  i.env.define(s.varName.lexeme, value)

proc visitExpressionStmt(i, s) = 
  discard i.visit(s.expr)

proc visitPrintStmt(i, s) = 
  let value = i.visit(s.prExpr)
  echo value

proc visit(i, s)

proc execute(i, s) =
  i.visit(s)

proc executeBlock(i; stmts: seq[Stmt], env: Environment) = 
  let prev = i.env
  try:
    i.env = env
    for stmt in stmts:
      i.execute(stmt)
  finally:
    i.env = prev

proc visitBlockStmt(i, s) = 
  i.executeBlock(s.blockStmts, newEnvironment(i.env))

proc visit(i, s) = 
  case s.kind
  of PrintStmt: i.visitPrintStmt(s)
  of ExprStmt: i.visitExpressionStmt(s)
  of VarStmt: i.visitVarStmt(s)
  of BlockStmt: i.visitBlockStmt(s)
  else: discard

proc interpret*(i; stmts: seq[Stmt]) = 
  try:
    for stmt in stmts:
      i.execute(stmt)
  except RuntimeError as err:
    errors.runtimeError(err)



proc newInterpreter*(env: Environment): Interpreter = 
  Interpreter(env: env)