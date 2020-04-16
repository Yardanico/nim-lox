import ast, errors, tokens


type
  LoxValueKind = enum
    String, Number, Bool, Nil

  LoxValue = ref object
    case kind: LoxValueKind
    of String: strVal: string
    of Number: numVal: float
    of Bool: boolVal: bool
    of Nil: discard

type
  Interpreter* = ref object

proc isTruthy(v: LoxValue): bool =
  # Everything except Nil is true
  if v.kind == Nil: false
  elif v.kind == Bool: v.boolVal
  else: true

proc isEqual(l, r: LoxValue): bool = 
  # Nil is only equal to nil
  if l.kind == Nil and r.kind == Nil: true
  elif l.kind == Nil: false
  else: l == r

proc checkNumberOperand(op: Token, v: LoxValue) = 
  if v.kind == Number: return
  raise RuntimeError(tok: op, msg: "Operand must be a number.")

proc checkNumberOperands(op: Token, l, r: LoxValue) = 
  if l.kind == Number and r.kind == Number: return
  raise RuntimeError(tok: op, msg: "Operand must be a number.")

proc visit(i: Interpreter, e: Expr): LoxValue

proc `$`(v: LoxValue): string

proc visitBinary(i: Interpreter, e: Expr): LoxValue = 
  let left = i.visit(e.binLeft)
  let right = i.visit(e.binRight)
  result = LoxValue(kind: Number)
  
  case e.binOp.kind
  of Plus:
    # Overloading for numbers *AND* strings
    if left.kind == Number and right.kind == Number:
      return LoxValue(kind: Number, numVal: left.numVal + right.numVal)
    elif left.kind == String and right.kind == String:
      return LoxValue(kind: String, strVal: left.strVal & right.strVal)
    else:
      raise RuntimeError(
        tok: e.binOp, msg: "Operands must be two numbers or two strings."
      )
  # Arithmetics
  of Minus: 
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: Number, numVal: left.numVal - right.numVal)
  of Slash: 
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: Number, numVal: left.numVal / right.numVal)
  of Star: 
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: Number, numVal: left.numVal * right.numVal)
  # Comparison operators
  of Greater:
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: Bool, boolVal: left.boolVal > right.boolVal)
  of GreaterEqual:
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: Bool, boolVal: left.boolVal >= right.boolVal)
  of Less:
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: Bool, boolVal: left.boolVal > right.boolVal)
  of LessEqual:
    checkNumberOperands(e.binOp, left, right)
    return LoxValue(kind: Bool, boolVal: left.boolVal <= right.boolVal)
  # Equality
  of BangEqual:
    return LoxValue(kind: Bool, boolVal: not isEqual(left, right))
  of EqualEqual:
    return LoxValue(kind: Bool, boolVal: isEqual(left, right))
  else: return

proc visitGrouping(i: Interpreter, e: Expr): LoxValue = 
  i.visit(e.grpExpr)

proc visitTernary(i: Interpreter, e: Expr): LoxValue = 
  let val = i.visit(e.ternExpr)
  # Short-circuit evaluation
  result = 
    if val.isTruthy(): i.visit(e.ternTrue)
    else: i.visit(e.ternFalse)

proc visitLiteral(i: Interpreter, e: Expr): LoxValue = 
  case e.litKind
  of LitStr: LoxValue(kind: String, strVal: e.litStr)
  of LitNum: LoxValue(kind: Number, numVal: e.litNum)
  of LitBool: LoxValue(kind: Bool, boolVal: e.litBool)
  of LitNil: LoxValue(kind: Nil)

proc visitUnary(i: Interpreter, e: Expr): LoxValue = 
  let right = i.visit(e.unRight)
  case e.unOp.kind
  of Minus:
    checkNumberOperand(e.unOp, right)
    return LoxValue(kind: Number, numVal: -right.numVal)
  of Bang:
    return LoxValue(kind: Bool, boolVal: not isTruthy(right))
  else: assert false


proc visit(i: Interpreter, e: Expr): LoxValue = 
  case e.kind
  of Binary: i.visitBinary(e)
  of Grouping: i.visitGrouping(e)
  of Literal: i.visitLiteral(e)
  of Unary: i.visitUnary(e)
  of Ternary: i.visitTernary(e)
  else: LoxValue(kind: Nil)

proc `$`(v: LoxValue): string = 
  if v.kind == Nil: ""
  elif v.kind == Number:
    # A bit of a hack to output "integers" without decimal point
    if float(int(v.numVal)) == v.numVal: $int(v.numVal)
    else: $v.numVal
  elif v.kind == Bool: $v.boolVal
  elif v.kind == String: v.strVal
  # Can't happen
  else: return


proc interpret*(i: Interpreter, e: Expr) = 
  try:
    let val = i.visit(e)
    echo val
  except RuntimeError as err:
    errors.runtimeError(err)

proc newInterpreter*(): Interpreter = 
  Interpreter()