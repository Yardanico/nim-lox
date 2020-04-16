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

using
  i: Interpreter
  op: Token
  v, l, r: LoxValue
  e: Expr

proc isTruthy(v): bool =
  # Everything except Nil is true
  if v.kind == Nil: false
  elif v.kind == Bool: v.boolVal
  else: true

proc `==`(l, r): bool = 
  # Nil is only equal to nil
  if l.kind == Nil and r.kind == Nil: true
  # Nil is always false
  elif l.kind == Nil: false
  elif l.kind == Number and r.kind == Number: l.numVal == r.numVal
  elif l.kind == String and r.kind == String: l.strVal == r.strVal
  elif l.kind == Bool and r.kind == Bool: l.boolVal == r.boolVal
  # Otherwise false
  else: false

proc checkNumberOperand(op, v) = 
  if v.kind == Number: return
  raise RuntimeError(tok: op, msg: "Operand must be a number.")

proc checkNumberOperands(op, l, r) = 
  if l.kind == Number and r.kind == Number: return
  raise RuntimeError(tok: op, msg: "Operand must be a number.")

proc visit(i, e): LoxValue

proc `$`(v): string

proc visitBinary(i, e): LoxValue = 
  let left = i.visit(e.binLeft)
  let right = i.visit(e.binRight)
  result = LoxValue(kind: Number)
  
  case e.binOp.kind
  of Plus:
    # Overloading for numbers *AND* strings
    if left.kind == Number and right.kind == Number:
      return LoxValue(kind: Number, numVal: left.numVal + right.numVal)
    # Ch. 7 Ex. 2 -> "scone" + 4 == "scone4"
    elif left.kind == String or right.kind == String:
      return LoxValue(kind: String, strVal: $left & $right)
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
    # Ch. 7 Ex. 3
    if right.numVal == 0:
      raise RuntimeError(tok: e.binOp, msg: "Division by zero.")
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
    return LoxValue(kind: Bool, boolVal: not (left == right))
  of EqualEqual:
    return LoxValue(kind: Bool, boolVal: left == right)
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
  of LitStr: LoxValue(kind: String, strVal: e.litStr)
  of LitNum: LoxValue(kind: Number, numVal: e.litNum)
  of LitBool: LoxValue(kind: Bool, boolVal: e.litBool)
  of LitNil: LoxValue(kind: Nil)

proc visitUnary(i, e): LoxValue = 
  let right = i.visit(e.unRight)
  case e.unOp.kind
  of Minus:
    checkNumberOperand(e.unOp, right)
    return LoxValue(kind: Number, numVal: -right.numVal)
  of Bang:
    return LoxValue(kind: Bool, boolVal: not isTruthy(right))
  else: assert false


proc visit(i, e): LoxValue = 
  case e.kind
  of Binary: i.visitBinary(e)
  of Grouping: i.visitGrouping(e)
  of Literal: i.visitLiteral(e)
  of Unary: i.visitUnary(e)
  of Ternary: i.visitTernary(e)
  else: LoxValue(kind: Nil)

proc `$`(v): string = 
  if v.kind == Nil: ""
  elif v.kind == Number:
    # A bit of a hack to output "integers" without decimal point
    if float(int(v.numVal)) == v.numVal: $int(v.numVal)
    else: $v.numVal
  elif v.kind == Bool: $v.boolVal
  elif v.kind == String: v.strVal
  # Can't happen
  else: return


proc interpret*(i, e) = 
  try:
    let val = i.visit(e)
    echo val
  except RuntimeError as err:
    errors.runtimeError(err)

proc newInterpreter*(): Interpreter = 
  Interpreter()