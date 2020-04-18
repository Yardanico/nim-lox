import strutils, tables

import ast

type
  Environment* = ref object
    values*: TableRef[string, LoxValue]
    enclosing*: Environment

  Interpreter* = ref object
    env*, globals*: Environment

  LoxValueKind* = enum
    StrVal, NumVal, BoolVal, CallableVal, NilVal
  
  LoxBuiltin* = proc (i: Interpreter, args: seq[LoxValue]): LoxValue

  LoxCallableKind* = enum
    Builtin, Code, Anonymous

  LoxCallable* = ref object
    arity*: int
    env*: Environment
    case kind*: LoxCallableKind
    of Builtin:
      bName*: string
      bCall*: LoxBuiltin
    of Code:
      cDecl*: Stmt
    of Anonymous:
      aDecl*: Expr

  LoxValue* = ref object
    case kind*: LoxValueKind
    of StrVal: strVal*: string
    of NumVal: numVal*: float
    of BoolVal: boolVal*: bool
    of CallableVal: calVal*: LoxCallable
    of NilVal: discard


proc `$`*(v: LoxValue): string = 
  ## Convert a LoxValue object to a string
  ## Used by the interpreter to stringify variables
  case v.kind
  of NilVal: ""
  of NumVal:
    var res = v.numVal.formatBiggestFloat(ffDecimal, -1)
    res.trimZeros()
    res
  of BoolVal: $v.boolVal
  of StrVal: v.strVal
  of CallableVal:
    case v.calVal.kind
    of Builtin: "<fn " & v.calVal.bName & ">"
    of Code: "<fn " & v.calVal.cDecl.funName.lexeme & ">"
    of Anonymous: "<fn>"