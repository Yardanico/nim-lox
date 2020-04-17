import strutils

type
  LoxValueKind* = enum
    StrVal, NumVal, BoolVal, NilVal

  LoxValue* = ref object
    case kind*: LoxValueKind
    of StrVal: strVal*: string
    of NumVal: numVal*: float
    of BoolVal: boolVal*: bool
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