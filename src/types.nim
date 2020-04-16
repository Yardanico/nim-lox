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
  if v.kind == NilVal: ""
  elif v.kind == NumVal:
    # A bit of a hack to output "integers" without decimal point
    if float(int(v.numVal)) == v.numVal: $int(v.numVal)
    else: $v.numVal
  elif v.kind == BoolVal: $v.boolVal
  elif v.kind == StrVal: v.strVal
  # Can't happen
  else: return