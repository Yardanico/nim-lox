import std / strutils

type
  ValueKind* = enum
    ValBool, # A boolean
    ValNil, # A nil (empty) value
    ValNumber, # A (floating point) number
    ValObj # A heap-allocated object
  
  ## Represents a container for all
  ## value types which can be used in Lox
  Value* = object
    case kind*: ValueKind
    of ValBool: boolean*: bool
    of ValNumber: number*: float
    of ValObj: obj*: Obj
    of ValNil: discard
  
  ObjKind* = enum
    ObjString
  
  Obj* = object
    case kind*: ObjKind
    of ObjString: 
      # Uses Nim's own GC'd strings
      # Maybe use raw arrays later for optimization?
      str*: string
  
  ValueArray* = ref object
    values*: seq[Value]

using
  val: Value

template isBool*(val): bool = 
  val.kind == ValBool

template isNilVal*(val): bool = 
  val.kind == ValNil

template isNumber*(val): bool = 
  val.kind == ValNumber

template isObj*(val): bool = 
  val.kind == ValObj

proc isObjType*(val; kind: ObjKind): bool {.inline.} = 
  isObj(val) and val.obj.kind == kind

template isString*(val): bool = 
  isObj(val) and val.obj.kind == ObjString

template stringVal*(val): string = 
  val.obj.str

template boolVal*(val: bool): Value = 
  Value(kind: ValBool, boolean: val)

template nilVal*(): Value = 
  Value(kind: ValNil)

template numVal*(val: float): Value = 
  Value(kind: ValNumber, number: val)

template objVal*(val: Obj): Value = 
  Value(kind: ValObj, obj: val)

template stringObj*(data: string): Obj =
  Obj(kind: ObjString, str: data)

proc newValueArray*(): ValueArray = 
  ValueArray(values: @[])

proc writeValueArray*(arr: ValueArray, val) = 
  arr.values.add(val)


proc `$`*(val: Obj): string = 
  ## Convert an Obj to a string
  case val.kind
  of ObjString:
    val.str

proc `$`*(val): string = 
  ## Convert a Value to a string
  case val.kind
  of ValNumber:
    result = val.number.formatBiggestFloat(ffDecimal, -1)
    trimZeros(result)
  of ValBool:
    result = $val.boolean
  of ValNil:
    result = "nil"
  of ValObj:
    result = $val.obj

proc `==`*(a, b: Obj): bool = 
  ## Equality check between Obj instances
  if (a.kind != b.kind): false
  else:
    case a.kind
    of ObjString: a.str == b.str

proc `==`*(a, b: Value): bool = 
  ## Equality check between Value instances
  if (a.kind != b.kind): false
  else:
    case a.kind
    of ValBool: a.boolean == b.boolean
    of ValNil: true
    of ValNumber: a.number == b.number
    of ValObj: a.obj == b.obj