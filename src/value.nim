import strutils

type
  Value* = float
  ValueArray* = ref object
    values*: seq[Value]


proc newValueArray*(): ValueArray = 
  ValueArray(values: @[])

proc writeValueArray*(arr: ValueArray, val: Value) = 
  arr.values.add(val)

#proc `$`*(val: Value): string = 
#  result = val.formatBiggestFloat(ffDecimal, -1)
#  trimZeros(result)