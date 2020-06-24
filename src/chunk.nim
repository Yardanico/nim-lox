import value

type
  Opcode* = enum
    ## Stores a constant in a value array
    ## Next byte after that opcode is the index of the value in the array
    OpConstant = 0'u8,
    ## Stores a constant in a value array
    ## Next 3 (24 bits) bytes after that opcode are 
    ## the index of the value in the array so that opcode + number = 32 bits
    OpConstantLong,
    ## Represents nil (no value)
    OpNil, 
    ## Represents true
    OpTrue, 
    ## Represents false
    OpFalse,
    ## Pops two operands from the stack and checks if they're equal
    OpEqual, 
    ## Pops two operands from the stack and checks if the first operand 
    ## is greater than the second one
    OpGreater, 
    ## Same as OpGreater but in the other direction
    OpLess,
    ## Pops two operands from the stack (a string and a number) and tries
    ## to access character in the string by the index of the first operand
    OpIndexGet,
    ## Pops two operands and adds them
    OpAdd, 
    ## Pops two operands and subtracts them
    OpSubtract, 
    ## Pops two operands and multiplies them
    OpMultiply, 
    ## Pops two operands and divides them
    OpDivide,
    ## Pops one (bool) operand
    OpNot,
    ## Returns the first value on the stack
    OpReturn, 
    ## Negates one (number) operand
    OpNegate
  
  Chunk* = ref object
    # Sequence of all opcodes
    code*: seq[uint8]
    lines*: seq[int]
    consts*: ValueArray

using
  c: Chunk

proc newChunk*(): Chunk = 
  Chunk(consts: newValueArray())

proc getLine*(c; i: int): int = 
  var curInstr = 0
  for n in countup(0, c.lines.len - 1, 2):
    let cnt = c.lines[n]
    let line = c.lines[n + 1]

    if i >= (curInstr - cnt) and i < (curInstr + cnt):
      return line
    curInstr += cnt
  return -1

proc setLine*(c; line: int) = 
  if c.lines.len >= 2 and c.lines[^1] == line:
    # If it's not the first instruction and that line is already there
    inc c.lines[^2]
  else:
    # Add new line count to the seq
    c.lines.add(1)
    c.lines.add(line)

proc writeChunk*(c; byt: uint8, line: int) = 
  c.code.add(byt)
  c.setLine(line)

proc addConstant*(c; value: Value): int = 
  writeValueArray(c.consts, value)
  result = c.consts.values.len - 1