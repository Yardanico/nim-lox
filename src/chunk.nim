import value

type
  Opcode* = enum
    OpConstant = 0'u8, OpAdd, OpSubtract, 
    OpMultiply, OpDivide, OpReturn, OpNegate
  
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
  for n in countup(0, c.lines.len-1, 2):
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
    c.lines.add(line)
    c.lines.add(1)

proc writeChunk*(c; byt: uint8, line: int) = 
  c.code.add(byt)
  c.setLine(line)

proc addConstant*(c; value: Value): int = 
  writeValueArray(c.consts, value)
  result = c.consts.values.len - 1