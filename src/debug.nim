import strformat, strutils

import chunk, value

proc disassembleInstruction(chunk: Chunk, offset: int): int

proc disassembleChunk*(chunk: Chunk, name: string) = 
  echo fmt"== {name} =="
  var offset = 0
  while offset < chunk.code.len:
    offset = disassembleInstruction(chunk, offset)

proc constantInstruction(name: string, chunk: Chunk, offset: int): int = 
  let val = chunk.code[offset + 1]
  echo fmt"{name:16} {val:4} '{chunk.consts.values[val]}'"
  offset + 2

proc simpleInstruction(name: string, offset: int): int = 
  echo name
  offset + 1

proc disassembleInstruction(chunk: Chunk, offset: int): int = 
  stdout.write(offset.intToStr(4))
  stdout.write(" ")
  let line = getLine(chunk, offset)
  let prevLine = getLine(chunk, offset - 1)

  if offset > 0 and line == prevLine:
    stdout.write("   | ")
  else:
    #stdout.write(line.intToStr(4))
    stdout.write(fmt"{line:4} ")
  
  let instr = chunk.code[offset]
  result = case Opcode(instr)
  of OpConstant: constantInstruction("OP_CONSTANT", chunk, offset)
  of OpReturn: simpleInstruction("OP_RETURN", offset)
  of OpNegate: simpleInstruction("OP_NEGATE", offset)
  of OpAdd: simpleInstruction("OP_ADD", offset)
  of OpSubtract: simpleInstruction("OP_SUBTRACT", offset)
  of OpMultiply: simpleInstruction("OP_MULTIPLY", offset)
  of OpDivide: simpleInstruction("OP_DIVIDE", offset)
  of OpNil: simpleInstruction("OP_NIL", offset)
  of OpTrue: simpleInstruction("OP_TRUE", offset)
  of OpFalse: simpleInstruction("OP_FALSE", offset)
  of OpNot: simpleInstruction("OP_NOT", offset)
  of OpEqual: simpleInstruction("OP_EQUAL", offset)
  of OpGreater: simpleInstruction("OP_GREATER", offset)
  of OpLess: simpleInstruction("OP_LESS", offset)
  of OpIndexGet: simpleInstruction("OP_INDEX_GET", offset)
  #else: 
  #  echo("Unknown opcode " & $instr & " ")
  #  offset + 1