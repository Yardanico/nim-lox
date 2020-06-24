import std / [strformat, strutils]

import chunk, value

proc disassembleInstruction(chunk: Chunk, offset: int): int

proc disassemble*(chunk: Chunk, name: string) = 
  ## Disassembles a Chunk and prints all opcodes
  ## in a neatly-formatted way
  echo fmt"== {name} =="
  var offset = 0
  while offset < chunk.code.len:
    offset = disassembleInstruction(chunk, offset)

proc constantInstruction(name: string, chunk: Chunk, offset: int): int = 
  let idx = chunk.code[offset + 1]
  echo fmt"{name:16} {idx} '{chunk.consts.values[idx]}'"
  offset + 2

proc longConstInstruction(name: string, chunk: Chunk, offset: int): int = 
  # TODO: Simplify this
  let idx = cast[int]([chunk.code[offset + 1], chunk.code[offset+2], chunk.code[offset+3]])
  echo fmt"{name:16} {idx:4} '{chunk.consts.values[idx]}'"
  offset + 4

proc simpleInstruction(name: string, offset: int): int = 
  echo name
  offset + 1

proc disassembleInstruction(chunk: Chunk, offset: int): int = 
  ## Disassembles a single instruction from the Chunk with
  ## index ``offset``
  stdout.write(offset.intToStr(4))
  stdout.write(" ")
  let line = getLine(chunk, offset)
  let prevLine = getLine(chunk, offset - 1)

  if offset > 0 and line == prevLine:
    stdout.write("   | ")
  else:
    stdout.write(fmt"{line:4} ")
  
  let instr = chunk.code[offset]
  result = case Opcode(instr)
  of OpConstant: constantInstruction("OP_CONSTANT", chunk, offset)
  of OpConstantLong: longConstInstruction("OP_CONSTANT_LONG", chunk, offset)
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