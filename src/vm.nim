import chunk, value, compiler

type
  VM* = ref object
    chunk: Chunk
    #ip: ref Opcode
    pc: int
    stack: array[256, Value]
    stackTop: int
  
  InterpretResult* = enum
    InterpretOk, InterpretCompileError, InterpretRuntimeError

var vm* = VM()

proc resetStack() = 
  vm.stackTop = 0

proc initVM*() = 
  resetStack()

proc freeVM*() = 
  discard

template top(): Value = 
  vm.stack[vm.stackTop]

proc push*(v: Value) = 
  vm.stack[vm.stackTop] = v
  inc vm.stackTop

proc pop(): Value = 
  dec vm.stackTop
  result = vm.stack[vm.stackTop]

proc run*(): InterpretResult = 
  template readByte: untyped = 
    inc vm.pc
    vm.chunk.code[vm.pc-1]
  
  template readConst: Value = 
    vm.chunk.consts.values[int(readByte())]
  
  template binOp(op) = 
    let b = pop()
    let a = pop()
    push(`op`(a, b))
  while true:
    let instr = Opcode(readByte())
    case instr
    of OpReturn:
      # For printing stuff because we don't yet have "print" stmt
      echo pop()
      return InterpretOk
    of OpConstant:
      push(readConst())
    of OpNegate:
      push(-pop())
    of OpAdd:
      binOp(`+`)
    of OpSubtract:
      binOp(`-`)
    of OpMultiply:
      binOp(`*`)
    of OpDivide:
      binOp(`/`)
    #else: discard

proc interpret*(src: string): InterpretResult = 
  var chunk = newChunk()

  if not compile(src, chunk):
    return InterpretCompileError
  
  vm.chunk = chunk
  vm.pc = 0

  result = run()