import std / [lists, strformat]

import chunk, value, compiler

type
  VM* = ref object
    chunk: Chunk
    #ip: ref Opcode
    pc: int
    stack: array[256, Value]
    stackTop: int
    objects: SinglyLinkedList[Obj]
  
  InterpretResult* = enum
    InterpretOk, InterpretCompileError, InterpretRuntimeError

var vm* = VM()

proc resetStack() = 
  vm.stackTop = 0
  vm.objects = initSinglyLinkedList[Obj]()

proc runtimeError(msg: string) = 
  stderr.write(msg)
  stderr.write('\n')

  let instr = vm.pc - vm.chunk.code.len - 1
  let line = getLine(vm.chunk, instr)
  stderr.write(&"[line {line}] in script\n")

proc initVM*() = 
  resetStack()

proc freeVM*() = 
  # TODO: Freeing memory here (right now Nim's GC does it for us)
  discard
  vm.objects = initSinglyLinkedList[Obj]()
  #for obj in vm.objects:

template top(): Value = 
  vm.stack[vm.stackTop]

proc push*(v: Value) = 
  vm.stack[vm.stackTop] = v
  inc vm.stackTop

proc pop(): Value = 
  dec vm.stackTop
  result = vm.stack[vm.stackTop]

proc peek(dist: int): Value = 
  vm.stack[vm.stackTop - 1 - dist]

proc isFalse(val: Value): bool = 
  val.kind == ValNil or (val.kind == ValBool and not val.boolean)

proc concatenate() = 
  let b = pop().obj.str
  let a = pop().obj.str

  let res = stringObj(a & b)
  vm.objects.append(res)
  push(objVal(res))

proc run*(): InterpretResult = 
  template readByte: untyped = 
    inc vm.pc
    vm.chunk.code[vm.pc-1]
  
  template readConst: Value = 
    vm.chunk.consts.values[int(readByte())]
  
  template readConstLong: Value = 
    var arr = [readByte(), readByte(), readByte()]
    vm.chunk.consts.values[cast[int](arr)]

  template binOp(kind, op) = 
    if not isNumber(peek(0)) or not isNumber(peek(1)):
      runtimeError("Operands must be numbers.")
      return InterpretRuntimeError
    let b = pop().number
    let a = pop().number
    push(kind(`op`(a, b)))
  
  while true:
    # https://nim-lang.org/docs/manual.html#pragmas-computedgoto-pragma
    {.computedGoto.}
    let instr = Opcode(readByte())
    case instr
    of OpReturn:
      return InterpretOk
    of OpConstant: push(readConst())
    of OpConstantLong: push(readConstLong())

    of OpNil: push(nilVal())
    of OpTrue: push(boolVal(true))
    of OpFalse: push(boolVal(false))

    of OpEqual:
      let b = pop()
      let a = pop()
      push(boolVal(a == b))

    of OpNegate:
      if not isNumber(peek(0)):
        runtimeError("Operand must be a number.")
        return InterpretRuntimeError
      push(numVal(-pop().number))
    
    of OpGreater:
      binOp(boolVal, `>`)
    
    of OpLess:
      binOp(boolVal, `<`)

    of OpAdd:
      # String + string -> string
      if isString(peek(0)) and isString(peek(1)):
        concatenate()
      # NUmber + number -> number
      elif isNumber(peek(0)) and isNumber(peek(1)):
        let b = pop().number
        let a = pop().number
        push(numVal(a + b))
      else:
        runtimeError("Operands must be two numbers or two strings.")
        return InterpretRuntimeError
    of OpSubtract: binOp(numVal, `-`)
    of OpMultiply: binOp(numVal, `*`)
    of OpDivide: binOp(numVal, `/`)
    of OpNot:
      push(boolVal(isFalse(pop())))
    of OpIndexGet:
      if not isNumber(peek(0)) or not isString(peek(1)):
        runtimeError("Indexing is only supported for strings by numbers.")
        return InterpretRuntimeError
      let b = pop().number
      # If the float number can't be safely converted to an int
      if float(int(b)) != b:
        runtimeError("You can only use whole numbers for indexing.")
        return InterpretRuntimeError
      let a = pop().obj.str
      let res = stringObj($a[int(b)])
      vm.objects.append(res)
      push(objVal(res))
    
    of OpPrint:
      echo pop()
      
    #else: discard

proc interpret*(src: string): InterpretResult = 
  var chunk = newChunk()

  if not compile(src, chunk):
    return InterpretCompileError
  vm.chunk = chunk
  vm.pc = 0

  result = run()