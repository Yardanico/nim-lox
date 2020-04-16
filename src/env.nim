import tables

import tokens, errors, types

type
  Environment* = ref object
    values: TableRef[string, LoxValue]
    enclosing: Environment

using
  e: Environment

proc define*(e; name: string, val: LoxValue) = 
  e.values[name] = val

proc get*(e; name: Token): LoxValue = 
  if name.lexeme in e.values:
    return e.values[name.lexeme]

  # Try to find variable in the outer(er) scope
  if e.enclosing != nil: return e.enclosing.get(name)

  raise RuntimeError(
    tok: name, msg: "Undefined variable '" & name.lexeme & "'."
  )

proc assign*(e; name: Token, val: LoxValue) = 
  if name.lexeme in e.values:
    e.values[name.lexeme] = val
    return

  # Try to assign a variable in the outer(er) scope
  # if it doesn't exist in our scope
  if e.enclosing != nil:
    e.enclosing.assign(name, val)
    return
  
  raise RuntimeError(tok: name, msg: "Undefined variable '" & name.lexeme & "'.")

proc newEnvironment*(enclosing: Environment = nil): Environment = 
  Environment(values: newTable[string, LoxValue](), enclosing: enclosing)