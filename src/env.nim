import tables

import tokens, errors, types

using
  e: Environment

proc define*(e; name: string, val: LoxValue) = 
  e.values[name] = val

proc ancestor(e; dist: int): Environment = 
  result = e
  for i in 0 ..< dist:
    result = result.enclosing

proc get*(e; name: Token): LoxValue = 
  if name.lexeme in e.values:
    return e.values[name.lexeme]

  # Try to find variable in the outer(er) scope
  if e.enclosing != nil: return e.enclosing.get(name)

  raise RuntimeError(
    tok: name, msg: "Undefined variable '" & name.lexeme & "'."
  )

proc getAt*(e; dist: int, name: string): LoxValue = 
  e.ancestor(dist).values[name]

proc assignAt*(e; dist: int, name: Token, val: LoxValue) = 
  e.ancestor(dist).values[name.lexeme] = val

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