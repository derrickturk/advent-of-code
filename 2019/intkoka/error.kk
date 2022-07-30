extend type exception-info
  IOError
  InvalidOpcode(op: int)
  InvalidProgram(path: string)
  MalformedInstruction(ip: int)
