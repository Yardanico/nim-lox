version       = "0.1.0"
author        = "Danil Yarantsev (Yardanico)"
description   = "Lox interpreter written in Nim"
license       = "MIT"
srcDir        = "src"
bin           = @["lox"]
skipExt       = @["nim"]

# Dependencies
requires "nim >= 1.2.0"