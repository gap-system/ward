package = "LPEG"
version = "local-1"
source = {
  url = ""
}
description = {
  summary = "Parsing Expression Grammars For Lua (locally modified)",
  detailed = [[
    LPEG (http://www.inf.puc-rio.br/~roberto/lpeg) extended to
    support an unlimited number of runtime captures.
  ]],
  license = "MIT/X11"
}
dependencies = {
  "lua >= 5.1"
}
build = {
  type = "builtin",
  modules = {
    lpeg = {
      sources = { "lpeg.c" },
      defines = { "NDEBUG=1" },
    }
  },
}
