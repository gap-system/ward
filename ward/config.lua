config_track_actions = false

WriteGuards = {
  WriteGuard = true,
  ImpliedWriteGuard = true,
  WriteGuardBag = true,
  WriteGuardObj = true,
}

ReadGuards = {
  ReadGuard = true,
  ImpliedReadGuard = true,
  ReadGuardBag = true,
  ReadGuardObj = true,
}

preprocessor_defines = {
  HAVE_NATIVE_TLS = 1,
  CONFIG_H = 1,
  WARD_ENABLED = 1,
}

preprocessor_options = {
  "-D__extension__=",
  "-D__attribute__(x)=",
  -- Ignore C11 annotations.
  "-D_Alignas(x)=",
  -- TODO: The following is a workaround around __typeof() occurring
  -- in Linux header files. The correct solution is to properly deal
  -- with typeof() and __typeof() expressions.
  "-D__typeof(x)=int"
}

local R, W = "R", "W"
local VA = -1

Externals = {
  memcpy = { W, R, nil },
  memset = { W, nil, nil },
  read= { nil, R, nil, nil },
  write = { nil, W, nil, nil },
  bcopy = { R, W, nil },
  printf = { R, VA = R },
  sprintf = { R, R, VA = R },
  fprintf = { R, R, VA = R },
  fputs = { R, R },
  fgets = { R, W },
}
