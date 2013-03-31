#!/usr/bin/env python
"exec" "python" "$0" "$@"

import sys, os, shutil

def sh(cmd):
  print "SH: " + cmd
  if os.system(cmd):
    print "Command failed: " + cmd
    sys.exit(1)

root = os.getcwd()

if not os.path.exists(".ward_directory"):
  print "Must run " + __file__ + " from ward's main directory."
  sys.exit(1)

# Copy everything to a separate build directory
sh("cp -prf ext build")

# Build and install Lua
sh('scons/scons -j 4 -C build/lua-5.1.5 prefix="'+root+'/lua" install')

# Build and install LuaJIT
sh('make -j 4 -C build/LuaJIT-2.0.0 PREFIX="'+root+'/luajit" install')
if sys.platform.startswith("cygwin"):
  shutil.copy2("build/LuaJIT-2.0.0/src/cyglua51.dll",
      "luajit/bin/")

# Build and install lpeg for both Lua and LuaJIT.
sh('scons/scons -j 4 -C build/lpeg-0.11 prefix="'+root+'/lua" install')
sh('scons/scons -j 4 -C build/lpeg-0.11 luajit=yes prefix="'+root+'/luajit" install')

commands = [
  "ward", "cward", "addguards", "addguardsc", "addguards2", "addguards2c"
]

# If the directory exists, avoid aborting.
sh('mkdir bin || true')

for cmd in commands:
  luajit = False
  if cmd.startswith("c"):
    luajit = True
    lua = cmd[1:] + ".lua"
  elif cmd.endswith("c"):
    luajit = True
    lua = cmd[:-1] + ".lua"
  else:
    lua = cmd + ".lua"
  with open("bin/" + cmd, "w") as cmdfile:
    # Will be a shell file.
    cmdfile.write("#!/bin/sh\n")
    # So that we know where we are.
    cmdfile.write("export WARD='" + root + "'\n")
    if luajit:
      path = root + "/luajit/bin/luajit"
    else:
      path = root + "/lua/bin/lua"
    # Call Lua/LuaJIT with an expanded path and the lua
    # file as an argument.
    cmdfile.write(path + " -e 'package.path = \"" +
      root + "/ward/?.lua;\" .. package.path' " +
      "'" + root + "/ward/" + lua + "' \"$@\"\n")
  os.chmod("bin/" + cmd, 0755)
