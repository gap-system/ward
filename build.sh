#!/bin/sh
"exec" "tools/run-python" "$0" "$@"

import sys, os, shutil

def sh(cmd):
  print "SH: " + cmd
  if os.system(cmd):
    print "Command failed: " + cmd
    sys.exit(1)

def trysh(cmd):
  print "SH: " + cmd
  return not os.system(cmd)

root = os.getcwd()

if not os.path.exists(".ward_directory"):
  print "Must run " + __file__ + " from ward's main directory."
  sys.exit(1)

# Copy everything to a separate build directory
sh("rm -rf build")
sh("cp -pRf ext build")

# Build and install Lua
sh('tools/run-python scons/scons -j 4 -C build/lua-5.1.5 prefix="'+root+'/lua" install')

# Build and install LuaJIT
have_luajit = \
  trysh('make -j 4 -C build/LuaJIT-2.0.0 PREFIX="'+root+'/luajit" install')
if have_luajit:
  if sys.platform.startswith("cygwin"):
    shutil.copy2("build/LuaJIT-2.0.0/src/cyglua51.dll",
	"luajit/bin/")

# Build and install lpeg for both Lua and LuaJIT.
sh('tools/run-python scons/scons -j 4 -C build/lpeg-0.11 prefix="'+root+'/lua" install')
if have_luajit:
  sh('tools/run-python scons/scons -j 4 -C build/lpeg-0.11 luajit=yes prefix="'+root+'/luajit" install')
  

commands = [
  "ward", "cward", "addguards", "addguardsc", "addguards2", "addguards2c"
]

# If the directory exists, avoid aborting.
try: os.mkdir("bin")
except: pass

for cmd in commands:
  use_luajit = False
  if cmd.startswith("c"):
    use_luajit = True
    lua = cmd[1:] + ".lua"
  elif cmd.endswith("c"):
    use_luajit = True
    if cmd == "addguards2c":
      lua = cmd + ".lua"
    else:
      lua = cmd[:-1] + ".lua"
  else:
    lua = cmd + ".lua"
  cmdfile = open("bin/" + cmd, "w")
  # Will be a shell file.
  cmdfile.write("#!/bin/sh\n")
  # So that we know where we are.
  cmdfile.write("export WARD='" + root + "'\n")
  if have_luajit and use_luajit:
    path = "$WARD/luajit/bin/luajit -joff"
  else:
    path = "$WARD/lua/bin/lua"
  # Call Lua/LuaJIT with an expanded path and the lua
  # file as an argument.
  cmdfile.write(path + " -e \"package.path = \\\"$WARD/ward/?.lua;\\\" .. package.path\" " +
    "\"$WARD/ward/" + lua + "\" \"$@\"\n")
  cmdfile.close()
  os.chmod("bin/" + cmd, 0755)
