#!/bin/sh
export LUA_ROOT=`pwd`
case `uname -s` in
  Darwin)
    export PLATFORM=macosx
    ;;
  Linux)
    export PLATFORM=linux
    ;;
  *)
    export PLATFORM=posix
    ;;
esac
test -e .ward_directory && rm -rf build
test -e .ward_directory && cp -pr ext build
cd $LUA_ROOT/build/lua-5.1.4
make INSTALL_TOP=$LUA_ROOT $PLATFORM install
cd $LUA_ROOT/build/LuaJIT-2.0.0-beta6
make PREFIX=$LUA_ROOT install
cd $LUA_ROOT/bin
ln -sf luajit-2.0.0-beta6 luajit2
export PATH=$LUA_ROOT/bin:$PATH
cd $LUA_ROOT/build/luarocks-2.0.2
./configure
make install
cd $LUA_ROOT/build/lpeg
luarocks make lpeg-local-1.rockspec && cp lpeg.so $LUA_ROOT/lib/lua/5.1
cd $LUA_ROOT
cp -f build/luarocks-2.0.2/src/bin/luarocks-orig build/luarocks-2.0.2/src/bin/luarocks
cp -f build/luarocks-2.0.2/src/bin/luarocks-admin-orig build/luarocks-2.0.2/src/bin/luarocks-admin
LUA_PATH=`bin/lua -e 'print(package.path)'`
cat >bin/ward <<EOF
#!/bin/sh
export LUA_PATH='$LUA_ROOT/lua/?.lua;$LUA_PATH'
"$LUA_ROOT/bin/lua" "$LUA_ROOT/lua/ward.lua" "\$@"
EOF
chmod 755 bin/ward
LUAJIT_PATH=`bin/luajit2 -e 'print(package.path)'`
cat >bin/cward <<EOF
#!/bin/sh
export LUA_PATH='$LUA_ROOT/lua/?.lua;$LUAJIT_PATH'
"$LUA_ROOT/bin/luajit2" -joff "$LUA_ROOT/lua/ward.lua" "\$@"
EOF
chmod 755 bin/cward
cat >bin/addguards <<EOF
#!/bin/sh
export LUA_PATH='$LUA_ROOT/lua/?.lua;$LUA_PATH'
"$LUA_ROOT/bin/lua" "$LUA_ROOT/lua/addguards.lua" "\$@"
EOF
chmod 755 bin/addguards
cat >bin/addguardsc <<EOF
#!/bin/sh
export LUA_PATH='$LUA_ROOT/lua/?.lua;$LUAJIT_PATH'
"$LUA_ROOT/bin/luajit2" "$LUA_ROOT/lua/addguards.lua" "\$@"
EOF
chmod 755 bin/addguardsc
cat >bin/addguards2 <<EOF
#!/bin/sh
export LUA_PATH='$LUA_ROOT/lua/?.lua;$LUA_PATH'
export WARD=$LUA_ROOT
"$LUA_ROOT/bin/lua" "$LUA_ROOT/lua/addguards2.lua" "\$@"
EOF
chmod 755 bin/addguards2
cat >bin/addguards2c <<EOF
#!/bin/sh
export LUA_PATH='$LUA_ROOT/lua/?.lua;$LUAJIT_PATH'
export WARD=$LUA_ROOT
"$LUA_ROOT/bin/luajit2" "$LUA_ROOT/lua/addguards2c.lua" "\$@"
EOF
chmod 755 bin/addguards2c
chmod 755 bin/findguards
