#!/bin/sh
test -e .ward_directory || {
  echo "Not a ward installation"
  exit 1
}
rm -rf build bin lua luajit
