function catfile(file)
  local fp = io.open(file)
  local contents = fp:read("*a")
  fp:close()
  io.write(contents)
end

ward = os.getenv("WARD")
args = table.concat({...}, " ")
argtable = {...}
file = argtable[#argtable]
if string.sub(file, -2, -1) == ".c" then
  guard = os.tmpname()
  if os.execute(ward .. "/bin/ward -suggest " .. args .. ">" .. guard) == 0 then
    io.write("/***********************************************************/\n")
    io.write("/*                                                         */\n")
    io.write("/*       NOTE: This file was preprocessed by Ward.         */\n")
    io.write("/*       Do not edit it; changes will be lost.             */\n")
    io.write("/*                                                         */\n")
    io.write("/***********************************************************/\n")
    io.write("\n")
    io.flush()
    os.execute(ward .. "/bin/addguards " .. guard .. " " .. file)
  else
    io.stderr:write("ERROR: Ward failed to parse C source\n")
    os.remove(guard)
    os.exit(1)
  end
  os.remove(guard)
else
  catfile(file)
end
