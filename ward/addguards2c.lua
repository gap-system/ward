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
  if os.execute(ward .. "/bin/cward -suggest " .. args .. ">" .. guard) == 0 then
    os.execute(ward .. "/bin/addguardsc " .. guard .. " " .. file)
  else
    io.write("/* ERROR: Ward failed to parse C source */\n")
    catfile(file)
  end
  os.remove(guard)
else
  catfile(file)
end
