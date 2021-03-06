require "util"

function catfile(file)
  local fp = io.open(file)
  local contents = fp:read("*a")
  fp:close()
  io.write(contents)
end

ward = os.getenv("WARD")
args = table.concat(map({...}, shell_escape), " ")
argtable = {...}
file = argtable[#argtable]
if string.sub(file, -2, -1) == ".c" then
  guard = os.tmpname()
  if os.execute(ward .. "/bin/ward -suggest " .. args .. ">" .. guard) == 0 then
    io.write("/* This file was generated by Ward from " .. file .." */\n\n")
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
