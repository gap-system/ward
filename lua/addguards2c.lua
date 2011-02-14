ward = os.getenv("WARD")
file = ...
guard = os.tmpname()
if string.sub(file, -2, -1) == ".c" then
  os.execute(ward .. "/bin/cward -suggest " .. file .. ">" .. guard)
  os.execute(ward .. "/bin/addguardsc " .. guard .. " " .. file)
  os.remove(guard)
else
  os.execute("cat "..file)
end
