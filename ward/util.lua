local function Indent(depth)
  return string.rep("  ", depth)
end

local function IsEmpty(table)
  return next(table) == nil
end

local seen = { }

local function print_recursively(value, depth)
  local prefix = Indent(depth)
  if type(value) == "table" then
    local class_name = value.class_name
    if type(class_name) == "string" then
      io.write(class_name .. " ")
    end
    if IsEmpty(value) then
      io.write("{ }")
    elseif seen[value] then
      io.write("{ ... }")
    else
      seen[value] = true
      io.write("{\n")
      for k, v in pairs(value) do
	io.write(prefix .. "  ")
	if type(k) == "string" and string.match(k, "^%a[%a%d_]*$") then
	  io.write(k)
	else
	  print_recursively(k, depth+1)
	end
	io.write(" => ")
	print_recursively(v, depth+1)
	io.write("\n")
      end
      io.write(prefix .. "}")
    end
  elseif type(value) == "string" then
    io.write("\"")
    io.write(value)
    io.write("\"")
  else
    io.write(tostring(value))
  end
end

function pp(value)
  seen = { }
  print_recursively(value, 0)
  seen = nil
  io.write("\n")
end

function clone(table)
  local result = { }
  for k, v in pairs(table) do
    result[k] = v
  end
  return result
end

push = table.insert
pop = table.remove

function fatal_error(pos, message)
  print(message)
  os.exit(1)
end

function system_error(message)
  print(message)
  os.exit(1)
end

function map(table, mapfunc)
  local result = { }
  for k, v in pairs(table) do
    result[k] = mapfunc(v)
  end
  return result
end

function shell_escape(str)
  if str == "" then
    return "''"
  elseif string.match(str, "^[%.%=%-%s%w_/]+$") then
    return str
  else
    return string.gsub(str, "[^%.%=%-%s%w_/]", function(s)
      if s == "'" then
        return [["'"]]
      else
        return "'" .. s .. "'"
      end
    end)
  end
end

function read_from_command(command)
  local cmd = table.concat(map(command, shell_escape), " ")
  if options.debug then
    printerr([[popen: "]] .. cmd .. [["]])
  end
  local pipe = io.popen(cmd, "r")
  if not pipe and options.debug then
    printerr("popen: command failed")
    printerr("PATH: ".. os.getenv("PATH"))
  end
  local result = pipe:read("*a")
  --print(table.concat(map(command, shell_escape), " "))
  pipe:close()
  return result
end

function run_command(command)
  os.execute(table.concat(map(command, shell_escape), " "))
end

function chmod(mode, file)
  if type(mode) == "number" then
    mode = string.format("%04x", mode)
  end
  run_command({"chmod", mode, file})
end

function printerr(...)
  io.stderr:write(table.concat({...}, "\t").."\n")
end

local old_os_exit = os.exit
local exit_functions = { }

function os.exit(...)
  for _, func in ipairs(exit_functions) do
    func(...)
  end
  old_os_exit(...)
end

function at_exit(func)
  push(exit_functions, func)
end
