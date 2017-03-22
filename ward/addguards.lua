lpeg = require "lpeg"
lpeg.setmaxstack(1000)

-- Some more readable versions of the lpeg primitives.

local pattern = lpeg.P -- match a literal
local range = lpeg.R -- match a range of characters
local rule = lpeg.V -- match a non-terminal in a grammar
local error_pos = 0
local function match(pattern, string)
  error_pos = 0
  a = (lpeg.match(pattern, string) or false)
  return a, error_pos
end

-- lpeg matches strings to arbitrary semantic values via a 'capture'
-- mechanism. Each rule can be enclosed by a capture mapping that
-- maps the string it matches plus all captures contained in subrules
-- to a new capture.
--
-- The simplest capture just maps a string to itself. They are used,
-- for instance, to return the name of a matched identifier.
--
-- Table captures ignore the string matched by the rule and map it
-- to a table of all captures produced by subrules. This is, for
-- example, for evaluating a comma-separated list; it will evaluate
-- to a table containing the captures for all the list items, but
-- ignore the string itself, including the semantically meaningless
-- comma separators.
--
-- Match time or function captures allow arbitrary Lua functions
-- to be written that match a string and subrule captures to a
-- result capture. They are used to build abstract syntax trees
-- or to evaluate expressions, for example.
--
-- Constant captures return a fixed result, regardless of the input
-- string. They are used, among other things, to inject constant
-- values into a table capture without having to write a more
-- complex function capture.

local action = lpeg.Cmt -- "match time capture", evaluated via a function
local capture = lpeg.C -- direct capture, just captures the match
local aggregate = lpeg.Ct -- returns all contained captures in a table
local value = lpeg.Cc -- constant capture, returns a fixed value
local position = lpeg.Cp -- position capture, returns current position

local function track(pattern, tag)
  return action(pattern, function(string, pos, match)
    if tag then
      print(string.format("%s -> %d %s", tag, pos, tostring(match)))
    else
      print(string.format("-> %d %s", pos, match))
    end
    return pos
  end)
end

-- code to track action overhead, disabled by default
if config_track_actions then
  local action_counter = { }
  local action_lines = { }

  function action(pattern, func)
    local line = debug.getinfo(2).currentline
    action_counter[func] = 0
    action_lines[func] = line
    return lpeg.Cmt(pattern, function(...)
      action_counter[func] = action_counter[func] + 1
      return func(...)
    end)
  end

  at_exit(function()
    local a = { }
    for func, count in pairs(action_counter) do
      push(a, { func, count, action_lines[func] })
    end
    table.sort(a, function(a, b) return a[2] < b[2] end)
    pp(a)
  end)
end

-- a * b denotes a sequence: pattern a, then pattern b.
-- a + b denotes an alternative: pattern a or pattern b.
-- a ^ n denotes repetition: a repeated n or more times.
-- a ^ -n denotes limited repetition: a repeated at most n times.

local function line_surrounding(str, pos)
  local first = pos
  local last = pos
  while first > 1 and string.byte(str, first) ~= 10 do
    first = first - 1
  end
  while last < #str and string.byte(str, last) ~= 10 do
    last = last + 1
  end
  if string.byte(str, first) == 10 then first = first + 1 end
  if string.byte(str, last) == 10 then last = last - 1 end
  return string.sub(str, first, last)
end

local function track_error_pos(rule)
  return action(rule, function(str, pos)
    if pos > error_pos then
      error_pos = pos
    end
    return pos
  end)
end

local function discard(str, pos) return pos end

local eof = pattern(-1)
local sep = pattern ":"
local field = capture((1-sep)^1)
local number = capture((range "09")^1)
local range_suggestion = field * sep * number * pattern "-" * number *
  sep * field * sep * field * eof

local line_suggestion = field * sep * number * sep * field * sep * field * eof

suggfile, infile = ...
inline_suggestions = { }
prefix_suggestions = { }

for line in io.lines(suggfile) do
  data = match(aggregate(range_suggestion), line)
  if data then
    local file = data[1]
    local from = tonumber(data[2])
    local to = tonumber(data[3])
    local mode = (data[4] == "W")
    local var = data[5]
    if not inline_suggestions[file] then
      inline_suggestions[file] = { }
    end
    for i = from, to do
      if not inline_suggestions[file][i] then
	inline_suggestions[file][i] = { }
      end
      inline_suggestions[file][i][var] =
	inline_suggestions[file][i][var] or mode
    end
  else
    data = match(aggregate(line_suggestion), line)
    if data then
      local file = data[1]
      local num = tonumber(data[2])
      local mode = (data[3] == "W")
      local var = data[4]
      if not prefix_suggestions[file] then
	prefix_suggestions[file] = { }
      end
      if not prefix_suggestions[file][num] then
	prefix_suggestions[file][num] = { }
      end
      prefix_suggestions[file][num][var] =
	prefix_suggestions[file][num][var] or mode
    end
  end
end

local prefix_suggestions = prefix_suggestions[infile] or { }
local inline_suggestions = inline_suggestions[infile] or { }

local start_spaces = capture((pattern " " + pattern "\t")^0)
local space = (pattern " " + pattern "\t")
local stringchar = (1-pattern("\\")) + (pattern("\\") * pattern(1))
local string_literal = pattern '"' * (stringchar-pattern('"'))^0 * pattern '"'
local char_literal = pattern "'" *(stringchar-pattern("'"))^0 * pattern "'"
local lit = capture(string_literal + char_literal)
local non_alpha = capture((1-range("az", "AZ", "__")))
local alpha = range("az", "AZ", "__")
local digit = range("09")
local name = capture(alpha * (alpha + digit)^0)
local var_splitter = aggregate((lit+non_alpha) ^ 0 *
  (name * (lit+non_alpha)^1) ^ 0 * name ^ 0)

local function replace_var(line, var, guard)
  local vars = match(var_splitter, line)
  for i = 1, #vars do
    if vars[i] == var then
      vars[i] = guard
    end
  end
  return table.concat(vars)
end

local assignment = space ^ 0 * alpha * (alpha + digit)^ 0 * space ^ 0 *
  (pattern "="+pattern "[") * (1-pattern "=") *
  ((1-(pattern ";"+pattern "," + pattern "("))^0 *
  (pattern ";" + pattern "," + pattern "("))^1 * space ^ 0 * eof
local macro_or_expr = space ^ 0 * range "AZ" * (alpha + digit)^ 0 * space ^ 0 *
  pattern "(" * ((1-pattern ";") ^ 0 * pattern ";")^1 * eof
local else_if = aggregate(capture(space ^ 0 *
  (pattern "else" * space ^ 1)^-1 * pattern "if" * space ^ 0 * pattern "(") *
  capture(pattern(1)^1)) +
  aggregate(capture(space ^ 0 *
  pattern "return" * space ^ 1) *
  capture(pattern(1)^1))

local comment_line = space ^ 0 * pattern "/*" * (1-#pattern "*/")^0 * pattern "*/" *
  space ^ 0 * eof

local lineno = 1
local defined = { }
print("#line 1 \"" .. infile .. "\"")
for line in io.lines(infile) do
  local suppress = false
  local spaces = match(start_spaces, line)
  local vars = match(var_splitter, line)
  if string.sub(line, #line) ~= "\\" and string.sub(line, 1, 1) ~= "*" and
     not match(comment_line, line) then
    for i = 1, #vars do
      defined[vars[i]] = true
    end
  end
  local prefix = prefix_suggestions[lineno]
  if prefix then
    for var, mode in pairs(prefix) do
      if defined[var] then
	if mode then
	  print(spaces .. "WriteGuard(" .. var .. ");")
	else
	  print(spaces .. "ReadGuard(" .. var .. ");")
	end
	print("#line " .. lineno .. " \"" .. infile .. "\"")
      end
    end
  end
  local inline = inline_suggestions[lineno]
  if inline then
    if match(assignment, line) then
      for var, mode in pairs(inline) do
	if defined[var] then
	  if mode then
	    print(spaces .. "WriteGuard(" .. var .. "),")
	  else
	    print(spaces .. "ReadGuard(" .. var .. "),")
	  end
	  print("#line " .. lineno .. " \"" .. infile .. "\"")
	end
      end
    elseif match(macro_or_expr, line) then
      print(spaces .. "{")
      for var, mode in pairs(inline) do
	if defined[var] then
	  if mode then
	    print(spaces .. "WriteGuard(" .. var .. ");")
	  else
	    print(spaces .. "ReadGuard(" .. var .. ");")
	  end
	  print("#line " .. lineno .. " \"" .. infile .. "\"")
	end
      end
      print(line)
      suppress = true
      print(spaces .. "}")
    elseif match(else_if, line) then
      local m = match(else_if, line)
      local out = m[1]
      for var, mode in pairs(inline) do
	if defined[var] then
	  if mode then
	    out = out .. "WriteGuard(" .. var .. "), "
	  else
	    out = out .. "ReadGuard(" .. var .. "), "
	  end
	end
      end
      line = (out .. m[2])
    else -- brute force replacement
      for var, mode in pairs(inline) do
	if mode then
	  line = replace_var(line, var, "WriteGuard("..var..")")
	else
	  line = replace_var(line, var, "ReadGuard("..var..")")
	end
      end
    end
  end
  if not suppress then
    print(line)
  end
  lineno = lineno + 1
end
