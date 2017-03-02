lpeg = require "lpeg"
lpeg.setmaxstack(1000)
require "util"
require "config"
require "objects"
require "ast"
require "cparser"
require "dataflow"

local files_parsed = 0
options = {
  report_type = "errors",
  pass_count = 2,
  parse_only = false,
  verbose = true,
  debug = false,
}

local ccprog = "cc"

local i = 1
while i <= #arg do
  local argument = arg[i]
  i = i + 1
  if string.sub(argument, 1, 1) == "-" then
    if argument == "-pass" then
      local pass_count = arg[i]
      i = i + 1
      if pass_count and lpeg.match(lpeg.R("09")^1*lpeg.P(-1), pass_count) then
        options.pass_count = tonumber(pass_count)
      else
        system_error("The -pass option requires a numeric argument")
      end
      if options.pass_count == 0 then
        system_error("The number of passes must be at least 1")
      end
    elseif argument == "-cpp" then
      while i <= #arg and arg[i] ~= "--" do
        push(preprocessor_options, arg[i])
        i = i + 1
      end
      if arg[i] == "--" then i = i + 1 end
    elseif argument == "-I" then
      local incdir = arg[i]
      i = i + 1
      if not incdir then
       system_error "The -I option requires a directory argument"
      end
      push(preprocessor_options, "-I"..incdir)
    elseif argument == "-cc" then
      ccprog = arg[i]
      i = i + 1
    elseif string.sub(argument, 1, 2) == "-I" then
      push(preprocessor_options, argument)
    elseif string.sub(argument, 1, 2) == "-D" then
      push(preprocessor_options, argument)
    elseif argument == "-errors" then
      options.report_type = "errors"
    elseif argument == "-suggest" then
      options.report_type = "suggestions"
    elseif argument == "-parseonly" then
      options.parse_only = true
    elseif argument == "-verbose" then
      options.verbose = true
    elseif argument == "-debug" then
      options.debug = true
    elseif argument == "-terse" then
      options.verbose = false
    else
      system_error("Unknown option '" .. argument .. "'")
    end
  else
    local command = { ccprog, "-E" }
    for _, opt in ipairs(preprocessor_options) do
      push(command, opt)
    end
    for name, value in pairs(preprocessor_defines) do
      push(command, "-D" .. name .. "=" .. value)
    end
    push(command, argument)
    local filecontents = read_from_command(command)
    if filecontents then
      local success = parse(filecontents, argument)
      if not success then
        os.exit(1)
      end
      files_parsed = files_parsed + 1
    else
      system_error("File "..argument.." not found")
    end
  end
end


if files_parsed > 0 and not options.parse_only then
  run_dataflow_analysis()
end

os.exit(0)
