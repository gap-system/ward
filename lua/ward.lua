lpeg = require "listlpeg"
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
}

local i = 1
while i <= #arg do
  local file = arg[i]
  i = i + 1
  if string.sub(file, 1, 1) == "-" then
    if file == "-pass" then
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
    elseif file == "-errors" then
      options.report_type = "errors"
    elseif file == "-suggest" then
      options.report_type = "suggestions"
    elseif file == "-parseonly" then
      options.parse_only = true
    else
      system_error("Unknown option '" .. file .. "'")
    end
  else
    local command = {"gcc", "-E"}
    for _, opt in ipairs(preprocessor_options) do
      push(command, opt)
    end
    for name, value in pairs(preprocessor_defines) do
      push(command, "-D" .. name .. "=" .. value)
    end
    push(command, file)
    local filecontents = read_from_command(command)
    if filecontents then
      local success = parse(filecontents)
      if not success then
        os.exit(1)
      end
      files_parsed = files_parsed + 1
    else
      system_error("File "..file.." not found")
    end
  end
end


if files_parsed > 0 and not options.parse_only then
  run_dataflow_analysis()
end

os.exit(0)
