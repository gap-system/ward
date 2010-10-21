lpeg = require "listlpeg"
require "util"
require "config"
require "objects"
require "ast"
require "cparser"
require "dataflow"

local files_parsed = 0

for i = 1, #arg do
  local file = arg[i]
  if string.sub(file, 1, 1) == "-" then
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


if files_parsed > 0 then
  run_dataflow_analysis()
end

os.exit(0)
