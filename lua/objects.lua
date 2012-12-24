local no_class = "<no class>"

function new(basetype, ...)
  local result = { }
  setmetatable(result, basetype.__dispatch__)
  result:create(...)
  return result
end

function class(base)
  local result = { }
  result.__class_name__ = no_class
  for method, body in pairs(base or { }) do
    result[method] = body
  end
  if not base then
    result.__base__ = result
  else
    result.__base__ = base
  end
  result.__class__ = result
  result.__dispatch__ = { __index = result }
  return result
end

function name_classes()
  for varname, value in pairs(_G) do
    if type(value) == "table" and value.__class_name__ == no_class then
      value.__class_name__ = varname
    end
  end
end
