local no_class = { }

function new(basetype, ...)
  local result = { }
  setmetatable(result, getmetatable(basetype))
  result:create(...)
  return result
end

function class(t)
  local result = { }
  setmetatable(result, { __index =
    function(_, m)
      return rawget(result, m)
    end })
  result.class_name = no_class
  for method, body in pairs(t or { }) do
    result[method] = body
  end
  if not t then
    result.base_class = result
  end
  return result
end

function name_classes()
  for varname, value in pairs(_G) do
    if type(value) == "table" and rawget(value, "class_name") == no_class then
      value.class_name = varname
    end
  end
end
