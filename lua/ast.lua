function type_error(pos, message)
  fatal_error(pos, message or "type error")
end

Type = class()
function Type:create()
end

function Type:to_string()
  local result
  if next(self) == nil then
    return self.class_name
  end
  result = self.class_name .. "["
  local comma = false
  for k, v in pairs(self) do
    if comma then
      result = result .. ","
    else
      comma = true
    end
    if type(v) == "table" then
      if v.base_class == Type then
        result = result .. k .. "=" .. v:to_string()
      else
        result = result .. k .. "= ?"
      end
    else
      result = result .. k .. "=" .. tostring(v)
    end
  end
  return result .. "]"
end

local is_bag_name = {
  Bag = true,
  Obj = true
}

function Type:bag()
  -- nil for types that aren't bags or bag pointers/arrays
  -- 0 for bags, 1 for a pointer to a bag, 2 for a pointer to a pointer to a bag.
  -- arrays are treated as pointers
  return nil
end

function Type:is_record_type()
  return false
end

function Type:is_function()
  return false
end

TypeUnknown = class(Type)
type_unknown = new(TypeUnknown)

TypeNum = class(Type)
function TypeNum:is_base_type()
  return true
end

TypeInt = class(TypeNum)
TypeFloat = class(TypeNum)
type_int = new(TypeInt)
type_float = new(TypeFloat)

TypeVoid = class(Type)
type_void = new(TypeVoid)

TypeRef = class(Type)

function TypeRef:create(ref_type)
  self.ref_type = ref_type
end

function TypeRef:bag()
  local bag = self.ref_type:bag()
  if bag == nil then
    return nil
  else
    return bag+1
  end
end

type_string = new(TypeRef, type_int)
type_void_ref = new(TypeRef, type_void)

TypeArray = class(Type)

function TypeArray:create(ref_type, size)
  self.ref_type = ref_type
  self.size = size
end

function TypeArray:bag()
  local bag = self.ref_type:bag()
  return bag and bag+1
end

TypeFunction = class(Type)

function TypeFunction:create(result_type, arg_types)
  self.result_type = result_type
  self.arg_types = arg_types
end

function TypeFunction:is_function()
  return true
end

TypeRecord = class(Type)

function TypeRecord:is_record_type()
  return true
end

function TypeRecord:create(name, member_types)
  self.name = name
  self.members = { }
  for name, member_type in pairs(member_types) do
    self.members[name] = member_type
  end
end

TypeStruct = class(TypeRecord)
TypeUnion = class(TypeRecord)

TypeUserDef = class(Type)

function TypeUserDef:create(name, actual_type)
  self.name = name
  self.actual_type = actual_type
end

function TypeUserDef:bag()
  return is_bag_name[self.name] and 0
end

Expr = class()

function Expr:create()
end

local no_children = { }

function Expr:position()
  if self.pos then
    return self.pos
  else
    local children = self.children
    if children then
      for i = 1, #children do
        local pos = children[i]:position()
	if pos then
	  return pos
	end
      end
    end
  end
  return 1
end

function Expr:sub_expressions()
  return self.children or no_children
end

function Expr:set_context(local_vars, static_vars, static_functions)
  self.local_vars = local_vars
  self.static_vars = static_vars
  self.static_functions = static_functions
  local children = self.children
  if children then
    for i = 1, #children do
      children[i]:set_context(local_vars, static_vars, static_functions)
    end
  end
end

function Expr:track_thread_locals(list)
  local children = self.children
  if children then
    for i = 1, #children do
      children[i]:track_thread_locals(list)
    end
  end
end

function Expr:fix_types()
  local children = self.children
  if children then
    for i = 1, #children do
      children[i]:fix_types()
    end
  end
end

function Expr:is_constant()
  return false
end

function Expr:var_name()
  local children = self.children
  if children then
    for i = 1, #children do
      local name, pos = children[i]:var_name()
      if name then
        return name, pos
      end
    end
    return nil
  else
    return nil
  end
end

function Expr:bag_access(read, write, realread, realwrite, deref_depth)
  local children = self:sub_expressions()
  for i = 1, #children do
    children[i]:bag_access(read, write, realread, realwrite, 0)
  end
end

function Expr:track_aliases_and_guards(aliases, writeguards, readguards)
  local children = self:sub_expressions()
  for i = 1, #children do
    children[i]:track_aliases_and_guards(aliases, writeguards, readguards)
  end
end

ExprCall = class(Expr)

function ExprCall:create(pos, func, args)
  self.pos = pos
  self.func = func
  self.args = args
  self.children = { func }
  for i = 1, #args do
    push(self.children, args[i])
  end
  if static_functions[func] then
    self.type = static_functions[func].return_type
  elseif global_functions[func] then
    self.type = global_functions[func].return_type
  else
    self.type = type_int
  end
end

function ExprCall:bag_access(read, write, realread, realwrite, deref_depth)
  self.func:bag_access(realread, realwrite, realread, realwrite, 0)
  local args = self.args
  for i = 1, #args do
    local arg = args[i]
    arg:bag_access(realread, realwrite, realread, realwrite, 0)
    local bag = arg.type:bag()
    local name, pos = arg:var_name()
    local var = self.local_vars[name]
    local func = self.func.name
    if var and bag and bag < 2 then
      if not func then
        push(realwrite, var)
	push(realwrite, pos)
      else
        local funcdef = self.static_functions[func]
	if not funcdef then
	  funcdef = global_functions[func]
	end
	if not ReadGuards[func] and not WriteGuards[func] then
	  if not funcdef or not funcdef.arg_writes then
	    push(realwrite, var)
	    push(realwrite, pos)
	  else
	    if funcdef.arg_writes[var] then
	      push(realwrite, var)
	      push(realwrite, pos)
	    elseif funcdef.arg_reads[var] then
	      push(realread, var)
	      push(realread, pos)
	    end
	  end
	end
      end
    end
  end
end

function ExprCall:track_aliases_and_guards(aliases, writeguards, readguards)
  local funcname = self.func:var_name()
  if WriteGuards[funcname] then
    local var = #self.args > 0 and self.local_vars[self.args[1]:var_name()]
    if var then
      push(writeguards, var)
    end
  elseif ReadGuards[funcname] then
    local var = #self.args > 0 and self.local_vars[self.args[1]:var_name()]
    if var then
      push(readguards, var)
    end
  else
    self.func:track_aliases_and_guards(aliases, writeguards, readguards)
    for i = 1, #self.args do
      self.args[i]:track_aliases_and_guards(aliases, writeguards, readguards)
    end
  end
end

ExprComma = class(Expr)

function ExprComma:create(pos, list)
  self.pos = list[1].pos
  self.children = list
  self.type = list[#list].type
end

ExprOp = class(Expr)

function ExprOp:create(pos, op, ...)
  local args = { ... }
  self.pos = pos
  self.op = op
  self.args = args
  self.children = args
  self.type = args[#args].type -- TODO: really need coercion here
end

function ExprOp:fix_types()
  for i = 1, #self.args do
    self.args[i]:fix_types()
  end
  if self.op == "+" or self.op == "-" then
    for i = 1, #self.args do
      local arg = self.args[i]
      if arg.type:bag() then
        self.type = arg.type
      end
    end
  end
end

function ExprOp:bag_access(read, write, realread, realwrite, deref_depth)
  if self.op == "+" or self.op == "-" then
    for i = 1, #self.args do
      local arg = self.args[i]
      if arg.type:bag() then
        arg:bag_access(read, write, realread, realwrite, deref_depth)
      else
        arg:bag_access(read, write, realread, realwrite, 0)
      end
    end
  else
    Expr.bag_access(self, read, write, realread, realwrite, deref_depth)
  end
end

ExprAssign = class(Expr)

function ExprAssign:create(pos, op, target, expr)
  self.pos = target.pos
  self.op = op
  self.target = target
  self.expr = expr
  self.children = { target, expr }
  self.type = target.type
end

function ExprAssign:track_thread_locals(list)
  local tbag = self.target.type:bag()
  if tbag == 0 then
    local tvar = self.local_vars[self.target:var_name()]
    if tvar and self.expr.funcname == "NewBag" then
      push(list, tvar)
    end
  end
  Expr.track_thread_locals(self, list) -- call to superclass
end

function ExprAssign:track_aliases_and_guards(aliases, guards)
  local tbag = self.target.type:bag()
  local ebag = self.expr.type:bag()
  if tbag == 0 then
    if ebag == 0 or ebag == 1 then
      -- case: tvar = evar, tvar = *evar
      local tvar = self.target:var_name()
      if tvar then
        tvar = self.local_vars[tvar]
      end
      if tvar then
        local evar = self.target:var_name()
	if evar then
	  evar = self.local_vars[evar]
	end
        if evar then
	  push(aliases, { tvar, evar })
	else
	  -- treat static and global variables as unknowns
	  push(aliases, { tvar })
	end
      end
    else
      -- case: tvar = unknown expression
      local tvar = self.target:var_name()
      if tvar then
        tvar = self.local_vars[tvar]
	if tvar then
	  push(aliases, { tvar })
	end
      end
    end
  end
end

function ExprAssign:bag_access(read, write, realread, realwrite, deref_depth)
  if self.target.type:bag() then
    self.target:bag_access(write, write, realread, realwrite, deref_depth)
  end
  self.expr:bag_access(read, write, realread, realwrite, deref_depth)
end

ExprDeref = class(Expr)

function ExprDeref:create(pos, arg)
  self.pos = pos
  self.arg = arg
  self.children = { arg }
  self.type = type_unknown
end

function ExprDeref:fix_types()
  self.arg:fix_types()
  self.type = new(TypeRef, self.arg.type)
end

ExprAddr = class(Expr)

function ExprAddr:create(pos, arg)
  self.pos = pos
  self.arg = arg
  self.children = { arg }
  self.type = new(TypeRef, arg.type)
end

function ExprDeref:bag_access(read, write, realread, realwrite, deref_depth)
  self.arg:bag_access(read, write, realread, realwrite, deref_depth+1)
end

ExprIndex = class(Expr)

function ExprIndex:create(pos, arg, index)
  self.pos = pos
  self.arg = arg
  self.index = index
  self.children = { arg, index }
  self.type = type_unknown
end

function ExprIndex:fix_types()
  self.arg:fix_types()
  self.index:fix_types()
  self.type = new(TypeRef, self.arg.type)
end

function ExprIndex:bag_access(read, write, realread, realwrite, deref_depth)
  self.arg:bag_access(read, write, realread, realwrite, deref_depth+1)
  self.index:bag_access(realread, realwrite, realread, realwrite, 0)
end

ExprComma = class(Expr)

function ExprComma:create(pos, expr_list)
  self.pos = pos
  self.expr_list = expr_list
  self.children = expr_list
  self.type = expr_list[#expr_list].type
end

ExprCast = class(Expr)

function ExprCast:create(pos, cast_type, cast_expr)
  self.pos = pos
  self.cast_expr = cast_expr
  self.children = { cast_expr }
  self.type = cast_type
end

ExprVar = class(Expr)

function ExprVar:create(pos, name)
  self.pos = pos
  -- TODO: Fix type
  self.name = name
  self.type = type_unknown
end

function ExprVar:fix_types()
  local name = self.name
  if self.local_vars[name] then
    self.type = self.local_vars[self.local_vars[name]][2]
  elseif self.static_vars[name] then
    self.type = self.static_vars[name]
  elseif global_variables[name] then
    self.type = global_variables[name]
  end
end

function ExprVar:var_name()
  return self.name, self.pos
end

function ExprVar:bag_access(read, write, realread, realwrite, deref_depth)
  local bagdepth = self.type:bag()
  if deref_depth >= 1 and bagdepth and deref_depth+bagdepth >= 2 then
    -- This is not quite kosher; it just so happens that PTR_BAG() and
    -- ADDR_OBJ() return a Bag * value, not a UInt *.
    -- We're covering basically two cases here:
    -- **bag (which is a UInt) and *PTR_BAG(bag), which is of type Bag.
    -- And, of course, ****bag is also covered, but probably an error.
    local index = self.local_vars[self.name]
    if index then
      push(read, index)
      push(read, self.pos)
    end
  end
end

ExprConstant = class(Expr)

function ExprConstant:create(pos, value)
  self.pos = pos
  self.value = value
  self.type = type_unknown
end

function ExprConstant:is_constant()
  return true
end

ExprIntConst = class(Expr)

function ExprIntConst:create(pos, value, suffix)
  self.pos = pos
  self.value = value
  self.type = type_int
end

function ExprIntConst:is_constant()
  return true
end

ExprFloatConst = class(Expr)

function ExprFloatConst:create(pos, value, suffix)
  self.pos = pos
  self.value = value
  self.type = type_float
end

function ExprFloatConst:is_constant()
  return true
end

ExprCharConst = class(Expr)

function ExprCharConst:create(pos, value)
  self.pos = pos
  self.value = value
  self.type = type_int
end

function ExprCharConst:is_constant()
  return true
end

ExprStringConst = class(Expr)

function ExprStringConst:create(pos, value)
  self.pos = pos
  self.value = value
  self.type = type_string
end

function ExprStringConst:is_constant()
  return true
end

ExprMember = class(Expr)

function ExprMember:create(pos, expr, name)
  self.pos = pos
  self.expr = expr
  self.name = name
  self.children = { expr }
  if expr.type:is_record_type() then
    local member = expr.type.members[name]
    if member then
      self.type = member
    else
      self.type = type_unknown
    end
  else
    self.type = type_unknown
  end
end

ExprAggregate = class(Expr)

function ExprAggregate:create(pos)
  self.pos = pos
  self.type = type_unknown
end

name_classes()
