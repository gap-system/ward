-- Some more readable versions of the lpeg primitives.

local pattern = lpeg.P -- match a literal
local range = lpeg.R -- match a range of characters
local charset = lpeg.S -- match a set of characters
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


local eof = pattern(-1) -- end of file
local space = pattern " " + pattern "\t" + pattern "\n" + pattern "\r"
local sp = track_error_pos(space^1)^-1
local alpha = range("az", "AZ", "__")
local digit = range("09")
local leading_digit = range("19")
local octal_digit = range("07")
local hex_digit = range("09", "af", "AF")

local storage_classifier_table = {
  register = "auto",
  auto = "auto",
  extern = "extern",
  inline = "inline",
  __inline = "inline",
  __inline__ = "inline",
  static = "static",
  __thread = "thread",
  __attribute__ = "attribute",
}

local keywords = {
  "asm", "auto", "enum", "restrict", "unsigned", "break", "extern",
  "return", "void", "case", "float", "short", "volatile", "char", "for",
  "signed", "while", "const", "goto", "sizeof", "_Bool", "continue", "if",
  "static", "_Complex", "default", "inline", "struct", "_Imaginary", "do",
  "int", "switch", "double", "long", "typedef", "else", "register",
  "union", "__asm", "__asm__", "__thread", "__builtin_va_list", "__inline__",
  "__builtin_va_arg", "__extension__", "__const", "__restrict", "__volatile__",
  "__signed", "__unsigned", "_Alignof", "_Noreturn", "_Nonnull", "_Nullable",
  "_Null_unspecified",
}

local is_keyword = { }
local typedefs = { }
local local_variables = { }
global_variables = { }
static_variables = { }
global_functions = { }
static_functions = { }
all_functions = { }
entry_points = { }

local keyword_pattern = pattern(keywords[1])
for i = 1, #keywords do
  is_keyword[keywords[i]] = true
end
for i = 2, #keywords do
  keyword_pattern = keyword_pattern + pattern(keywords[i])
end
keyword_pattern = keyword_pattern * #(1-(alpha+digit))

local raw_ident = alpha * (alpha + digit)^0

local ident = action(raw_ident, function(s, p, id)
  if is_keyword[id] or typedefs[id] then
    return nil
  end
  return p, id
end)

local pos_ident = action(raw_ident, function(s, p, id)
  if is_keyword[id] or typedefs[id] then
    return nil
  end
  return p, id, p-#id
end)


local ident_or_type = action(raw_ident, function(s, p, id)
  if is_keyword[id] then
    return nil
  end
  return p, id
end)


local pos_ident_or_type = action(raw_ident, function(s, p, id)
  if is_keyword[id] then
    return nil
  end
  return p, id, p-#id
end)


local type_name = action(raw_ident, function(s, p, id)
  if not is_keyword[id] and typedefs[id] then
    return p, id
  end
  return nil
end)


function keyword(k)
  return k * # (1-(alpha+digit))
end

function keyword_pos(k)
  local len = #k
  return action(keyword(k), function(str, pos)
    return pos, pos-len
  end)
end

local stringchar = (1-pattern("\\")) + (pattern("\\") * pattern(1))
local string_literal = '"' * (stringchar-pattern('"'))^0 *'"'
local char_literal = "'" *(stringchar-pattern("'"))^0 *"'"
local comment = pattern("/*") * (1-pattern("*/"))^0 * pattern("*/")
local integer_constant =
  (pattern "0x" * hex_digit^1 +
  leading_digit * digit^0 +
  pattern "0" * octal_digit^0) * charset("luLU")^0
local string_constant = string_literal * (sp * string_literal)^0
local float_constant =
  (digit^1 * pattern "." * digit^0 +
  pattern "." * digit^1) * ((pattern "e" + pattern "E") *
  (pattern "-" + pattern "+")^-1 * digit^1)^-1
  * charset("lfLF")^-1
local constant = float_constant + integer_constant

local attribute_arg = raw_ident + string_literal + integer_constant
local attribute = raw_ident * (sp * pattern "(" * sp * attribute_arg *
  (sp * pattern "," * sp * attribute_arg)^0 * sp * pattern ")")^-1
local attribute_decl = keyword "__attribute__" * action(sp * pattern "(" * sp *
  pattern "(" * sp * attribute * (sp * pattern "," * sp * attribute)^0 *
  sp * pattern ")" * sp * pattern ")", discard)
local attributes = attribute_decl * (sp * attribute_decl)^0

local assignment_operator =
  capture(pattern "=" * #(1-pattern "=") +
  pattern "+=" +
  pattern "-=" +
  pattern "*=" +
  pattern "/=" +
  pattern "%=" +
  pattern "<<=" +
  pattern ">>=" +
  pattern "&=" +
  pattern "|=" +
  pattern "^=")

local equality = pattern "==" + pattern "!="
local relation = pattern "<=" + pattern ">=" +
  (pattern "<" * # (1-pattern "<")) +
  (pattern ">" * # (1-pattern ">"))
local shift = (pattern "<<" * # (1 - pattern "=")) +
  (pattern ">>" * # (1-pattern "="))
local add_op = (pattern "+" * # (1 - range("++", "=="))) +
  (pattern "-" * # (1 - range("--", "==")))
local mult_op = (pattern "*" * # (1 - pattern "=")) +
  (pattern "/" * # (1-pattern "=")) +
  (pattern "%" * # (1-pattern "="))
local unary_operator =
  capture(pattern "&" +
  pattern "*" +
  pattern "+" +
  pattern "-" +
  pattern "~" +
  pattern "!")

local expression = rule "expression"

local precedence = {
 ["*"]=10, ["/"]=10, ["%"]=10,
 ["+"]=9, ["-"]=9,
 ["<<"]=8, [">>"]=8,
 ["<"]=7, [">"]=7, ["<="]=7, [">="]=7,
 ["=="]=6, ["!="]=6,
 ["&"]=5, ["^"]=4, ["|"]=3,
 ["&&"]=2, ["||"]=1,
}

local function build_postfix_expression(expr, list)
  for i = 1, #list, 2 do
    local op = list[i]
    local arg = list[i+1]
    if op == "x++" or op == "x--" then
      expr = new(ExprOp, expr.pos, op, expr)
    elseif op == "[]" then
      expr = new(ExprIndex, expr.pos, expr, arg)
    elseif op == "()" then
      expr = new(ExprCall, expr.pos, expr, arg)
    elseif op == "." then
      expr = new(ExprMember, expr.pos, expr, arg)
    elseif op == "->" then
      -- x->y == (*x).y
      expr = new(ExprMember, expr.pos, new(ExprDeref, expr.pos, expr), arg)
    end
  end
  return expr
end

local function build_unary_expression(op, expr)
  if op == "*" then
    return new(ExprDeref, expr.pos, expr)
  elseif op == "+" then
    return expr
  elseif op == "-" then
    return new(ExprOp, expr.pos, "neg", expr)
  elseif op == "&" then
    return new(ExprAddr, expr.pos, expr)
  elseif op == "~" or op == "!" then
    return new(ExprOp, expr.pos, op, expr)
  end
end

local function build_binary_expression(list)
  local operands = { list[1] }
  local operators = { }
  for i = 2, #list, 2 do
    local operator = list[i]
    local operand = list[i+1]
    while #operators > 0
       and precedence[operator] > precedence[operators[#operators]] do
      local op2 = pop(operands)
      local op1 = pop(operands)
      local op = pop(operators)
      if op1:is_constant() and op2:is_constant() then
        push(operands, new(ExprConstant))
      else
        push(operands, new(ExprOp, op1.pos, op, op1, op2))
      end
    end
    push(operands, operand)
    push(operators, operator)
  end
  while #operators > 0 do
    local op2 = pop(operands)
    local op1 = pop(operands)
    local op = pop(operators)
    if op1:is_constant() and op2:is_constant() then
      push(operands, new(ExprConstant))
    else
      push(operands, new(ExprOp, op1.pos, op, op1, op2))
    end
  end
  return operands[1]
end

local function build_type_postfix(str, pos, pretype, postfix)
  local result = { }
  for i = 1, #pretype do
    push(result, pretype[i])
  end
  for i = 1, #postfix do
    push(result, postfix[i])
  end
  return pos, result
end

local function build_typed_declaration1(basetype, decl)
  local fulltype = basetype
  for i = #decl, 3, -1 do
    local mod = decl[i]
    if mod == "*" then
      fulltype = new(TypeRef, fulltype)
    elseif type(mod) == "table" then
      fulltype = new(TypeFunction, fulltype, mod)
    elseif type(mod) == "number" then
      fulltype = new(TypeArray, fulltype, mod)
    end
  end
  return { decl[1], fulltype, decl[2] }
end

local function build_typed_declaration(str, pos, basetype, decl)
  -- returns a pair (name, type), where name can be nil.
  return pos, build_typed_declaration1(basetype, decl)
end

local function build_typed_declarations(str, pos, basetype, decls)
  -- returns a list of pairs (name, type)
  local result = { }
  for i = 1, #decls do
    push(result, build_typed_declaration1(basetype, decls[i]))
  end
  return pos, result
end

local function build_typed_declarations_init(str, pos, storage, basetype, decls)
  -- returns a list of triples (name, type, initial_value)
  local result = { }
  for i = 1, #decls, 2 do
    local decl = build_typed_declaration1(basetype, decls[i])
    decl[4] = decls[i+1]
    decl[5] = storage
    push(result, decl)
  end
  return pos, result
end

local function build_cond_expr(expr)
  if #expr == 1 then
    return build_binary_expression(expr[1])
  else
    return new(ExprOp, nil, "?:",
      build_binary_expression(expr[1]),
      build_cond_expr(expr[2]),
      build_cond_expr(expr[3]))
  end
end

local break_stack, continue_stack, switch_stack
local function_exit
local goto_statements, label_statements
local push, pop = table.insert, table.remove
local nodes
local dependent_nodes

local function add_local_var(name, type)
  push(local_variables, { name, type })
  local_variables[name] = #local_variables
end

local function connect(node1, node2)
  if node1 and node2 then
    push(node1.succ, node2)
    push(node2.pred, node1)
  end
end

local function remove_node(list, node)
  for i = 1, #list do
    if list[i] == node then
      table.remove(list, i)
      return
    end
  end
end

local function disconnect(node)
  local pred = node.pred
  local succ = node.succ
  for i = 1, #succ do
    local node2 = succ[i]
    remove_node(node2.pred, node)
  end
  for i = 1, #pred do
    local node2 = pred[i]
    remove_node(node2.succ, node)
  end
end

local function make_node(ast)
  local node = new(Node, ast)
  push(nodes, node)
  push(dependent_nodes, node)
  node.local_vars = local_variables
  node.static_vars = static_variables
  node.static_functions = static_functions
  node.index = #nodes
  return node
end

local function print_graph(graph)
  local nodes = graph.nodes
  print()
  for i = 1, #nodes do
    local node = nodes[i]
    local out = ""
    for _, pred in ipairs(node.pred) do
      out = out .. string.format("%d ", pred.index)
    end
    out = out .. "-> " .. node.index .. " ->"
    for _, succ in ipairs(node.succ) do
      out = out .. string.format(" %d", succ.index)
    end
    print(out)
  end
end

local build_function_names = { }

local function build_stmt(ast)
  local func, start, finish = unpack(ast)
  local entry, exit = func(select(4, unpack(ast)))
  entry.start_pos = start
  entry.finish_pos = finish
  return entry, exit
end

local function build_if_stmt(cond, then_part, else_part)
  local cond_node = make_node(cond)
  local exit_node = make_node()
  if else_part then
    local then_start, then_end = build_stmt(then_part)
    local else_start, else_end = build_stmt(else_part)
    connect(cond_node, then_start)
    connect(cond_node, else_start)
    connect(then_end, exit_node)
    connect(else_end, exit_node)
  else
    local then_start, then_end = build_stmt(then_part)
    connect(cond_node, exit_node)
    connect(cond_node, then_start)
    connect(then_end, exit_node)
  end
  return cond_node, exit_node
end

local function build_while_stmt(cond, body)
  local cond_node = make_node(cond)
  local exit_node = make_node()
  local body_start, body_end = build_stmt(body)
  connect(cond_node, body_start)
  connect(cond_node, exit_node)
  connect(body_end, cond_node)
  return cond_node, exit_node
end

local function build_do_stmt(body, cond)
  local cond_node = make_node(cond)
  local body_start, body_end = build_stmt(body)
  connect(cond_node, body_start)
  connect(body_end, cond_node)
  return body_start, cond_node
end

local function build_for_stmt(init, cond, step, body)
  local init_node = make_node(init)
  local cond_node = make_node(cond)
  local step_node = make_node(step)
  local exit_node = make_node()
  push(continue_stack, step_node)
  push(break_stack, exit_node)
  local body_start, body_end = build_stmt(body)
  connect(init_node, cond_node)
  connect(cond_node, exit_node)
  connect(cond_node, body_start)
  connect(body_end, step_node)
  connect(step_node, cond_node)
  pop(continue_stack)
  pop(break_stack)
  return init_node, exit_node
end

local function build_compound_stmt_part(decls, stmts)
  local first, last
  local head_node = make_node()
  local save_vars = nil
  if #decls then
    save_vars = clone(local_variables)
  end
  local decl_nodes = { }
  for i = 1, #decls do
    for j = 1, #decls[i] do
      local name, type, pos, value = unpack(decls[i][j])
      add_local_var(name, type)
      if value then
	local node = make_node(new(ExprAssign, pos, "=",
	  new(ExprVar, pos-#name, name), value))
	node.insertion_point = node
	push(decl_nodes, node)
      end
    end
  end
  for i = 2, #decl_nodes do
    connect(decl_nodes[i-1], decl_nodes[i])
  end
  if #stmts > 0 then
    local save_dep = dependent_nodes
    dependent_nodes = { }
    first, last = build_stmt(stmts[1])
    for _, node in ipairs(dependent_nodes) do
      node.insertion_point = first
    end
    for i = 2, #stmts do
      dependent_nodes = { }
      local a, b = build_stmt(stmts[i])
      for _, node in ipairs(dependent_nodes) do
	node.insertion_point = a
      end
      connect(last, a)
      last = b
    end
    dependent_nodes = save_dep
    if #decl_nodes > 0 then
      connect(decl_nodes[#decl_nodes], first)
      first = decl_nodes[1]
    end
  else
    if #decl_nodes > 0 then
      first = decl_nodes[1]
      last = decl_nodes[#decl_nodes]
    else
      first = make_node()
      last = first
    end
  end
  if save_vars then
    local_variables = save_vars
  end
  connect(head_node, first)
  return head_node, last
end

local function build_compound_stmt(decls_and_stmts)
  local first, last, start_iter
  if #decls_and_stmts == 1 then
    if #(decls_and_stmts[1]) == 0 then
      local head_node = make_node()
      return head_node, head_node
    else
      return build_compound_stmt_part({}, decls_and_stmts[1])
    end
  end
  if #(decls_and_stmts[1]) == 0 then
    first, last = build_compound_stmt_part(decls_and_stmts[2],
      decls_and_stmts[3])
    start_iter = 4
  else
    first, last = build_compound_stmt_part({}, decls_and_stmts[1])
    start_iter = 2
  end
  for i = start_iter, #decls_and_stmts, 2 do
    local first_iter, last_iter =
      build_compound_stmt_part(decls_and_stmts[i], decls_and_stmts[i+1])
    connect(last, first_iter)
    last = last_iter
  end
  return first, last
end

local function build_switch_stmt(expr, body)
  local expr_node = make_node(expr)
  local exit_node = make_node()
  push(break_stack, exit_node)
  push(switch_stack, expr_node)
  local body_start, body_end = build_stmt(body)
  return expr_node, body_end
end

local function build_case_stmt(id)
  local case_node = make_node()
  connect(switch_stack[#switch_stack], case_node)
  return case_node, case_node
end

local function build_break_stmt()
  local break_node = make_node()
  connect(break_node, break_stack[#break_stack])
  return break_node, nil
end

local function build_continue_stmt()
  local continue_node = make_node()
  connect(continue_node, continue_stack[#continue_stack])
  return continue_node, nil
end

local function build_return_stmt(expr)
  local expr_node = make_node(expr)
  connect(expr_node, function_exit)
  return expr_node, nil
end

local function build_goto_stmt(id)
  local goto_node = make_node()
  push(goto_statements, { goto_node, id })
  return goto_node, nil
end

local function build_label_stmt(id)
  local label_node = make_node()
  label_statements[id] = label_node
  return label_node, label_node
end

local function build_expr_stmt(expr)
  local expr_node = make_node(expr)
  return expr_node, expr_node
end

local function build_empty_stmt()
  local empty_node = make_node()
  return empty_node, empty_node
end

local build_asm_stmt = build_empty_stmt

local function build_graph(args, stmt_tree)
  break_stack = { }
  continue_stack = { }
  switch_stack = { }
  goto_statements = { }
  label_statements = { }
  local_variables = { }
  dependent_nodes = { }
  nodes = { }
  for i = 1, #args do
    add_local_var(args[i][1], args[i][2])
  end
  function_exit = make_node()
  local entry, exit = build_stmt(stmt_tree)
  connect(exit, function_exit)
  for i = 1, #goto_statements do
    local goto_statement = goto_statements[i]
    local goto_node, goto_target = unpack(goto_statement)
    connect(goto_node, label_statements[goto_target])
  end
  for i = 1, #nodes do
    local node = nodes[i]
    local ast = node.ast
    if ast then
      ast:set_context(node.local_vars, static_variables, static_functions)
    end
  end
  return new(Graph, nodes, entry)
end

local build_functions = {
  build_expr_stmt = build_expr_stmt,
  build_if_stmt = build_if_stmt,
  build_compound_stmt = build_compound_stmt,
  build_return_stmt = build_return_stmt,
  build_continue_stmt = build_continue_stmt,
  build_break_stmt = build_break_stmt,
  build_while_stmt = build_while_stmt,
  build_do_stmt = build_do_stmt,
  build_for_stmt = build_for_stmt,
  build_goto_stmt = build_goto_stmt,
  build_label_stmt = build_label_stmt,
  build_case_stmt = build_case_stmt,
  build_switch_stmt = build_switch_stmt,
  build_empty_stmt = build_empty_stmt,
}

for name, func in pairs(build_functions) do
  build_function_names[func] = name
end

local grammar = pattern {
  -- Start symbol
  "file",
  -- Types
  basetype =
    keyword "void" * value(new(TypeVoid)) +
    rule "float_type" +
    rule "integral_type" +
    keyword "unsigned" * value(type_int) +
    keyword "signed" * value(type_int) +
    keyword "__unsigned" * value(type_int) +
    keyword "__signed" * value(type_int) +
    keyword "struct" * sp * rule "record_type" +
    keyword "union" * sp * rule "record_type" +
    rule "enum_type" +
    keyword "__builtin_va_list" * value(type_void_ref) +
    action(type_name, function(str, pos, typename)
      return pos, new(TypeUserDef, typename)
    end),
  float_type =
    keyword "float" * value(type_float) +
    keyword "double" * value(type_float) +
    keyword "long" * sp * rule "float_type",
  integral_type = 
    keyword "int" * value(type_int) +
    keyword "short" * sp * rule "integral_type" +
    keyword "short" * value(type_int) +
    keyword "char" * value(type_int) +
    keyword "long" * sp * rule "integral_type" +
    keyword "long" * value(type_int) +
    keyword "signed" * sp * rule "integral_type" +
    keyword "unsigned" * sp * rule "integral_type" +
    keyword "__signed" * sp * rule "integral_type" +
    keyword "__unsigned" * sp * rule "integral_type" +
    keyword "_Bool" * value(type_int),
  enum_item = ident * (sp * "=" * sp * rule "expression")^-1,
  enum_list = rule "enum_item" *
    (sp * pattern "," * sp * rule "enum_item")^0 *
    (sp * pattern ",")^-1,
  optional_enum_list = pattern "{" * sp * rule "enum_list" * sp *
    pattern "}",
  enum_type = action(keyword "enum" * (sp * ident)^-1 *
    (sp * rule "optional_enum_list")^-1,
    function(str, pos)
      return pos, type_int
    end),
  record_type =
    action(ident_or_type * sp * rule "record_members",
      function(s, pos, name, types)
        return pos, new(TypeRecord, name, types)
      end)+
    action(ident_or_type, function(s, pos, name)
      return pos, new(TypeRecord, name, { })
    end) +
    action(rule "record_members", function(s, pos, types)
      return pos, new(TypeRecord, nil, types)
    end),
  record_members = pattern "{" * sp *
    (rule "record_member" * sp)^0 *
    pattern "}",
  record_member = action(rule "type_prefix" * sp * rule "type_declarations" *
    sp * pattern ";" , build_typed_declarations)+
    keyword "__extension__" * sp * rule "record_member",
  type_spec = action(rule "type_prefix" * sp * rule "type_declaration",
    build_typed_declaration),
  type_prefix = rule "type_access_qualifier" * rule "basetype",
  type_access_qualifier_prefix = (
    keyword "const" * sp +
    keyword "volatile" * sp +
    keyword "__const" * sp +
    keyword "_Noreturn" * sp +
    keyword "_Nonnull" * sp +
    keyword "_Nullable" * sp +
    keyword "_Null_unspecified" * sp
  ),
  type_access_qualifier = (rule "type_access_qualifier_prefix")^0,
  restrict = (keyword "restrict" + keyword "__restrict"),
  type_declaration =
    action(pattern "(" * sp * rule "type_declaration" * sp * pattern ")" *
      rule "type_postfix", build_type_postfix) +
    action(rule "type_access_qualifier" * sp *
           (pattern "*" + pattern "^") * sp * rule "type_declaration",
      function(s, p, c)
        push(c, "*")
	return p, c
      end) +
    action((rule "restrict" * sp)^-1 * aggregate(pos_ident_or_type) *
      rule "type_postfix", build_type_postfix)+
    action(rule "type_access_qualifier_prefix" * rule "type_declaration",
      function(s, p, c)
        return p, c
      end)+
    action(aggregate(value("")) * (sp * rule "restrict")^-1 *
      rule "type_postfix", build_type_postfix),
  opt_asm_declaration =
    (sp * (keyword "asm" + keyword "__asm__" + keyword "__asm") * sp *
      pattern "(" * sp * string_constant * sp * pattern ")")^-1,
  init_expression_list = (rule "init_expression" * (sp * pattern "," * sp *
    rule "init_expression")^0 * (sp * pattern ",")^-1)^-1,
  init_expression = action(pattern "{" * position() * sp *
    rule "init_expression_list" * sp * pattern "}",
    function(str, pos, init_pos)
      return pos, new(ExprAggregate, init_pos-1)
    end)+
    rule "expression",
  type_declarations = aggregate(rule "type_declaration" *
    (sp * pattern "," * sp * rule "type_declaration")^0),
  type_declarations_init = aggregate(rule "type_declaration_init" * sp *
    (pattern "," * sp * rule "type_declaration_init")^0),
  type_declaration_init = rule "type_declaration" * rule "opt_asm_declaration" *
    (sp * "=" * sp * rule "init_expression" + value(nil)),
  type_postfix =
    sp * pattern ":" * sp * integer_constant * value({})+
    aggregate((sp * rule "type_postfix_item")^0),
  type_postfix_item =
    pattern "[" * sp * pattern "]" * value("*") +
    pattern "[" * sp * action(expression,
      function(str, pos, expr) return pos, 1 end) * sp * pattern "]" +
    pattern "(" * sp * (keyword "void" * sp)^-1 * pattern ")" * value("()")+
    pattern "(" * sp * action(rule "formal_argument_list", discard) * sp *
    pattern ")" * value("()"),
  formal_argument_list = aggregate(rule "formal_argument" * sp *
    (pattern "," * sp * rule "formal_argument" * sp)^0 *
    (pattern "," * sp * pattern "...")^-1)+
    aggregate(pattern "..."),
  opt_auto = (keyword "register" * sp + keyword "auto" * sp)^-1,
  formal_argument = action(rule "opt_auto" * (attributes * sp)^-1 *
    rule "type_prefix" * sp * rule "type_declaration" *
    (sp * keyword "const")^-1, build_typed_declaration),
  storage_classifier = capture(keyword "static" +
    keyword "inline" +
    keyword "__inline__" +
    keyword "__inline" +
    keyword "__thread" +
    keyword "extern" +
    keyword "register" +
    keyword "auto") +
    action(attributes, discard) * value("__attribute__"),
  storage_classifiers =
    action(aggregate((rule "storage_classifier" * sp) ^ 0),
    function (str, pos, list)
      local result = { }
      for _, classifier in ipairs(list) do
        result[storage_classifier_table[classifier]] = true
      end
      return pos, result
    end),
  variable_declaration = action(rule "storage_classifiers" *
    rule "type_prefix" * sp *
    rule "type_declarations_init" * sp * (attributes * sp) ^ 0 * pattern ";",
      build_typed_declarations_init) +
      keyword "__extension__" * sp * rule "variable_declaration",
  -- Expressions
  expression = 
    -- first alternative is an optimization
    rule "assignment_expression" * #(sp * range("))", "]]", ";;"))+
    action(aggregate(rule "assignment_expression" * 
    (sp * "," * sp * rule "assignment_expression")^0),
    function(str, pos, list)
      if #list == 1 then
        return pos, list[1]
      else
        return pos, new(ExprComma, nil, list)
      end
    end)+
    keyword "__extension__" * sp * rule "expression",
  assignment_expression =
    action(aggregate(rule "cond_expression" * (sp * assignment_operator * sp *
      rule "cond_expression")^0),
      function(str, pos, list)
        local result = build_cond_expr(list[#list])
	for i = #list-2, 1, -2 do
	  result = new(ExprAssign, nil, list[i+1], build_cond_expr(list[i]),
	    result)
	end
        return pos, result
      end),
  cond_expression = aggregate(rule "binary_expression" *
    (sp * pattern "?" * sp * rule "cond_expression" * sp *
    pattern ":" * sp * rule "cond_expression")^-1),
  binary_expression = aggregate(rule "cast_expression" *
    (sp * rule "binary_operator" * sp * rule "cast_expression")^0),
  binary_operator =
    capture(pattern "||" + pattern "&&" +
    pattern "|" + pattern "&" + pattern "^" +
    equality + relation + shift + add_op + mult_op),
  cast_expression =
    rule "unary_expression" +
    action(pattern "(" * sp * rule "type_spec" * sp * pattern ")" * sp *
    rule "cast_expression", function(str, pos, decl, expr)
      return pos, new(ExprCast, pos, decl[2], expr)
    end),
  unary_expression = rule "postfix_expression" +
    action(pattern "++" * sp * rule "unary_expression",
      function(str, pos, expr)
        return pos, new(ExprOp, pos, "++x", expr)
      end)+
    action(pattern "--" * sp * rule "unary_expression",
      function(str, pos, expr)
        return pos, new(ExprOp, pos, "--x", expr)
      end)+
    action(unary_operator * sp * rule "cast_expression",
      function(str, pos, op, expr)
        return pos, build_unary_expression(op, expr)
      end)+
    action(keyword "sizeof" * sp * pattern "(" * rule "type_spec" *
      pattern ")", function(str, pos)
        return pos, new(ExprConstant)
      end)+
    action(keyword "sizeof" * sp * rule "unary_expression",
      function(str, pos)
        return pos, new(ExprConstant)
      end)+
    action(keyword "_Alignof" * sp * pattern "(" * rule "type_spec" *
      pattern ")", function(str, pos)
        return pos, new(ExprConstant)
      end),
  postfix_expression =
    action(rule "primary_expression" * aggregate((sp * rule "postfix")^0),
      function(str, pos, expr, postlist)
        return pos, build_postfix_expression(expr, postlist)
      end),
  primary_expression =
    action(ident, function(str, pos, id)
      return pos, new(ExprVar, pos-#id, id)
    end) +
    constant * value(new(ExprConstant))+
    pattern "(" * sp *
      (rule "expression" + rule "stmt_expression") *
       sp * pattern ")" +
    string_constant * value(new(ExprConstant))+
    char_literal * value(new(ExprConstant))+
    rule "aggregate_expression"+
    rule "vararg_expr",
  aggregate_expression =
    action(pattern "{" * sp * ("." * sp)^-1 *rule "expression" *
      (sp * pattern "," * sp * ("." * sp)^-1 *rule "expression")^0 *
      (pattern ",")^-1 * sp * pattern "}", function(str, pos)
        return pos, new(ExprConstant)
      end),
  stmt_expression =
    action(rule "compound_statement", function(str, pos)
      return pos, new(ExprConstant)
    end),
  postfix =
    action(pattern "(" * sp * rule "actual_arguments" * sp * pattern ")",
      function(str, pos, arglist)
        return pos, "()", arglist
      end) +
    action(pattern "[" * sp * rule "expression" * sp * pattern "]",
      function(str, pos, expr)
        return pos, "[]", expr
      end) +
    action(pattern "." * sp * ident,
      function(str, pos, id)
        return pos, ".", id
      end) +
    action(pattern "->" * sp * ident,
      function(str, pos, id)
        return pos, "->", id
      end) +
    action(pattern "++",
      function(str, pos)
        return pos, "x++", ""
      end)+
    action(pattern "--",
      function(str, pos)
        return pos, "x--", ""
      end),
  vararg_expr = action(keyword "__builtin_va_arg" * sp * pattern "(" * sp *
    rule "expression" * sp * pattern "," * sp * rule "type_spec" * sp *
    pattern ")", function(str, pos, expr, decl)
      -- TODO: better approximation of this as *((type *)va)++?
      return pos, new(ExprCast, pos, decl[2], new(ExprDeref, pos, expr))
    end),
  actual_arguments = aggregate((rule "expression" *
    (sp * pattern "," * sp * rule "expression")^0)^-1),

  -- Statements
  if_statement = action(keyword_pos "if" * sp *
    pattern "(" * sp * expression * sp * pattern ")" * sp *
    rule "statement" * (sp * keyword "else" * sp * rule "statement") ^ -1 *
    position(),
    function (str, pos, start, cond, if_part, else_part, finish)
      if finish then
        return pos, { build_if_stmt, start, finish, cond, if_part, else_part }
      else
        return pos, { build_if_stmt, start, else_part, cond, if_part }
      end
    end),
  while_statement = action(keyword_pos "while" * sp *
    pattern "(" * sp * expression * sp * ")" * sp *
    rule "statement" * position(),
    function(str, pos, start, cond, body, finish)
      return pos, { build_while_stmt, start, finish, cond, body }
    end),
  do_statement = action(keyword_pos "do" * sp * rule "statement" * sp *
    keyword "while" * sp * pattern "(" * sp * expression * sp * ")" *
    sp * pattern ";" * position(),
    function (str, pos, start, body, cond, finish)
      return pos, { build_do_stmt, start, finish, body, cond }
    end),
  compound_statement = action(action(pattern "{", function(str, pos)
      return pos, pos-1
    end) * sp *
    aggregate(
      aggregate((rule "statement" * sp) ^ 0) *
      (aggregate(action(rule "variable_declaration" * sp,
	function(str, pos, declarations)
	  local local_declarations = { }
	  for _, declaration in ipairs(declarations) do
	    local name, type, vpos, value = unpack(declaration)
	    local storage = declaration[5]
	    if type:is_function() then
	      local funcdef = storage
	      funcdef.type = type.result_type
	      funcdef.arg_types = type.arg_types
	      funcdef.name = name
	      funcdef.filename = source_file_name
	      funcdef.graph = nil
	      if storage.static then
		if not static_functions[name] then
		  static_functions[name] = funcdef
		end
	      else
		if not global_functions[name] then
		  global_functions[name] = funcdef
		end
	      end
	    else
	      if storage.extern then
		global_variables[name] = type
	      else
		push(local_declarations, declaration);
	      end
	    end
	  end
	  return pos, local_declarations
	end) ^ 1) *
      aggregate((rule "statement" * sp) ^ 0)) ^ 0
    ) * pattern "}" * position(),
    function(str, pos, start, decls_and_stmts, finish)
      return pos, { build_compound_stmt, start, finish, decls_and_stmts }
    end),
  for_statement = action(keyword_pos "for" * sp * pattern "(" * sp *
    ((expression + value(nil)) * sp) * pattern ";" * sp *
    ((expression + value(nil)) * sp) * pattern ";" * sp *
    ((expression + value(nil)) * sp) * pattern ")" * sp *
    rule "statement" * position(),
    function (str, pos, start, init, cond, step, body, finish)
      return pos, { build_for_stmt, start, finish, init, cond, step, body }
    end),
  switch_statement = action(keyword_pos "switch" * sp *
    pattern "(" * sp * expression * sp * pattern ")" * sp * rule "statement" *
    position(),
    function (str, pos, start, expr, body, finish)
      return pos, { build_switch_stmt, start, finish, expr, body }
    end),
  label_statement = action(keyword_pos "case" * sp * expression * sp *":" *
      position(),
      function(str, pos, start, expr, finish)
        return pos, { build_case_stmt, start, finish, expr }
      end)+
    action(keyword_pos "default" * sp * ":" * position(),
      function(str, pos, start, finish)
        return pos, { build_case_stmt, start, finish, nil }
      end)+
    action(pos_ident * sp * ":" * position(),
      function(str, pos, id, start, finish)
        return pos, { build_label_stmt, start, finish, id }
      end),
  jump_statement = action(keyword_pos "goto" * sp  * ident * position(),
      function(str, pos, start, id, finish)
        return pos, { build_goto_stmt, start, finish, id }
      end)+
    action(keyword_pos "continue" * position(),
      function(str, pos, start, finish)
	return pos, { build_continue_stmt, start, finish }
      end)+
    action(keyword_pos "break" * position(),
      function(str, pos, start, finish)
	return pos, { build_break_stmt, start, finish }
      end)+
    action(keyword_pos "return" * sp * expression * position(),
      function(str, pos, start, expr, finish)
        return pos, { build_return_stmt, start, finish, expr }
      end)+
    action(keyword_pos "return" * position(),
      function(str, pos, start, finish)
	return pos, { build_return_stmt, start, finish }
      end),
  asm_register = string_literal * sp * pattern "(" * sp *
    rule "expression" * sp * ")",
  asm_registers = sp * (rule "asm_register" * sp *
    (pattern "," * sp * rule "asm_register" * sp) ^ 0)^-1,
  opt_asm_args = pattern ":" * rule "asm_registers" * (pattern ":" *
    rule "asm_registers" * (pattern ":" * (sp * string_constant *
    (sp * pattern "," * sp *string_constant)^0)^-1)^-1)^-1,
  asm_statement = (keyword_pos "asm" + keyword "__asm__" + keyword "__asm") *
    (sp * (keyword "volatile" + keyword "__volatile__"))^-1 *
    sp * pattern "(" * sp * string_constant * sp * (rule "opt_asm_args" * sp) *
    pattern ")" * position(),
  statement = rule "if_statement" +
    rule "while_statement" +
    rule "do_statement" +
    rule "for_statement" + 
    rule "switch_statement" +
    rule "jump_statement" * sp * pattern ";" +
    rule "label_statement" +
    rule "compound_statement" +
    action(position() * expression * sp * pattern ";" * position(),
      function(str, pos, start, expr, finish)
        return pos, { build_expr_stmt, start, finish, expr }
      end)+
    action(pattern ";", function(str, pos)
      return pos, { build_empty_stmt, pos-1, pos }
    end)+
    action(rule "asm_statement",
      function(str, pos, start, finish)
        return pos, { build_asm_stmt, start, finish }
      end),

    -- Toplevel
  file = sp * (rule "toplevel_declaration" * sp)^0 * eof,
  function_arguments =
    pattern "(" * sp * (pattern "void" * sp)^-1 * pattern ")" * value({}) +
    pattern "(" * sp * rule "formal_argument_list" * sp * pattern ")",
  function_declaration =
    rule "storage_classifiers" * rule "basetype" * (sp * pattern "*")^0
    * sp * ident * sp * rule "function_arguments" *
    rule "opt_asm_declaration" * (sp * attributes)^-1 * sp * pattern ";",
  function_definition =
    action(rule "storage_classifiers" *
      rule "type_access_qualifier" * rule "basetype" *
      aggregate((sp * capture(pattern "*"))^0) * sp *
      (attributes * sp)^-1 * ident * sp *
      rule "function_arguments" * sp * rule "compound_statement" *
      (sp * pattern ";")^-1,
      function(str, pos, storage, basetype, ptrs, name, args, stmt)
	local funcdef = storage
	for i = 1, #ptrs do
	  basetype = new(TypeRef, basetype)
	end
	funcdef.type = basetype
	funcdef.arg_types = map(args, function(arg) return arg[2]; end)
	funcdef.name = name
	funcdef.filename = source_file_name
	funcdef.args = args
	funcdef.graph = build_graph(args, stmt)
	funcdef.source_file_input = source_file_input
	funcdef.source_file_mapping = source_file_mapping
	push(all_functions, funcdef)
	if funcdef.static then
	  static_functions[name] = funcdef
	else
	  global_functions[name] = funcdef
	  push(entry_points, funcdef)
	end
	--print_graph(funcdef.graph)
	return pos
      end)+
      keyword "__extension__" * sp * rule "function_definition",
  typedef_declaration =
    action(keyword "typedef" * sp * rule "type_prefix" * sp *
    rule "type_declarations" * sp *
    (attributes * sp)^-1 * pattern ";",
    function(str, pos, basetype, defs)
      for _, def in ipairs(defs) do
	local name, typedef =
	  unpack(build_typed_declaration1(basetype, def))
        typedefs[name] = typedef
      end
      return pos
    end),
  global_variable_declaration =
    action(rule "variable_declaration",
      function(str, pos, declarations)
        for _, declaration in ipairs(declarations) do
	  local name, type, vpos, value = unpack(declaration)
	  local storage = declaration[5]
	  if type:is_function() then
	    local funcdef = storage
	    funcdef.type = type.result_type
	    funcdef.name = name
	    funcdef.filename = source_file_name
	    funcdef.graph = nil
	    if storage.static then
	      if not static_functions[name] then
	        static_functions[name] = funcdef
	      end
	    else
	      if not global_functions[name] then
	        global_functions[name] = funcdef
	      end
	    end
	  else
	    if storage.static then
	      static_variables[name] = type
	    else
	      global_variables[name] = type
	    end
	  end
	end
        return pos
      end),
  struct_union_declaration =
    #((keyword "struct" + keyword "union")*sp) * action(rule "type_prefix"
      * sp * rule "type_declarations" * sp *
      pattern ";", build_typed_declarations),
  toplevel_declaration = rule "typedef_declaration" +
    rule "global_variable_declaration" +
    rule "struct_union_declaration" +
    -- rule "function_declaration" +
    rule "function_definition"
}

local unterminated_line = (1 - range ("\r\r", "\n\n"))^0
local eol = pattern "\n" + pattern "\r\n" + pattern "\r"
local terminated_line = unterminated_line * eol
local line_splitter =
  aggregate(capture(terminated_line)^0 * capture(unterminated_line)^-1 * eof)
local spaces_only = space ^ 0 * eof

local last_split_text, last_split_table
function split_input_lines(input)
  if input == last_split_text then
    return last_split_table
  end
  last_split_text = input
  last_split_table = match(line_splitter, input)
  return last_split_table
end

function line_from_offset(offset, offsettable)
  local l, r = 1, #offsettable
  if offset >= offsettable[r] then
    return r
  end
  while l < r do
    local m = (l + r - (l + r) % 2)/2
    if offset < offsettable[m] then
      r = m-1
    elseif offset >= offsettable[m+1] then
      l = m+1
    else
      return m
    end
  end
  return r
end

local lineno_table = { }

function find_text_coordinates(text, pos)
  local offsets = lineno_table[text]
  local result
  if not offsets then
    local split = split_input_lines(text)
    offsets = { }
    offsets[1] = 1
    for i = 1, #split do
      offsets[i+1] = offsets[i] + #(split[i])
    end
    lineno_table[text] = offsets
  end
  result = line_from_offset(pos, offsets)
  return result, pos-offsets[result]+1
end

function preprocess_input(input)
  local lines = split_input_lines(input)
  local currentline = 1
  local currentfile = ""
  local mapping = { }
  for n, line in ipairs(lines) do
    local lineno, file
    push(mapping, { currentfile, currentline })
    lineno, file = string.match(line, [[^#%s*(%d+)%s*"([^"]*)"]])
    if lineno then
      lines[n] = "\n"
      currentline = lineno
      currentfile = file
    elseif string.match(line, [[^#%s*pragma]]) then
      lines[n] = "\n"
      currentline = currentline + 1
    else
      currentline = currentline + 1
    end
  end
  push(mapping, { currentfile, currentline+1 })
  return table.concat(lines, ""), mapping
end

function is_line_start(text, pos)
  local lines = split_input_lines(text)
  if not pos then
    return false
  end
  local lineno, col = find_text_coordinates(text, pos)
  return match(spaces_only, string.sub(lines[lineno], 1, col-1))
end

function find_source_position(text, pos, mapping)
  local all_lines = split_input_lines(text)
  local lineno, colno = find_text_coordinates(text, pos)
  return mapping[lineno][1], mapping[lineno][2], colno, all_lines[lineno]
end

function show_error(input, mapping, pos, message)
  local sourcefile, sourceline, sourcecol, line =
    find_source_position(input, pos, mapping)
  if message then
    io.stderr:write(string.format("%s:%d:%d: %s\n", sourcefile, sourceline, sourcecol,
      message))
  else
    io.stderr:write(string.format("%s:%d:%d\n", sourcefile, sourceline, sourcecol))
  end
  if options.verbose then
    line = string.gsub(line, "[\r\n]", "")
    io.stderr:write(line .. "\n")
    io.stderr:write(string.rep(" ", sourcecol-1).."^" .. "\n")
  end
end

function parse(input, filename)
  local success, pos, mapping
  input, mapping = preprocess_input(input)
  source_file_input = input
  source_file_mapping = mapping
  source_file_name = filename
  static_variables = { }
  static_functions = { }
  typedefs = { }
  success, pos = match(aggregate(grammar), input)
  if not success then
    show_error(input, mapping, pos, "Syntax error")
  end
  return success
end
