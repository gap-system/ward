Node = class()
-- attributes:
-- changed: has changed since last iteration
-- wp, rp: write and read protection
-- aa: argument alias information
-- assign: assignment information
-- writes, reads: which variables are being read/written
-- pred, succ: lists of predecessors and successors
-- ast: abstract syntax tree of statement/expression.

Graph = class()
-- Attributes:
-- nodes: list of all nodes
-- start: entry node

function Graph:create(nodes, start)
  self.nodes = nodes or { }
  self.start = start
end

function Node:create(ast)
  self.ast = ast
  self.pred = { }
  self.succ = { }
end

function list_errors(graph, accessproperty, guardproperty)
  local nodes = graph.nodes
  local errors = { }
  for i = 1, #nodes do
    local node = nodes[i]
    local accesses = node[accessproperty]
    local guards = node[guardproperty]
    for j = 1, #accesses do
      local varname, pos = unpack(accesses)
      if not guards[varname] then
        push(errors, pos)
      end
    end
  end
  return errors
end

local function intersection(lists)
  if #lists == 0 then
    return { }
  else
    local result = { }
    local list = lists[1]
    for j, _ in pairs(list) do
      local r = true
      for i = 2, #lists do
        r = r and lists[i][j]
      end
      result[j] = r
    end
    return result
  end
end

local function assignments(input, node)
  local output = { }
  for var, _ in pairs(input) do
    if not node.assignto[var] then
      output[var] = true
    elseif node.assignfrom[var] then
      output[node.assignfrom[var]] = true
    end
  end
  return output
end

local function trans_rp(input, node)
  local rg = node.rg
  local wg = node.wg
  for i = 1, #rg do
    input[rg[i]] = true
  end
  for i = 1, #wg do
    input[wg[i]] = true
  end
  local r = assignments(input, node)
  return r
end

local function trans_wp(input, node)
  local wg = node.wg
  for i = 1, #wg do
    input[wg[i]] = true
  end
  return assignments(input, node)
end

local function trans_tl(input, node)
  local newbags = node.newbags
  for i = 1, #newbags do
    input[i] = true
  end
  return assignments(input, node)
end

local function standard_changed_from(new, old)
  -- this relies on sets growing monotonically
  if not old then
    return true
  end
  for i, _ in pairs(new) do
    if not old[i] then
      return true
    end
  end
  return false
end

local function join_aa(preds)
  local result = { }
  for i = 1, #preds do
    local pred = preds[i]
    for var, map in pairs(pred) do
      if not result[var] then
        result[var] = clone(map)
      else
        for var2, _ in map do
	  result[var][var2] = true
	end
      end
    end
  end
  return result
end

local function trans_aa(input, node)
  local result = { }
  for var, map in pairs(input) do
    result[var] = assignments(map, node)
  end
  return result
end

local function forward_dataflow(graph, input, output, trans, join, changed_from)
  local nodes = graph.nodes
  local changed
  for i = 1, #nodes do
    nodes[i].changed = true
  end
  repeat
    changed = false
    for i = 1, #nodes do
      local node = nodes[i]
      local node_changed
      if true then
        local preds, newinput, newoutput
	node.changed = false
	preds = { }
	for _, pred in ipairs(node.pred) do
	  push(preds, pred[output])
	end
	newinput = join(preds, node)
        newoutput = trans(clone(newinput), node)
	if changed_from(newinput, node[input]) then
	  node[input] = newinput
	  node_changed = true
	  changed = true
	end
	if changed_from(newoutput, node[output]) then
	  node[output] = newoutput
	  node_changed = true
	  changed = true
	end
	if node_changed then
	  for _, succ in ipairs(node.succ) do
	    succ.changed = true
	  end
	end
      end
    end
  until not changed
end

local function make_unique(list)
  local mem = { }
  local p = 1
  for i = 1, #list, 2 do
    local item = list[i]
    if not mem[item] then
      mem[item] = true
      list[p] = list[i]
      list[p+1] = list[i+1]
      p = p + 2
    end
  end
  -- delete trailing entries
  for i = p, #list do
    list[i] = nil
  end
end

local pass1 = true

local function init_properties(node)
  local ast = node.ast
  node.changed = false
  node.aain = { }
  node.newbags = { }
  node.aaout = { }
  node.tlin = { }
  node.tlout = { }
  node.wpin = { }
  node.rpin = { }
  node.wpout = { }
  node.rpout = { }
  node.wg = { }
  node.rg = { }
  node.writes = { }
  node.reads = { }
  node.assign = { }
  node.assignto = { }
  node.assignfrom = { }
  if ast then
    if pass1 then
      ast:fix_types()
    end
    ast:track_aliases_and_guards(node.assign, node.wg, node.rg)
    for _, pair in ipairs(node.assign) do
      node.assignto[pair[1]] = true
      if pair[2] then
	node.assignfrom[pair[2]] = pair[1]
      end
    end
    ast:bag_access(node.reads, node.writes, node.reads, node.writes, 0)
    make_unique(node.reads)
    make_unique(node.writes)
  end
end

local function dataflow_pass(funcdef)
  local graph = funcdef.graph
  local nodes = graph.nodes
  local start = graph.start
  for i = 1, #nodes do
    local node = nodes[i]
    init_properties(node)
  end
  local aa = { }
  for i = 1, #funcdef.args do
    aa[i] = { }
    aa[i][i] = true
  end
  start.aain = aa
  forward_dataflow(graph, "wpin", "wpout",
    trans_wp, intersection, standard_changed_from)
  forward_dataflow(graph, "rpin", "rpout",
    trans_rp, intersection, standard_changed_from)
  forward_dataflow(graph, "tlin", "tlout",
    trans_tl, intersection, standard_changed_from)
  forward_dataflow(graph, "aain", "aaout",
    trans_aa, join_aa, standard_changed_from)
end

local function add_guards(funcdef)
  local graph = funcdef.graph
  local nodes = graph.nodes
  local start = graph.start
  for i = 1, #nodes do
    local node = nodes[i]
    init_properties(node)
    node.sugg_wg = { }
    node.sugg_rg = { }
  end
  local aa = { }
  for i = 1, #funcdef.args do
    if funcdef.arg_writes[i] then
      push(start.sugg_wg, i)
      push(start.wg, i)
    elseif funcdef.arg_reads[i] then
      push(start.sugg_rg, i)
      push(start.rg, i)
    end
  end

  local errors
  repeat
    errors = false
    forward_dataflow(graph, "wpin", "wpout",
      trans_wp, intersection, standard_changed_from)
    forward_dataflow(graph, "rpin", "rpout",
      trans_rp, intersection, standard_changed_from)
    forward_dataflow(graph, "tlin", "tlout",
      trans_tl, intersection, standard_changed_from)
    for i = 1, #nodes do
      local node = nodes[i]
      for j = 1, #node.writes, 2 do
	local var = node.writes[j]
	if not node.wpin[var] and not node.tlin[var] then
	  push(node.sugg_wg, var)
	  push(node.wg, var)
	  push(node.wpin, var)
	  errors = true
	  break
	end
      end
      if errors then break end
      for j = 1, #node.reads, 2 do
	local var = node.reads[j]
	if not node.rpin[var] and not node.tlin[var] then
	  push(node.sugg_rg, var)
	  push(node.rg, var)
	  push(node.rpin, var)
	  errors = true
	  break
	end
      end
      if errors then break end
    end
  until not errors
end

local function check_argument_accesses(funcdef)
  local graph = funcdef.graph
  local nodes = graph.nodes
  local reads = { }
  local writes = { }
  for i = 1, #nodes do
    local node = nodes[i]
    for arg, map in pairs(node.aain) do
      for j = 1, #node.writes, 2 do
        local var = node.writes[j]
        if map[var] and not node.tlin[var] and not node.wpin[var] then
	  writes[arg] = true
	end
      end
      for j = 1, #node.reads, 2 do
        local var = node.reads[j]
        if map[var] and not node.tlin[var] and not node.rpin[var] then
	  reads[arg] = true
	end
      end
    end
    funcdef.arg_writes = writes
    funcdef.arg_reads = reads
  end
end

local function checkerrors(funcdef)
  local graph = funcdef.graph
  local nodes = graph.nodes
  for i = 1, #nodes do
    local node = nodes[i]
    for j = 1, #node.writes, 2 do
      local var = node.writes[j]
      if not node.wpin[var] and not node.tlin[var] then
        show_error(funcdef.source_file_input, funcdef.source_file_mapping,
	  node.writes[j+1], "Unprotected write of '"..
	  node.local_vars[var][1] .."'")
      end
    end
    for j = 1, #node.reads, 2 do
      local var = node.reads[j]
      if not node.rpin[var] and not node.tlin[var] then
        show_error(funcdef.source_file_input, funcdef.source_file_mapping,
          node.reads[j+1], "Unprotected read of '"..
	  node.local_vars[var][1] .."'")
      end
    end
  end
end

function run_dataflow_analysis()
  for _, funcdef in ipairs(all_functions) do
    dataflow_pass(funcdef)
  end
  pass1 = false
  for i = 2, options.pass_count do
    for _, funcdef in ipairs(all_functions) do
      check_argument_accesses(funcdef)
    end
    for _, funcdef in ipairs(all_functions) do
      dataflow_pass(funcdef)
    end
  end
  if options.report_type == "errors" then
    for _, funcdef in ipairs(entry_points) do
      checkerrors(funcdef)
    end
  elseif options.report_type == "suggestions" then
     for _, funcdef in ipairs(all_functions) do
       add_guards(funcdef)
     end
  else
    system_error("Unknown report type")
  end
end

function dump(funcdef)
  local nodes = funcdef.graph.nodes
  for i = 1, #nodes do
    local node = nodes[i]
    pp({
      index = node.index,
      rpin = node.rpin,
      rpout = node.rpout,
      rg = node.rg,
      reads = node.reads,
      succ = map(node.succ, function(n) return n.index end)
    })
  end
end
