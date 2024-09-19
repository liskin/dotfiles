local ts = vim.treesitter

local M = {}

function M.parse()
	local ok, parser = pcall(ts.get_parser)
	if ok then
		local syntax_trees = parser:parse(true)
		return parser, syntax_trees[1]:root()
	end
end

local function text_objects(root_parser)
	local objects = {}

	root_parser:for_each_tree(function(tree, parser)
		local query = ts.query.get(parser:lang(), 'textobjects')
		if query then
			for _, match, _metadata in query:iter_matches(tree:root(), 0, nil, nil, { all = true }) do
				for id, nodes in pairs(match) do
					table.insert(objects, {
						capture = query.captures[id],
						nodes = nodes,
					})
				end
			end
		end
	end)

	return objects
end

function M.text_objects()
	local parser = M.parse()
	if parser then
		return text_objects(parser)
	else
		return {}
	end
end

local function for_each_named_child_node(node, fn)
	fn(node)
	for i = 0, node:named_child_count() - 1, 1 do
		for_each_named_child_node(node:named_child(i), fn)
	end
end

local function named_nodes(root_parser)
	local nodes = {}
	root_parser:for_each_tree(function(tree, _parser)
		for_each_named_child_node(tree:root(), function(node)
			table.insert(nodes, node)
		end)
	end)
	return nodes
end

function M.named_nodes()
	local parser = M.parse()
	if parser then
		return named_nodes(parser)
	else
		return {}
	end
end

return M
