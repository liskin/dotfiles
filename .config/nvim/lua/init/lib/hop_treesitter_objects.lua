local ts_lib = require'init.lib.treesitter'

local M = {}
M.opts = {}

local function text_objects(captures)
	local objects = ts_lib.text_objects()
	local jump_targets = {}

	for _, object in ipairs(objects or {}) do
		if not captures or vim.tbl_contains(captures, object.capture) then
			for _, node in ipairs(object.nodes) do
				local row, col = node:range()
				local jump_target = {
					window = 0,
					buffer = 0,
					cursor = {
						row = row + 1,
						col = col,
					},
					length = 0,
				}
				table.insert(jump_targets, jump_target)
				-- FIXME: filter out duplicates and invisible targets
			end
		end
	end

	return {
		jump_targets = jump_targets,
	}
end

function M.text_objects(captures, opts)
	local hop = require'hop'

	opts = setmetatable(opts or {}, { __index = M.opts })
	hop.hint_with(function(_opts)
		return text_objects(captures)
	end, opts)
end

function M.register(opts)
	M.opts = opts
	vim.api.nvim_create_user_command('HopTextObjects', function()
		M.text_objects()
	end, {})
end

return M
