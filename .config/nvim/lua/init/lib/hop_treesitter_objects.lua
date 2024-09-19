local ts_lib = require'init.lib.treesitter'

local M = {}
M.opts = {}

local function text_objects(captures)
	local jump_targets = {}
	local seen = {}

	for _, object in ipairs(ts_lib.text_objects() or {}) do
		if not captures or vim.tbl_contains(captures, object.capture) then
			for _, node in ipairs(object.nodes) do
				local row, col = node:range()
				row = row + 1

				local seen_key = row .. ':' .. col
				if not seen[seen_key] then
					seen[seen_key] = true

					table.insert(jump_targets, {
						window = 0,
						buffer = 0,
						cursor = {
							row = row,
							col = col,
						},
						length = 0,
					})
				end
			end
		end
	end

	return { jump_targets = jump_targets }
end

local function sort_indirect_jump_targets(locations, opts)
	local indirect_jump_targets = {}
	local c_row, c_col = unpack(vim.api.nvim_win_get_cursor(0))
	local cursor = { row = c_row, col = c_col }
	for i, jump_target in ipairs(locations.jump_targets) do
		table.insert(indirect_jump_targets, {
			index = i,
			score = opts.distance_method(cursor, jump_target.cursor, opts.x_bias),
		})
	end
	require'hop.jump_target'.sort_indirect_jump_targets(indirect_jump_targets, opts)

	locations.indirect_jump_targets = indirect_jump_targets
end

function M.text_objects(captures, opts)
	opts = setmetatable(opts or {}, { __index = M.opts })
	require'hop'.hint_with(function()
		local locations = text_objects(captures)
		sort_indirect_jump_targets(locations, opts)
		return locations
	end, opts)
end

function M.register(opts)
	M.opts = opts
	vim.api.nvim_create_user_command('HopTextObjects', function(info)
		M.text_objects(#info.fargs > 0 and info.fargs)
	end, {
		nargs = '*',
	})
end

return M
