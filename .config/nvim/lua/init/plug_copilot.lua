local copilot = require'copilot'

copilot.setup {
	copilot_node_command = 'bwrap-node-copilot', -- restricted jail for the proprietary crap
	suggestion = {
		enabled = false,
	},
	panel = {
		keymap = {
			jump_next = "<M-]>",
			jump_prev = "<M-[>",
			open = "<M-CR>",
		},
	},
	filetypes = {
		["*"] = function()
			if vim.b.lsp_disabled or vim.g.lsp_disabled then
				return false
			end

			local name = vim.api.nvim_buf_get_name(0)
			local basename = vim.fs.basename(name)

			-- disable for .env files
			if string.match(basename, '^%.env.*') then
				return false
			end

			-- enable for everything else, should be okay considering I trigger this manually
			return true
		end,
	},
}
