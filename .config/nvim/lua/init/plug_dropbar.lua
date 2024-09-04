local dropbar = require'dropbar'
local dropbar_configs = require'dropbar.configs'
local dropbar_utils = require'dropbar.utils'

local enabled = false

dropbar.setup {
	general = {
		-- enable = false,
		enable = function(buf, win, _)
			return enabled
				and vim.api.nvim_buf_is_valid(buf)
				and vim.api.nvim_win_is_valid(win)
				and vim.wo[win].winbar == ''
				and vim.fn.win_gettype(win) == ''
				and vim.bo[buf].ft ~= 'help'
				and ((pcall(vim.treesitter.get_parser, buf)) and true or false)
		end,
	},
	icons = {
		enable = false,
		kinds = {
			symbols = {
				-- Function = 'f',
			},
		},
		ui = {
			bar = {
				separator = ' › ', -- »
				extends = '…',
			},
			menu = {
				separator = ' ',
				indicator = '/',
			},
		},
	},
	menu = {
		preview = false,
	}
}

setmetatable(dropbar_configs.opts.icons.kinds.symbols, {
	__index = function()
		return ''
	end,
})

vim.api.nvim_create_user_command('DropbarToggle', function()
	enabled = not enabled
	for _, win in ipairs(vim.api.nvim_list_wins()) do
		if enabled then
			dropbar_utils.bar.attach(vim.api.nvim_win_get_buf(win), win)
		else
			vim.wo[win].winbar = ''
		end
	end
end, {})
