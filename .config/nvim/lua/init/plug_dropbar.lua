-- XXX: nvim-0.11 only
if vim.fn.has('nvim-0.11') == 0 then
	vim.g.loaded_dropbar = true
	return
end

local dropbar = require'dropbar'
local dropbar_configs = require'dropbar.configs'
local dropbar_utils = require'dropbar.utils'

local enabled = false
local orig_enable = dropbar_configs.opts.bar.enable

local symbols = {
	Class = 'c',
	File = '/',
	Folder = '/',
	Function = 'f',
	MarkdownH1 = '#',
	MarkdownH2 = '##',
	MarkdownH3 = '#3',
	MarkdownH4 = '#4',
	MarkdownH5 = '#5',
	MarkdownH6 = '#6',
	Method = 'f',
	Struct = 's',
	Type = 't',
}

dropbar.setup {
	bar = {
		enable = function(...)
			return enabled and orig_enable(...)
		end,
		padding = {
			left = 0,
			right = 0,
		},
		update_events = {
			buf = {
				'BufModifiedSet',
				'FileChangedShellPost',
				'TextChanged',
				'ModeChanged',
				'LspProgress', -- extra
			},
		},
	},
	icons = {
		enable = false,
		kinds = {
			dir_icon = function(_)
				return symbols.Folder, 'DropBarIconKindFolder'
			end,
			file_icon = function(_)
				return symbols.File, 'DropBarIconKindFile'
			end,
			symbols = {},
		},
		ui = {
			bar = {
				separator = '',
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
	__index = function(_, key)
		local sym = symbols[key]
		if sym then
			return '  ' .. sym .. '·'
		else
			return '  '
		end
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
