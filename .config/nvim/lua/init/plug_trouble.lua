local trouble = require 'trouble'

trouble.setup {
	icons = false,
	padding = false,
	fold_open = "▾",
	fold_closed = "▸",
	signs = {
		error = "E",
		warning = "W",
		hint = "H",
		information = "I",
		other = "",
	},
	action_keys = {
		hover = "<C-K>",
	},
	win_config = {
		border = { '', '', '', '│' },
	},
	auto_preview = false,
}

vim.keymap.set({ 'n', 'i' }, '<Plug>(trouble_toggle)', function()
	if trouble.is_open() then
		trouble.close()
	else
		trouble.open('workspace_diagnostics')
	end
end)
