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
