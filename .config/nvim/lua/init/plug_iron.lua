require'iron.core'.setup {
	config = {
		repl_open_cmd = require("iron.view").split.botright("40%"),
		repl_definition = {
			python = {
				command = { "ipython3", "--no-autoindent" },
				format = require("iron.fts.common").bracketed_paste,
			}
		},
	},
	keymaps = {
		send_motion = "<Leader>sc",
		visual_send = "<Leader>sc",
		send_line = "<Leader>scc",
		cr = "<Leader>s<cr>",
		interrupt = "<Leader>s<space>",
		exit = "<Leader>sq",
	},
}
