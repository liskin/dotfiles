require'fidget'.setup {
	notification = {
		configs = {
			default = vim.tbl_extend("force", require('fidget.notification').default_config, {
				name = false,
				icon = false,
				ttl = false,
				icon_on_left = true,
			}),
		},
		view = {
			stack_upwards = false,
			line_margin = 0,
		},
		window = {
			border = "rounded",
			border_hl = "Normal",
			winblend = 0,
			x_padding = 0,
			y_padding = 0,
		},
	},
	progress = {
		display = {
			done_ttl = 1,
		},
		ignore = {
			function(msg)
				-- hide annoying flake8 progress updates every few seconds
				return msg.lsp_client.name == "pylsp" and msg.title == "lint: flake8"
			end,
		},
	},
}
