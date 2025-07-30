require'fidget'.setup {
	notification = {
		configs = {
			default = {
				icon_on_left = true,
			},
		},
		view = {
			stack_upwards = false,
		},
		window = {
			border = "rounded",
			border_hl = "Normal",
			winblend = 0,
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
