return {
	'vimwiki',
	event = {
		event = "BufReadPre",
		pattern = vim.env.HOME .. "/taskwiki/*",
	},
}
