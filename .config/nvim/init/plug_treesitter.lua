require'nvim-treesitter.configs'.setup {
	sync_install = true,
	ensure_installed = {
		"c",
		"lua",
		"query",
		"rust",
		"vim",
		"vimdoc",
	},
	highlight = {
		enable = true,
	},
}
