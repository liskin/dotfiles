vim.cmd.source(vim.fn.stdpath("config") .. "/vimrc.vim")

require'nvim-treesitter.configs'.setup {
	ensure_installed = {
		"rust",
	},
	highlight = {
		enable = true,
	},
}
