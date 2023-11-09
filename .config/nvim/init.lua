vim.cmd.source(vim.fn.stdpath("config") .. "/vimrc.vim")

require'nvim-treesitter.configs'.setup {
	sync_install = true,
	ensure_installed = {
		"rust",
	},
	highlight = {
		enable = true,
	},
}

vim.diagnostic.config {
	severity_sort = true,
}
