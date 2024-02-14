---@diagnostic disable: missing-fields
require'nvim-treesitter.configs'.setup {
	sync_install = true,
	ensure_installed = {
		"c",
		"cpp",
		"css",
		"go",
		"html",
		"javascript",
		"lua",
		"markdown",
		"markdown_inline",
		"python",
		"query",
		"regex",
		"rust",
		"vim",
		"vimdoc",
		"yaml",
	},
	highlight = {
		enable = true,
	},
}
