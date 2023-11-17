---@diagnostic disable: missing-fields
require'nvim-treesitter.configs'.setup {
	sync_install = true,
	ensure_installed = {
		"c",
		"cpp",
		"go",
		"html",
		"lua",
		"markdown",
		"markdown_inline",
		"python",
		"query",
		"regex",
		"rust",
		"vim",
		"vimdoc",
	},
	highlight = {
		enable = true,
	},
}
