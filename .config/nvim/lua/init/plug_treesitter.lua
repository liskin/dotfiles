---@diagnostic disable: missing-fields
require'nvim-treesitter.configs'.setup {
	sync_install = true,
	ensure_installed = {
		"c",
		"cpp",
		"css",
		"git_config",
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
	textobjects = {
		-- FIXME: doesn't work (https://github.com/nvim-treesitter/nvim-treesitter-textobjects/issues/479)
		-- move = {
		-- 	enable = true,
		-- 	goto_next_start = {
		-- 		["]l"] = "@markup.link.label"
		-- 	},
		-- 	goto_previous_start = {
		-- 		["[l"] = "@markup.link.label"
		-- 	},
		-- },
	},
}
