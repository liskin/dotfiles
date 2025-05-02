---@diagnostic disable: missing-fields
require'nvim-treesitter.configs'.setup {
	sync_install = true,
	ensure_installed = {
		"bash",
		"c",
		"comment",
		"cpp",
		"css",
		"diff",
		"dockerfile",
		"git_config",
		"go",
		"graphql",
		"html",
		"javascript",
		"jsdoc",
		"json",
		"json5",
		"jsonc",
		"lua",
		"markdown",
		"markdown_inline",
		"mermaid",
		"python",
		"query",
		"regex",
		"rust",
		"sql",
		"vim",
		"vimdoc",
		"yaml",
	},
	highlight = {
		enable = true,
		disable = {
			"bash", -- too buggy (but stays enabled in injections)
		},
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
