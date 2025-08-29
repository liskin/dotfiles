local to = require"nvim-treesitter-textobjects"
local to_select = require"nvim-treesitter-textobjects.select"
local to_move = require"nvim-treesitter-textobjects.move"
local to_repeat = require"nvim-treesitter-textobjects.repeatable_move"

to.setup {
	select = {
		lookahead = true,
		include_surrounding_whitespace = true,
		selection_modes = {
			["@function.outer"] = "V", -- linewise
		},
	}
}

-- select
vim.keymap.set({ "x", "o" }, "af", function()
	to_select.select_textobject("@function.outer", "textobjects")
end, { desc = "select @function.outer" })
vim.keymap.set({ "x", "o" }, "if", function()
	to_select.select_textobject("@function.inner", "textobjects")
end, { desc = "select @function.inner" })
vim.keymap.set({ "x", "o" }, "ac", function()
	to_select.select_textobject("@class.outer", "textobjects")
end, { desc = "select @class.outer" })
vim.keymap.set({ "x", "o" }, "ic", function()
	to_select.select_textobject("@class.inner", "textobjects")
end, { desc = "select @class.inner" })

-- move
vim.keymap.set({ "n", "x", "o" }, "]f", function()
	to_move.goto_next_start("@function.outer", "textobjects")
end, { desc = "next @function.outer start" })
vim.keymap.set({ "n", "x", "o" }, "[f", function()
	to_move.goto_previous_start("@function.outer", "textobjects")
end, { desc = "prev @function.outer start" })

-- -- move - repeatable
vim.keymap.set({ "n", "x", "o" }, ";", to_repeat.repeat_last_move, {
	desc = "repeat latest f, t, F, T or treesitter-textobjects move"
})
vim.keymap.set({ "n", "x", "o" }, "|", to_repeat.repeat_last_move_opposite, {
	desc = "repeat latest f, t, F, T or treesitter-textobjects move in opposite direction"
})

-- make builtin f, F, t, T also repeatable
vim.keymap.set({ "n", "x", "o" }, "f", to_repeat.builtin_f_expr, { expr = true })
vim.keymap.set({ "n", "x", "o" }, "F", to_repeat.builtin_F_expr, { expr = true })
vim.keymap.set({ "n", "x", "o" }, "t", to_repeat.builtin_t_expr, { expr = true })
vim.keymap.set({ "n", "x", "o" }, "T", to_repeat.builtin_T_expr, { expr = true })
