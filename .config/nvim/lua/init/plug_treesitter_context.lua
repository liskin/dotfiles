local tc = require"treesitter-context"

tc.setup {
	enable = true,
}

vim.keymap.set("n", "[c", function()
	tc.go_to_context(vim.v.count1)
end, { silent = true })
