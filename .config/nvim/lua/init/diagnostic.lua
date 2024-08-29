vim.keymap.set('n', '<C-K>', vim.diagnostic.open_float)

vim.keymap.set('n', '[d', function () vim.diagnostic.goto_prev({ float = false }) end)
vim.keymap.set('n', ']d', function () vim.diagnostic.goto_next({ float = false }) end)

vim.diagnostic.config {
	severity_sort = true,
	virtual_text = false,
}
